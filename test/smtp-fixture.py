"""Run a command against disposable loopback-only TLS SMTP servers.
No delivery is performed. Credentials and recipients are synthetic test data.
"""
import base64, json, os, pathlib, socketserver, ssl, subprocess, sys, threading, time
base = pathlib.Path(sys.argv[1]).resolve()
base.mkdir(parents=True, exist_ok=True)
cert, key = base / 'smtp-cert.pem', base / 'smtp-key.pem'
subprocess.run(['openssl', 'req', '-x509', '-newkey', 'rsa:2048', '-nodes', '-sha256',
                '-subj', '/CN=localhost', '-addext', 'subjectAltName=DNS:localhost',
                '-days', '1', '-keyout', str(key), '-out', str(cert)],
               check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
key.chmod(0o600)
(base / 'smtp-password').write_text('fixture-password\n')
(base / 'smtp-password').chmod(0o600)
wrong_cert, wrong_key = base / 'smtp-wrong-cert.pem', base / 'smtp-wrong-key.pem'
subprocess.run(['openssl', 'req', '-x509', '-newkey', 'rsa:2048', '-nodes', '-sha256',
                '-subj', '/CN=wrong.invalid', '-addext', 'subjectAltName=DNS:wrong.invalid',
                '-days', '1', '-keyout', str(wrong_key), '-out', str(wrong_cert)],
               check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
wrong_key.chmod(0o600)
wrong_ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
wrong_ctx.load_cert_chain(wrong_cert, wrong_key)
ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
ctx.load_cert_chain(cert, key)
class Server(socketserver.ThreadingTCPServer):
    daemon_threads = True
    allow_reuse_address = True
class Handler(socketserver.StreamRequestHandler):
    def setup(self):
        self.request.settimeout(10)
        self.request = self.server.ctx.wrap_socket(self.request, server_side=True)
        super().setup()
    def handle(self):
        try:
            self.session()
        except (OSError, ValueError):
            pass
    def reply(self, data):
        self.wfile.write(data + b'\r\n'); self.wfile.flush()
    def session(self):
        mode = self.server.mode
        if mode == 'slow':
            time.sleep(3)
        self.reply(b'220 localhost fixture')
        sender, recipients, username = '', [], ''
        while line := self.rfile.readline(8192):
            command = line.decode().strip()
            if command.startswith(('EHLO ', 'HELO ')):
                self.reply(b'250-localhost\r\n250 AUTH PLAIN')
            elif command.startswith('AUTH PLAIN'):
                fields = command.split()
                if len(fields) == 2:
                    self.reply(b'334 ')
                    token = self.rfile.readline(8192).strip()
                else:
                    token = fields[2]
                _, user, password = base64.b64decode(token).split(b'\0')
                if password != b'fixture-password':
                    self.reply(b'535 rejected'); continue
                username = user.decode()
                self.reply(b'235 authenticated')
            elif command.startswith('MAIL FROM:'):
                sender = command[10:].strip(); self.reply(b'250 sender')
            elif command.startswith('RCPT TO:'):
                recipients.append(command[8:].strip()); self.reply(b'250 recipient')
            elif command == 'DATA':
                self.reply(b'354 data')
                data = bytearray()
                while line := self.rfile.readline(8192):
                    if line == b'.\r\n': break
                    if line.startswith(b'..'): line = line[1:]
                    data.extend(line)
                    if len(data) > 4*1024*1024: raise ValueError('too much data')
                if mode == 'fail':
                    p = base / 'failed-send-count'
                    p.write_text(str(int(p.read_text())+1) if p.exists() else '1')
                    return  # Accepted bytes, then lost connection: uncertain result.
                (base / 'sent.eml').write_bytes(data)
                (base / 'sent-envelope.json').write_text(json.dumps(dict(sender=sender, recipients=recipients, username=username)))
                self.reply(b'250 stored locally')
            elif command == 'QUIT':
                self.reply(b'221 bye'); return
            else:
                self.reply(b'500 unsupported')
    def finish(self):
        try: super().finish()
        except OSError: pass
class QuietServer(Server):
    def handle_error(self, request, client_address): pass
servers = []
env = dict(os.environ)
for mode, name in [('ok','SMTP_TEST_PORT'), ('fail','SMTP_FAIL_PORT'), ('slow','SMTP_SLOW_PORT'), ('wrong-host','SMTP_WRONG_HOST_PORT')]:
    server = QuietServer(('127.0.0.1', 0), Handler)
    server.mode = mode
    server.ctx = wrong_ctx if mode == 'wrong-host' else ctx
    servers.append(server)
    env[name] = str(server.server_address[1])
    threading.Thread(target=server.serve_forever, daemon=True).start()
try:
    result = subprocess.run(sys.argv[2:], env=env)
finally:
    for server in servers: server.shutdown(); server.server_close()
sys.exit(result.returncode)
