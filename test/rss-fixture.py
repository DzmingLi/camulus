"""Loopback RSS/Atom server. Never contacts external services."""
import json
import http.server
import os
import subprocess
import sys
import threading
import time

class Handler(http.server.BaseHTTPRequestHandler):
    offline = False
    revision = 1
    def log_message(self, *args): pass
    def do_GET(self):
        if self.path.startswith('/control/'):
            Handler.offline = self.path == '/control/offline'
            Handler.revision = 2 if self.path == '/control/update' else 1
            return self.reply(200, b'OK')
        if Handler.offline:
            return self.reply(503, b'offline')
        if self.path == '/missing':
            self.send_error(404); return
        if self.path == '/slow':
            time.sleep(5)
        if self.path == '/file.bin':
            return self.reply(200, b'RSS\x00attachment\xff\n')
        if self.path == '/redirect':
            self.send_response(302); self.send_header('Location', '/rss'); self.end_headers(); return
        tag = '"v%d"' % Handler.revision
        if self.headers.get('If-None-Match') == tag:
            return self.reply(304, b'')
        if self.path in ('/json', '/json-page2'):
            number = 1 if self.path == '/json' else 2
            body = json.dumps({'version': 'https://jsonfeed.org/version/1.1', 'title': 'JSON fixture',
                'next_url': '/json-page2' if number == 1 else '',
                'items': [{'id': 'json:%d' % number, 'title': 'JSON item %d' % number,
                    'content_html': '<p>JSON body version %d</p>' % Handler.revision,
                    'date_published': '2026-10-16T02:30:00Z',
                    'attachments': [{'url': '/file.bin', 'mime_type': 'application/octet-stream'}]}]})
        elif self.path == '/atom':
            body = '''<feed xmlns="http://www.w3.org/2005/Atom"><title>Atom</title><entry><id>atom:one</id><title>Atom entry</title><updated>2026-10-16T02:30:00Z</updated><content type="text">Atom plain &lt;b&gt;text&lt;/b&gt;</content><link href="/original"/><link rel="enclosure" href="/file.bin" length="16" type="application/octet-stream"/></entry></feed>'''
        else:
            body = '''<rss version="2.0"><channel><title>RSS fixture</title><item><guid isPermaLink="false">rss:one</guid><title>RSS entry</title><link>/original</link><description><![CDATA[<p>RSS body version %d</p>]]></description><pubDate>Fri, 16 Oct 2026 10:30:00 +0800</pubDate><enclosure url="/file.bin" length="16" type="application/octet-stream"/></item></channel></rss>''' % Handler.revision
        self.send_response(200)
        self.send_header('ETag', tag)
        self.send_header('Content-Type', 'application/xml; charset=utf-8')
        self.end_headers()
        try: self.wfile.write(body.encode())
        except (BrokenPipeError, ConnectionResetError): pass
    def reply(self, status, body):
        self.send_response(status); self.end_headers()
        try: self.wfile.write(body)
        except (BrokenPipeError, ConnectionResetError): pass

server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)
threading.Thread(target=server.serve_forever, daemon=True).start()
env = dict(os.environ, RSS_FIXTURE='http://127.0.0.1:%d' % server.server_port)
try:
    result = subprocess.run(sys.argv[1:], env=env)
finally:
    server.shutdown(); server.server_close()
sys.exit(result.returncode)
