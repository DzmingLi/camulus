"""Local phpBB fixture. No external requests or real forum writes."""
import http.server
import os
import subprocess
import sys
import threading
from urllib.parse import urlsplit, parse_qs

class Handler(http.server.BaseHTTPRequestHandler):
    posts = 0
    def log_message(self, *args): pass
    def reply(self, status, body, headers=()):
        self.send_response(status)
        for k, v in headers: self.send_header(k, v)
        self.end_headers()
        self.wfile.write(body.encode() if isinstance(body, str) else body)
    def do_GET(self):
        u = urlsplit(self.path); q = parse_qs(u.query)
        if u.path == '/control/count': return self.reply(200, str(Handler.posts))
        if u.path == '/forum/feed.php':
            return self.reply(200, """<feed xmlns='http://www.w3.org/2005/Atom'><entry><id>https://cs.rin.ru/forum/viewtopic.php?t=12&amp;p=101</id><title>Topic title</title><author><name>Alice</name></author><published>2026-10-04T02:14:30+01:00</published><content type='html'>&lt;p&gt;Recent reply&lt;/p&gt;</content></entry></feed>""")
        if u.path == '/forum/viewtopic.php':
            first = q.get('start', ['0']) == ['0']; post = 100 if first else 101
            return self.reply(200, f"""<html><a class='titles'>Topic title</a><table class='tablebg'><tr><td><a name='p{post}'></a><b class='postauthor'>Alice</b><a href='./viewtopic.php?f=14&amp;t=12'>Post</a><div>Posted: Sunday, 04 Oct 2026, 02:14</div></td></tr><tr><td><div class='postbody'><p>Full post {post}</p><img src='./image.png'><a href='./download/file.php?id=7'>file.bin</a></div></td></tr></table>{"<a href='./viewtopic.php?t=12&amp;start=15'>Next</a>" if first else ''}</html>""")
        if u.path == '/forum/image.png': return self.reply(200, b'IMAGE\0')
        if u.path == '/forum/download/file.php': return self.reply(200, b'ATTACHMENT\0\xff')
        if u.path == '/forum/ucp.php':
            return self.reply(200, "<form action='./ucp.php?mode=login'><input name='password'><input type='hidden' name='sid' value='fresh-login'></form>")
        if u.path == '/forum/posting.php':
            return self.reply(200, "<form action='./posting.php?mode=reply&amp;f=14&amp;t=12'><input type='hidden' name='form_token' value='fresh'><input type='hidden' name='creation_time' value='123'><textarea name='message'></textarea></form>")
        self.reply(404, 'Not found')
    def do_POST(self):
        data = parse_qs(self.rfile.read(int(self.headers.get('Content-Length', 0))).decode())
        if urlsplit(self.path).path == '/forum/ucp.php':
            if data.get('sid') != ['fresh-login'] or data.get('password') != ['fixture-password']:
                return self.reply(403, 'Invalid login')
            return self.reply(200, "<a href='ucp.php?mode=logout'>Logout</a>", [('Set-Cookie', 'session=ok; Path=/forum/; HttpOnly')])
        if urlsplit(self.path).path == '/forum/posting.php':
            Handler.posts += 1
            if data.get('form_token') != ['fresh'] or 'session=ok' not in self.headers.get('Cookie', ''):
                return self.reply(403, 'Login or CSRF missing')
            if 'uncertain' in data.get('message', [''])[0]: return self.reply(200, 'Unconfirmed response')
            return self.reply(200, "<meta http-equiv='refresh' content='3;url=./viewtopic.php?t=12&amp;p=102#p102'>")
        self.reply(404, 'Not found')

server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)
threading.Thread(target=server.serve_forever, daemon=True).start()
try:
    result = subprocess.run(sys.argv[1:], env=dict(os.environ, CSRIN_FIXTURE=f'http://127.0.0.1:{server.server_port}'))
finally:
    server.shutdown(); server.server_close()
sys.exit(result.returncode)
