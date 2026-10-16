"""Synthetic Discourse API on loopback; mutations never reach a real forum."""
import http.server
import json
import os
import subprocess
import sys
import threading
import time
import urllib.parse

class Handler(http.server.BaseHTTPRequestHandler):
    offline = False
    revision = 3
    liked = False
    level = 1
    writes = 0
    delay_list = False
    list_started = False
    def log_message(self, *args): pass
    def respond(self, status, data):
        self.send_response(status)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        try: self.wfile.write(data if isinstance(data, bytes) else json.dumps(data).encode())
        except (BrokenPipeError, ConnectionResetError): pass
    def topic(self, topic=7):
        return dict(id=topic, title='Discourse topic %d' % topic, category_id=3,
                    posts_count=self.revision, highest_post_number=self.revision,
                    last_poster_username='alice', created_at='2026-10-16T00:00:00Z',
                    last_posted_at='2026-10-16T00:0%d:00Z' % self.revision)
    def post(self, floor):
        return dict(id=100 + floor, topic_id=7, post_number=floor,
                    reply_to_post_number=(None if floor == 1 else floor-1),
                    username='alice', created_at='2026-10-16T00:0%d:00Z' % floor,
                    updated_at='2026-10-16T00:0%d:00Z' % floor,
                    cooked='<p>Floor %d body</p><a class="attachment" href="/uploads/file.bin">file.bin</a>' % floor,
                    actions_summary=[dict(id=2, count=2 if self.liked else 1, acted=self.liked)])
    def do_GET(self):
        parsed = urllib.parse.urlsplit(self.path)
        path = parsed.path
        q = urllib.parse.parse_qs(parsed.query)
        if path.startswith('/control/'):
            mode=path.rsplit('/',1)[1]
            Handler.offline=mode=='offline'
            if mode=='delay-list': Handler.delay_list=True; Handler.list_started=False
            if mode=='reset':
                Handler.revision=3; Handler.liked=False; Handler.level=1; Handler.writes=0; Handler.delay_list=False; Handler.list_started=False
            if mode=='update': Handler.revision=4
            return self.respond(200, dict(writes=Handler.writes, list_started=Handler.list_started))
        if Handler.offline: return self.respond(503, {})
        if path == '/uploads/file.bin': return self.respond(200, b'Discourse\x00attachment\xff')
        if path == '/categories.json':
            return self.respond(200, dict(category_list=dict(categories=[dict(id=3, slug='help', name='Help')])))
        if path == '/c/3/show.json': return self.respond(200, dict(category=dict(id=3, slug='help')))
        if path in ('/latest.json', '/c/help/3.json'):
            if 'state' in q:
                if self.headers.get('User-Api-Key')!='fixturekey': return self.respond(403,{})
                expected={'watching':3,'tracking':2}[q['state'][0]]
                topics=[self.topic()] if self.level==expected else []
                if q.get('search')==['missing']: topics=[]
                return self.respond(200,dict(topic_list=dict(topics=topics)))
            page=int(q.get('page',['0'])[0])
            topics=[self.topic()] if page==0 else [self.topic(8)]
            if Handler.delay_list:
                Handler.delay_list=False
                Handler.list_started=True
                time.sleep(0.7)
            return self.respond(200, dict(topic_list=dict(topics=topics, more_topics_url='/latest?page=1' if page==0 else None)))
        if path == '/search.json':
            query=q.get('q',[''])[0]
            if 'category:3' not in query: return self.respond(422, dict(errors=['Missing category scope']))
            topics=[] if 'missing' in query else [self.topic()]
            return self.respond(200, dict(topics=topics, posts=[dict(topic_id=7, post_number=2)] if topics else [],
                                         grouped_search_result=dict(more_full_page_results=False)))
        if path == '/t/99.json': time.sleep(5)
        if path == '/t/7/posts.json':
            posts=[self.post(int(i)-100) for i in q.get('post_ids[]',[])]
            return self.respond(200, dict(post_stream=dict(posts=list(reversed(posts)))))
        if path.startswith('/t/7'):
            parts=path.removesuffix('.json').split('/')
            floor=int(parts[3]) if len(parts)>3 else 1
            return self.respond(200, dict(self.topic(), post_stream=dict(posts=[self.post(floor)], stream=list(range(101,101+self.revision))),
                                         details=dict(notification_level=self.level)))
        return self.respond(404, {})
    def mutation(self, delete=False):
        if self.headers.get('User-Api-Key')!='fixturekey' or self.headers.get('User-Api-Client-Id')!='fixtureclient':
            return self.respond(403,{})
        length=int(self.headers.get('Content-Length','0'))
        q=urllib.parse.parse_qs(self.rfile.read(length).decode())
        Handler.writes+=1
        if self.path.startswith('/post_actions'):
            Handler.liked=not delete
            return self.respond(200,self.post(1))
        if self.path=='/posts.json':
            floor=Handler.revision+1
            Handler.revision=floor
            p=self.post(floor)
            p['reply_to_post_number']=int(q.get('reply_to_post_number',['1'])[0])
            return self.respond(200,p)
        if self.path=='/t/7/notifications.json':
            Handler.level=int(q['notification_level'][0])
            return self.respond(200,dict(success='OK'))
        return self.respond(404,{})
    def do_POST(self): self.mutation()
    def do_DELETE(self): self.mutation(True)

server=http.server.ThreadingHTTPServer(('127.0.0.1',0),Handler)
threading.Thread(target=server.serve_forever,daemon=True).start()
env=dict(os.environ,DISCOURSE_FIXTURE='http://127.0.0.1:%d'%server.server_port)
try: result=subprocess.run(sys.argv[1:],env=env)
finally: server.shutdown(); server.server_close()
sys.exit(result.returncode)
