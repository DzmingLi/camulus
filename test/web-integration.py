"""Native HTTP workflow with isolated state and loopback feed fixtures."""
import http.server
import json
import os
from pathlib import Path
import socket
import subprocess
import sys
import tempfile
import threading
import time
import urllib.error
import urllib.request

class Feed(http.server.BaseHTTPRequestHandler):
    revision = 1
    offline = False
    def log_message(self, *_): pass
    def do_GET(self):
        if self.offline:
            self.send_error(503); return
        base = 'http://127.0.0.1:%d' % self.server.server_port
        html = '<p>Body %d</p><video controls src="%s/movie.mp4"></video><img src="%s/image.png" onerror="alert(1)"><svg onload="alert(1)"></svg><script>alert(1)</script><a href="javascript:alert(1)">unsafe</a>' % (self.revision, base, base)
        if self.path == '/json':
            data = json.dumps({'version': 'https://jsonfeed.org/version/1.1', 'title': 'JSON', 'items': [
                {'id': 'json-one', 'title': 'JSON one', 'content_html': html, 'date_published': '2026-11-07T03:00:00Z'}]})
        else:
            data = '<rss version="2.0"><channel><title>Test</title><item><guid isPermaLink="false">rss-one</guid><title>RSS one</title><link>%s/article</link><pubDate>Sat, 07 Nov 2026 03:00:00 GMT</pubDate><description><![CDATA[%s]]></description></item></channel></rss>' % (base, html)
        self.send_response(200); self.end_headers(); self.wfile.write(data.encode())

feed = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Feed)
threading.Thread(target=feed.serve_forever, daemon=True).start()
feed_url = 'http://127.0.0.1:%d' % feed.server_port
with socket.socket() as probe:
    probe.bind(('127.0.0.1', 0)); port = probe.getsockname()[1]
base = 'http://127.0.0.1:%d' % port

def api(path, data=None, method=None, headers=None, expected=200):
    headers = {'Content-Type': 'application/json', 'X-Camulus-Request': '1'} if headers is None else headers
    req = urllib.request.Request(base + path, None if data is None else json.dumps(data).encode(), headers,
                                 method=method or ('POST' if data is not None else 'GET'))
    try:
        response = urllib.request.build_opener(urllib.request.ProxyHandler({})).open(req, timeout=10)
    except urllib.error.HTTPError as error:
        response = error
    assert response.status == expected, (path, response.status, response.read().decode())
    return json.load(response)

def job(data):
    started = api('/api/jobs', data, expected=202)
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        result = api('/api/jobs/' + started['id'])
        if result['done']:
            assert result['error'] is None, result
            return result['events']
        time.sleep(.02)
    raise AssertionError('job timed out')

def state(op, **fields):
    return job({'kind': 'state', 'request': {'op': op, **fields}})

def rows(events, kind='feed'):
    return next(e['rows'] for e in events if e['kind'] == kind)

def timeline():
    return rows(api('/api/timeline')['events'])

with tempfile.TemporaryDirectory(prefix='camulus-web-test-') as directory:
    process = None
    def start():
        global process
        process = subprocess.Popen([sys.argv[1], '--port', str(port), '--config', directory+'/settings.json',
                                    '--state-dir', directory+'/state', '--assets', sys.argv[2]],
                                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
        for _ in range(100):
            if process.poll() is not None: raise AssertionError(process.stderr.read().decode())
            try: return api('/api/bootstrap')
            except OSError: time.sleep(.03)
        raise AssertionError('server did not start')
    try:
        initial = start()
        rss = 'rss:'+feed_url+'/rss'; js = 'jsonfeed:'+feed_url+'/json'
        settings = initial['settings']
        settings['groups'] = [{'name':'RSS', 'source':rss, 'group':feed_url+'/rss'},
                              {'name':'JSON', 'source':js, 'group':feed_url+'/json'}]
        settings['priorities'] = [{'source':rss,'group':feed_url+'/rss','level':1},
                                  {'source':js,'group':feed_url+'/json','level':3}]
        settings['home_priorities'] = [1]
        api('/api/settings', {'revision':initial['revision'], 'settings':settings}, method='PUT')
        assert api('/api/bootstrap')['subscriptions'] == []
        assert (Path(directory)/'settings.json').stat().st_mode & 0o777 == 0o600
        api('/api/settings', {'revision':initial['revision'], 'settings':settings}, method='PUT', expected=409)
        api('/api/jobs', {}, headers={'Origin':'https://attacker.invalid','X-Camulus-Request':'1'}, expected=400)
        api('/api/jobs', {}, headers={}, expected=400)
        api('/api/bootstrap', headers={'Host':'attacker.invalid'}, expected=400)
        state('refresh', source=rss, group=feed_url+'/rss')
        assert timeline() == [], 'browsing must not subscribe'
        for source, group in [(rss,feed_url+'/rss'),(js,feed_url+'/json')]:
            job({'kind':'subscription','source':source,'target':group,'setting':'subscribed'})
            state('refresh', source=source, group=group)
        assert len(api('/api/bootstrap')['subscriptions']) == 2
        assert [r['source'] for r in timeline()] == [rss], 'priority is a display filter'
        event = next(e for e in job({'kind':'read','source':rss,'id':'rss-one'}) if e['kind']=='article')
        html = event['message']['html']
        assert '<video' in html and 'controls' in html
        assert all(bad not in html for bad in ['<script','<svg','onerror','javascript:'])
        state('read', source=rss, ids=['rss-one'], value=True)
        assert timeline()[0]['unread'] is False
        state('favorite', source=rss, id='rss-one', target='article', folder='Research/OCaml')
        assert timeline()[0]['favorite'] is True
        Feed.offline = True
        archived = state('archive', source=rss, id='rss-one', target='article')
        assert 'Body 1' in next(e for e in archived if e['kind']=='archive')['articles'][0]['message']['html']
        assert rows(state('favorites', folder='Research/OCaml'), 'favorites')[0]['id']=='rss-one'
        api('/api/jobs', {'kind':'state','request':{'op':'attachment','source':rss,'id':'rss-one','part':0,'destination':'/tmp/not-allowed'}}, expected=400)
        Feed.offline = False
        current = timeline()[0]
        state('dismiss-timeline', source=rss, id=current['id'], revision=current['revision'])
        state('refresh', source=rss, group=feed_url+'/rss')
        assert timeline() == [], 'unchanged refresh must not revive dismissed content'
        assert len(rows(state('feed', source=rss, group=feed_url+'/rss'))) == 1
        process.terminate(); process.wait(timeout=5)
        resumed = start()
        assert len(resumed['subscriptions']) == 2
        assert timeline() == []
        assert rows(state('favorites', folder='Research/OCaml'), 'favorites')
        state('remove-folder', folder='Research')
        assert not rows(state('favorites', folder='Research/OCaml'), 'favorites')
        print('PASS: settings, subscription authority, priority, read, archive, dismissal, restart, origin isolation, sanitized video')
    finally:
        if process and process.poll() is None:
            process.terminate(); process.wait(timeout=5)
        feed.shutdown()
