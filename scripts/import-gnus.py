#!/usr/bin/env python3
"""Prepare declarative entries from Gnus metadata. Never evaluate the old config.
Writes a staging config/report; activation and existing-config merge are separate.
"""
import argparse
import json
import os
import re
from pathlib import Path
import subprocess
from urllib.parse import urlsplit


def convert(inventory, mail_root, reddit_user):
    entries, pending, skipped = [], [], []
    feeds = {r['group']: r['url'] for r in inventory['rss']}
    for old in inventory['groups']:
        level = old.get('level')
        backend = old['backend']
        if not isinstance(level, int) or level > 5 or level < 1:
            skipped.append(dict(old, reason='Gnus group is not subscribed'))
            continue
        full = old['group']
        group = full.split(':', 1)[1] if ':' in full else full
        source, mapped, reason = None, None, None
        if backend == 'nnjanestreet' and group == 'posts':
            source, mapped = 'janestreet', 'posts'
        elif backend in ('nntheatlantic', 'nnatlantic') and group.startswith('author.'):
            source, mapped = 'atlantic', group
        elif backend == 'nnrss':
            url = feeds.get(group)
            if url:
                source, mapped = 'rss:' + url, url
            else:
                reason = 'RSS URL absent from saved nnrss-group-alist'
        elif backend == 'nnatom':
            url = old.get('server') or ''
            if url and not urlsplit(url).scheme:
                url = 'https://' + url
            if url:
                source, mapped = 'rss:' + url, url
        elif backend == 'nnblogger' and group.startswith('posts.'):
            host = group.removeprefix('posts.')
            if re.fullmatch(r'[A-Za-z0-9](?:[A-Za-z0-9.-]*[A-Za-z0-9])?\.[A-Za-z]{2,}', host):
                source, mapped = 'blogger', 'posts.' + host.lower()
        elif backend == 'nndiscourse' and old.get('address'):
            source = 'discourse:' + old['address'].rstrip('/')
            if group == 'notifications':
                mapped = 'replies'  # Old nndiscourse selects notification_type=2.
            elif group in ('latest', 'watching', 'tracking'):
                mapped = group
            elif group.startswith(('topic.', 'category.')):
                mapped = group.replace('.', ':', 1)
        elif backend == 'nnzhihu' and re.fullmatch(r'(?:articles\.(?:people|org)|answers)\.[A-Za-z0-9_.-]+', group):
            source, mapped = 'zhihu', group
        elif backend == 'nndouban' and re.fullmatch(r'(?:timeline|replies|topic|group-topic)\.[0-9]+', group):
            source, mapped = 'douban', group
        elif backend == 'nnneteasemusic' and re.fullmatch(r'events\.[0-9]+', group):
            source, mapped = 'netease', group
        elif backend == 'nnpublicinbox' and old.get('server', '').startswith('https://'):
            source, mapped = 'public-inbox', old['server'].rstrip('/') + '/' + group + '/'
        elif backend == 'nngcores' and re.fullmatch(r'talks\.[1-9][0-9]*', group):
            source, mapped = 'gcores', group
        elif backend == 'nnreddit' and reddit_user:
            source = 'reddit:' + reddit_user
            mapped = group.removeprefix('r/')
        elif backend == 'nnimap' and old.get('server') in ('dzming-li', 'news-letter'):
            source = 'maildir:' + str(mail_root)
            mapped = '/' + old['server'] + '/' + ('Inbox' if group.upper() == 'INBOX' else group)
        elif backend in ('nnvirtual', 'nndraft'):
            skipped.append(dict(old, reason='Virtual view or draft store, not a remote content entry'))
            continue
        if source and mapped:
            entries.append(dict(name=old.get('comment') or full, source=source, group=mapped,
                                gnus_group=full, gnus_level=level, timeline=not source.startswith('rss:')))
        else:
            pending.append(dict(old, reason=reason or 'Backend or group capability not implemented'))
    entries.sort(key=lambda e: e['gnus_level'])
    # Identity deduplication preserves the first human-readable Gnus name.
    unique = list({(e['source'], e['group']): e for e in reversed(entries)}.values())[::-1]
    used = set()
    for e in unique:
        base, index = e['name'], 1
        while e['name'] in used:
            index += 1
            e['name'] = f'{base} ({index})'
        used.add(e['name'])
    config = dict(version=1, home=None, entries=unique)
    report = dict(migrated=unique, pending=pending, skipped=skipped,
                  note='Subscription/entry definitions only. Gnus numeric read marks, bodies and drafts are not imported.')
    return config, report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--newsrc', type=Path, default=Path.home()/'.newsrc.eld')
    parser.add_argument('--news', type=Path, default=Path.home()/'News')
    parser.add_argument('--mail-root', type=Path, default=Path.home()/'Maildir')
    parser.add_argument('--reddit-user')
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    env = dict(os.environ, EF_GNUS_INVENTORY='1', EF_GNUS_NEWSRC=str(args.newsrc), EF_GNUS_NEWS=str(args.news))
    script = Path(__file__).with_name('gnus-inventory.el')
    result = subprocess.run(['emacs', '-Q', '--batch', '-l', str(script)], env=env,
                            check=True, capture_output=True, text=True)
    inventory = json.loads(result.stdout)
    config, report = convert(inventory, args.mail_root, args.reddit_user)
    args.output.mkdir(parents=True, exist_ok=True)
    for name, data in [('entries.json', config), ('gnus-migration.json', report)]:
        path = args.output/name
        # Staging output is explicit and contains no credentials or message contents.
        path.write_text(json.dumps(data, ensure_ascii=False, indent=2)+'\n')
        path.chmod(0o600)
    print(json.dumps({key: len(report[key]) for key in ('migrated', 'pending', 'skipped')}))


if __name__ == '__main__':
    main()
