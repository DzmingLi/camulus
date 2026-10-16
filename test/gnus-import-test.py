#!/usr/bin/env python3
import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('gnus_import', ROOT/'scripts/import-gnus.py')
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)

class ImportTests(unittest.TestCase):
    def test_mapping_and_levels(self):
        groups = [dict(group=g, backend=b, level=l) for b, g, l in [
            ('nnjanestreet','nnjanestreet:posts',2),
            ('nnzhihu','nnzhihu:answers.marisa.moe',2),
            ('nndouban','nndouban:timeline.123',2),
            ('nndouban','nndouban:replies.456',2),
            ('nntheatlantic','nntheatlantic:author.ian-bogost',2),
            ('nnzhihu','nnzhihu:answers.old',6),
            ('nndraft','nndraft:drafts',1),
            ('unknown','unknown:future',2)]]
        config, report = module.convert(dict(groups=groups,rss=[]), Path('/mail'), 'alice')
        self.assertEqual(len(config['entries']),5)
        self.assertEqual(len(report['pending']),1)
        self.assertEqual(len(report['skipped']),2)
        self.assertEqual(config['entries'][1]['group'],'answers.marisa.moe')
    def test_blogger_and_gcores_are_native_entries(self):
        groups = [dict(group='nnblogger:posts.example.blogspot.com', backend='nnblogger', level=2),
                  dict(group='nngcores:talks.31418', backend='nngcores', level=2)]
        config, report = module.convert(dict(groups=groups, rss=[]), Path('/mail'), None)
        self.assertEqual([(e['source'], e['group']) for e in config['entries']],
                         [('blogger', 'posts.example.blogspot.com'), ('gcores', 'talks.31418')])
        self.assertTrue(all(e['timeline'] for e in config['entries']))
        self.assertFalse(report['pending'] + report['skipped'])

    def test_csrin_forum_and_topic_entries(self):
        groups = [dict(group='nncsrin:forum.14', backend='nncsrin', level=2),
                  dict(group='nncsrin:topic.12', backend='nncsrin', level=2)]
        config, report = module.convert(dict(groups=groups, rss=[]), Path('/mail'), None)
        self.assertEqual([(e['source'], e['group']) for e in config['entries']],
                         [('csrin', 'forum.14'), ('csrin', 'topic.12')])
        self.assertFalse(report['pending'] + report['skipped'])

    def test_dribble_overrides_and_no_evaluation(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory)
            old=root/'newsrc'
            marker=root/'MUST-NOT-EXIST'
            old.write_text(f'''(setq gnus-newsrc-alist '(("nnjanestreet:posts" 6 nil nil (nnjanestreet "blog.janestreet.com") nil)))
(write-region "unsafe" nil "{marker}")\n''')
            Path(str(old)+'-dribble').write_text('(gnus-group-change-level "nnjanestreet:posts" 2)\n')
            result=subprocess.run(['python3',str(ROOT/'scripts/import-gnus.py'),'--newsrc',str(old),'--news',str(root),'--output',str(root/'out')],capture_output=True,text=True,check=True)
            self.assertEqual(json.loads(result.stdout)['migrated'],1)
            self.assertFalse(marker.exists())
            self.assertEqual((root/'out/entries.json').stat().st_mode & 0o777,0o600)
    def test_dedup_and_unsupported_retained(self):
        row=dict(group='nnjanestreet:posts',backend='nnjanestreet',level=2)
        config, report=module.convert(dict(groups=[row,row,dict(group='unknown:talks.1',backend='unknown',level=2)],rss=[]),Path('/mail'),None)
        self.assertEqual(len(config['entries']),1)
        self.assertEqual(report['pending'][0]['group'],'unknown:talks.1')

if __name__=='__main__': unittest.main()
