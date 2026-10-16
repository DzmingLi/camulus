"""Private, disposable Maildir + mu index. Never reads the user's mailbox."""
import pathlib, subprocess, sys, shutil
from email.message import EmailMessage
from email.policy import SMTP

base = pathlib.Path(sys.argv[1]).resolve()
root = base / "Maildir"
home = base / "mu"
home.mkdir(parents=True, exist_ok=True)
for account in ["work", "second", "trash", "trash-other"]:
    # Only the disposable workflow fixtures are reset between test runs.
    if (root / account).exists():
        shutil.rmtree(root / account)
for folder in ["Inbox", "Other", "work/Inbox", "work/Drafts", "work/Archive", "work/Sent", "second/Inbox", "second/Drafts", "trash/Inbox", "trash/Sent", "trash-other/Inbox"]:
    for sub in ["cur", "new", "tmp"]:
        (root / folder / sub).mkdir(parents=True, exist_ok=True)

def message(name, minute, subject, body, parent=None, refs=None, folder="Inbox", attachment=False):
    m = EmailMessage(policy=SMTP)
    m["From"] = "Alice <alice@example.test>"
    m["To"] = "Bob <bob@example.test>"
    m["Date"] = f"Thu, 15 Oct 2026 10:{minute:02}:00 +0800"
    m["Message-ID"] = f"<{name}@example.test>"
    m["Subject"] = subject
    if parent:
        m["In-Reply-To"] = f"<{parent}@example.test>"
    if refs:
        m["References"] = " ".join(f"<{r}@example.test>" for r in refs)
    m.set_content(body)
    if attachment:
        m.add_alternative("<p>HTML 正文</p>", subtype="html")
        m.add_attachment(b"attachment\x00bytes\n", maintype="application", subtype="octet-stream", filename="../附件.bin")
    (root / folder / "cur" / name).write_bytes(m.as_bytes())

message("root", 0, "测试主题", "root body needle", attachment=True)
message("reply", 1, "Re: 测试主题", "reply body needle", "root", ["root"])
message("child", 2, "Re: 测试主题", "child body", "reply", ["root", "reply"])
message("independent", 3, "测试主题", "different discussion")
message("orphan", 4, "Orphan", "missing parent", "missing", ["missing"])
message("outside", 5, "Outside", "needle", folder="Other")
message("archive-root", 0, "Archive me", "archive root", folder="work/Inbox", attachment=True)
message("archive-reply", 1, "Re: Archive me", "archive reply", "archive-root", ["archive-root"], folder="work/Inbox")
message("sent-relative", 2, "Re: Archive me", "already sent", "archive-root", ["archive-root"], folder="work/Sent")
shutil.copyfile(root / "work/Inbox/cur/archive-root", root / "second/Inbox/cur/duplicate")
(root / "work/Inbox/cur/archive-root").rename(root / "work/Inbox/cur/archive-root,U=77:2,FR")
message("trash-root", 0, "Trash me", "trash root", folder="trash/Inbox", attachment=True)
message("trash-reply", 1, "Re: Trash me", "trash reply", "trash-root", ["trash-root"], folder="trash/Inbox")
message("trash-sent", 2, "Re: Trash me", "keep sent copy", "trash-root", ["trash-root"], folder="trash/Sent")
shutil.copyfile(root / "trash/Inbox/cur/trash-root", root / "trash-other/Inbox/cur/duplicate")
(root / "trash/Inbox/cur/trash-root").rename(root / "trash/Inbox/cur/trash-root,U=18:2,FR")
subprocess.run(["mu", "init", "--quiet", "--muhome", str(home), "--maildir", str(root)], check=True)
subprocess.run(["mu", "index", "--quiet", "--muhome", str(home)], check=True)
for name, delay in [("slow-mu", 20), ("delayed-mu", 0.25)]:
    script = base / name
    script.write_text(f"#!{sys.executable}\nimport time\ntime.sleep({delay})\nprint('[]')\n")
    script.chmod(0o700)
print("Created synthetic Maildir and mu index")
