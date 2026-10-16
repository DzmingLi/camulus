;;; mail-integration.el --- Real mu, MIME and native process bridge -*- lexical-binding: t; -*-
(require 'ert)
(require 'mail-demo)
(require 'smtp-fixture)
(defvar ef-mail-test-base (getenv "MAIL_FIXTURE"))
(unless ef-mail-test-base (error "MAIL_FIXTURE is required"))
(defun ef-mail-test-config (&optional program)
  (ef-mail-configure (expand-file-name "Maildir" ef-mail-test-base)
                     (expand-file-name "mu" ef-mail-test-base) "bob@example.test"
                     (or program (executable-find "mu")) ""))
(defmacro ef-mail-test-session (&rest body)
  `(progn (ef-mail-test-config) (ef-probe-session-start)
          (unwind-protect (progn ,@body) (ef-probe-session-stop))))
(defun ef-mail-test-finished (id)
  (cl-find-if (lambda (e) (and (eq (car e) 'finished) (= (nth 1 e) id))) ef-probe--events))
(defun ef-mail-test-await (id)
  (ef-probe-await (lambda () (ef-mail-test-finished id)) 20)
  (let ((event (ef-mail-test-finished id)))
    (unless (eq (nth 3 event) 'completed) (error "Mail task failed: %s" event))))
(defun ef-mail-test-data (id)
  (mapcar (lambda (e) (nth 3 e))
          (reverse (cl-remove-if-not (lambda (e) (and (eq (car e) 'mail) (= (cadr e) id))) ef-probe--events))))
(defun ef-mail-test-request (request)
  (let ((id (camulus-mail-submit request)))
    (ef-mail-test-await id) (ef-mail-test-data id)))

(ert-deftest mail-native-group-summary-and-search ()
  (ef-mail-test-session
   (let* ((data (car (ef-mail-test-request '((op . "list") (group . "/Inbox")))))
          (rows (alist-get 'messages data)))
     (should (= 3 (length rows)))
     (should (cl-find "mail:child@example.test" rows :key (lambda (r) (alist-get 'id r)) :test #'equal)))
   (let* ((data (car (ef-mail-test-request '((op . "search") (group . "/Inbox") (query . "needle")))))
          (rows (alist-get 'messages data)))
     (should (= 1 (length rows)))
     (should-not (equal "mail:outside@example.test" (alist-get 'id (car rows)))))
   (should (ef-probe-signals-unchanged))))

(ert-deftest mail-native-progressive-thread-and-attachment ()
  (ef-mail-test-session
   (let* ((events (ef-mail-test-request '((op . "thread") (id . "mail:root@example.test"))))
          (articles (cl-remove-if-not (lambda (x) (equal (alist-get 'kind x) "article")) events))
          (root (cl-find "mail:root@example.test" articles
                         :key (lambda (x) (alist-get 'id (alist-get 'message x))) :test #'equal))
          (attachments (alist-get 'attachments root))
          (download (expand-file-name "download.bin" ef-mail-test-base)))
     (should (= 3 (length articles)))
     (should (alist-get 'complete (car (last events))))
     (should (equal "mail:reply@example.test"
                    (alist-get 'parent_id (alist-get 'message
                        (cl-find "mail:child@example.test" articles
                                 :key (lambda (x) (alist-get 'id (alist-get 'message x))) :test #'equal)))))
     (should (= 1 (length attachments)))
     (should (string-match-p "HTML 正文" (alist-get 'html (alist-get 'message root))))
     (when (file-exists-p download) (delete-file download))
     (ef-mail-test-request `((op . "attachment") (id . "mail:root@example.test")
                             (part . ,(alist-get 'part (car attachments))) (destination . ,download)))
     (with-temp-buffer
       (set-buffer-multibyte nil) (insert-file-contents-literally download)
       (should (equal (buffer-string) "attachment\0bytes\n")))
     (should (= #o600 (file-modes download))))))

(ert-deftest mail-native-event-loop-and-cancellation ()
  (ef-mail-test-config (expand-file-name "delayed-mu" ef-mail-test-base))
  (ef-probe-session-start)
  (let ((beats 0) timer)
    (setq timer (run-at-time 0.01 0.01 (lambda () (cl-incf beats))))
    (unwind-protect
        (progn
          (ef-mail-test-request '((op . "list") (group . "/Inbox")))
          (should (> beats 5))
          (should (ef-probe-signals-unchanged)))
      (cancel-timer timer) (ef-probe-session-stop)))
  (ef-mail-test-config (expand-file-name "slow-mu" ef-mail-test-base))
  (ef-probe-session-start)
  (unwind-protect
      (let ((id (camulus-mail-submit '((op . "list") (group . "/Inbox")))))
        (ef-probe-await (lambda () (> (hash-table-count ef-host--jobs) 0)))
        (ef-probe-cancel id)
        (ef-probe-await (lambda () (ef-mail-test-finished id)))
        (should (eq (nth 3 (ef-mail-test-finished id)) 'cancelled))
        (ef-probe-await (lambda () (= 0 (hash-table-count ef-host--jobs))))
        (should (equal (ef-probe-status) "running")))
    (ef-probe-session-stop)))

(ert-deftest mail-demo-expanded-tree-with-inline-attachments ()
  (ef-mail-test-session
   (unwind-protect
       (progn
         (ef-mail-browse "/Inbox")
         (ef-mail-test-await ef-mail--active)
         (goto-char (point-min))
         (search-forward "Re: 测试主题")
         (ef-mail-open-thread)
         (with-current-buffer "*Mail thread*"
           (ef-mail-test-await ef-mail--active)
           (should (string-match-p "root body" (buffer-string)))
           (should (string-match-p "child body" (buffer-string)))
           (should (< (string-match "root body" (buffer-string)) (string-match "child body" (buffer-string))))
           (should (string-match-p "Save ../附件.bin" (buffer-string)))
           (should ef-mail--complete)))
     (dolist (b '("*Mail /Inbox*" "*Mail thread*")) (when (get-buffer b) (kill-buffer b))))))

(ert-deftest mail-native-send-to-loopback-tls-only ()
  (ef-mail-configure (expand-file-name "Maildir" ef-mail-test-base)
                     (expand-file-name "mu" ef-mail-test-base) "bob@example.test"
                     (executable-find "mu") "")
  (ef-test-smtp ef-mail-test-base "" "bob@example.test")
  (ef-probe-session-start)
  (unwind-protect
      (let ((id (camulus-mail-submit
                 '((op . "send") (to . ["alice@example.test"])
                                (subject . "测试回复") (body . "A test reply")
                                (reply_to . "mail:root@example.test")))))
        (ef-probe-close-view)
        (ef-mail-test-await id)
        (should (equal (alist-get 'kind (car (ef-mail-test-data id))) "sent"))
        (with-temp-buffer
          (insert-file-contents (expand-file-name "sent.eml" ef-mail-test-base))
          (should (string-match-p "In-Reply-To: <root@example.test>" (buffer-string)))
          (should (string-match-p "From: bob@example.test" (buffer-string))))
        (should (ef-probe-signals-unchanged)))
    (ef-probe-session-stop)))

(ert-deftest mail-native-smtp-rejects-wrong-host-certificate ()
  (ef-mail-test-config)
  (ef-test-smtp ef-mail-test-base "" "bob@example.test" "SMTP_WRONG_HOST_PORT" nil
                (expand-file-name "smtp-wrong-cert.pem" ef-mail-test-base))
  (ef-probe-session-start)
  (unwind-protect
      (let ((id (camulus-mail-submit '((op . "send") (to . ["a@example.test"]) (body . "never sent")))))
        (ef-probe-await (lambda () (ef-mail-test-finished id)) 10)
        (should (eq (nth 3 (ef-mail-test-finished id)) 'failed))
        (should (ef-probe-signals-unchanged)))
    (ef-probe-session-stop)))

(ert-deftest mail-native-smtp-rejects-untrusted-certificate ()
  (ef-mail-test-config)
  (ef-test-smtp ef-mail-test-base "" "bob@example.test" nil nil
                (expand-file-name "smtp-wrong-cert.pem" ef-mail-test-base))
  (ef-probe-session-start)
  (unwind-protect
      (let ((id (camulus-mail-submit '((op . "send") (to . ["a@example.test"]) (body . "never sent")))))
        (ef-probe-await (lambda () (ef-mail-test-finished id)) 10)
        (should (eq (nth 3 (ef-mail-test-finished id)) 'failed)))
    (ef-probe-session-stop)))

(ert-deftest mail-native-smtp-wait-is-cancellable-and-keeps-heartbeat ()
  (ef-mail-test-config)
  (ef-test-smtp ef-mail-test-base "" "bob@example.test" "SMTP_SLOW_PORT")
  (ef-probe-session-start)
  (let ((beats 0) timer)
    (setq timer (run-at-time 0.01 0.01 (lambda () (cl-incf beats))))
    (unwind-protect
        (let ((id (camulus-mail-submit '((op . "send") (to . ["a@example.test"]) (body . "cancelled")))))
          (ef-probe-await (lambda () (> beats 10)) 2)
          (should-not (ef-mail-test-finished id))
          (ef-probe-cancel id)
          (ef-probe-await (lambda () (ef-mail-test-finished id)) 2)
          (should (eq (nth 3 (ef-mail-test-finished id)) 'cancelled))
          (should (ef-probe-signals-unchanged)))
      (cancel-timer timer) (ef-probe-session-stop))))
(ert-run-tests-batch-and-exit)
