;;; mail-trash-integration.el --- Optimistic Trash and rollback -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'camulus)

(defun camulus-mail-test-rows (&optional source)
  (mapcar (lambda (id)
            `((source . ,(or source "maildir:/fixture")) (id . ,(concat "mail:" id "@example.test"))
              (conversation_id . ,(concat "mail:" id "@example.test")) (title . ,id)
              (author . "Fixture") (groups . ("/work/Inbox")) (unread . t) (occurred . 1)))
          '("a" "b" "c")))
(defun camulus-mail-test-visible ()
  (mapcar (lambda (entry) (nth 1 (car entry))) tabulated-list-entries))
(defun camulus-mail-test-await (predicate)
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defmacro camulus-mail-with-trash-fixture (&rest body)
  `(with-temp-buffer
     (camulus-reader-feed-mode)
     (setq-local camulus-reader--entry '((source . "maildir:/fixture") (group . "/work/Inbox")))
     (let ((rows (camulus-mail-test-rows)) (requests nil) (next-id 0) messages)
       (camulus-reader--set-rows rows)
       (goto-char (point-min))
       (cl-letf (((symbol-function 'camulus-mail-submit)
                  (lambda (request) (push request requests) (cl-incf next-id)))
                 ((symbol-function 'message)
                  (lambda (format &rest args) (push (apply #'format format args) messages))))
         (unwind-protect (progn ,@body)
           (dotimes (i next-id)
             (camulus-mail--action-event (list 'finished (1+ i) 'cancelled "Fixture cleanup"))))))))

(ert-deftest mail-trash-hides-immediately-and-d-d-removes-two-different-rows ()
  (camulus-mail-with-trash-fixture
   (camulus-reader-delete)
   (should (equal (camulus-mail-test-visible) '("mail:b@example.test" "mail:c@example.test")))
   (should (equal (alist-get 'id (camulus-reader--selected)) "mail:b@example.test"))
   (camulus-mail--action-event '(mail 1 ((kind . "progress") (message . "Waiting for mail access"))))
   (should (equal (car messages) "Waiting for mail access"))
   ;; A refresh of the old cache cannot resurrect pending rows.
   (camulus-reader--set-rows rows)
   (should (equal (camulus-mail-test-visible) '("mail:b@example.test" "mail:c@example.test")))
   (camulus-reader-delete)
   (should (= 2 (length requests)))
   (should (equal (mapcar (lambda (r) (alist-get 'id r)) requests)
                  '("mail:b@example.test" "mail:a@example.test")))
   (should (equal (camulus-mail-test-visible) '("mail:c@example.test")))
   (camulus-mail--action-event '(finished 1 failed "mu write lock"))
   (should (string-match-p "mu write lock" (car messages)))
   (should (equal (camulus-mail-test-visible) '("mail:a@example.test" "mail:c@example.test")))
   ;; Restoring an earlier row must not steal point back from the current one.
   (should (equal (alist-get 'id (camulus-reader--selected)) "mail:c@example.test"))
   (camulus-mail--action-event '(finished 2 cancelled "Cancelled"))
   (should (= 3 (length tabulated-list-entries)))))

(ert-deftest mail-trash-masks-home-and-group-but-preserves-favorites ()
  (camulus-mail-with-trash-fixture
   (let ((home (generate-new-buffer " *mail-home*"))
         (favorites (generate-new-buffer " *mail-favorites*"))
         (other (generate-new-buffer " *mail-other-group*")))
     (unwind-protect
         (progn
           (with-current-buffer home
             (camulus-reader-feed-mode)
             (setq-local camulus-reader--home t camulus-reader-home--entries nil)
             (camulus-reader--set-rows rows))
           (with-current-buffer favorites
             (camulus-reader-folder-mode)
             (camulus-reader--set-rows (mapcar (lambda (row) (cons '(target . "thread") row)) rows)))
           (with-current-buffer other
             (camulus-reader-feed-mode)
             (setq-local camulus-reader--entry '((source . "maildir:/fixture") (group . "/work/Sent")))
             (camulus-reader--set-rows rows))
           (camulus-reader-delete)
           (with-current-buffer home
             (camulus-reader--set-rows rows)
             (should (= 2 (length tabulated-list-entries))))
           (with-current-buffer favorites (should (= 3 (length tabulated-list-entries))))
           (with-current-buffer other (should (= 3 (length tabulated-list-entries))))
           (camulus-mail--action-event '(mail 1 ((kind . "trashed") (destination . "/work/Trash")
                                               (moved . ("mail:a@example.test")) (failed . 1))))
           (camulus-mail--action-event '(finished 1 completed ""))
           (should (= 3 (length tabulated-list-entries)))
           (with-current-buffer home (should (= 3 (length tabulated-list-entries))))
           (should (string-match-p "Some messages could not be moved" (car messages))))
       (mapc #'kill-buffer (list home favorites other))))))

(ert-deftest mail-trash-submit-failure-rolls-back-and-can-be-retried ()
  (camulus-mail-with-trash-fixture
   (cl-letf (((symbol-function 'camulus-mail-submit) (lambda (_) (error "Worker busy"))))
     (should-error (camulus-reader-delete)))
   (should (= 3 (length tabulated-list-entries)))
   (goto-char (point-min))
   (camulus-reader-delete)
   (should (= 1 (length requests)))
   (camulus-mail--action-event '(finished 1 completed ""))
   (should (= 3 (length tabulated-list-entries)))
   (should (string-match-p "No Trash result received" (car messages)))))

(ert-deftest mail-trash-success-waits-for-fresh-cache-and-rejects-late-old-response ()
  (camulus-mail-with-trash-fixture
   (let* ((directory (make-temp-file "camulus-trash-cache" t))
          (index (expand-file-name "index.json" directory))
          (program (expand-file-name "mu-fixture" directory))
          (source (concat "maildir:" directory))
          old-callback)
     (unwind-protect
         (progn
           ;; Only list queries hit this disposable program. No real Maildir,
           ;; mu database, synchronization or remote write is used by this test.
           (with-temp-file program
             (insert "#!" (executable-find "sh") "\nexec " (shell-quote-argument (executable-find "cat"))
                     " " (shell-quote-argument index) "\n"))
           (set-file-modes program #o700)
           (with-temp-file index
             (insert "[{\"message-id\":\"b@example.test\",\"maildir\":\"/work/Inbox\",\"subject\":\"b\",\"date-unix\":2},"
                     "{\"message-id\":\"c@example.test\",\"maildir\":\"/work/Inbox\",\"subject\":\"c\",\"date-unix\":1}]"))
           (setq rows (camulus-mail-test-rows source))
           (setq-local camulus-reader--entry `((source . ,source) (group . "/work/Inbox")))
           (camulus-reader--set-rows rows)
           (goto-char (point-min))
           (camulus-content-state-configure (expand-file-name "state.sqlite" directory))
           (camulus-mail-configure directory (expand-file-name "mu" directory) "fixture@example.test" program "")
           (camulus-probe-session-start)
           (camulus-reader--load-list nil)
           (setq old-callback (gethash camulus-reader--list-request camulus-reader--requests))
           (camulus-reader-delete)
           (should (= 2 (length tabulated-list-entries)))
           (camulus-mail--action-event '(mail 1 ((kind . "trashed") (destination . "/work/Trash")
                                               (moved . ("mail:a@example.test")) (failed . 0))))
           (should (= 2 (length tabulated-list-entries)))
           (camulus-mail--action-event '(finished 1 completed ""))
           (camulus-mail-test-await (lambda () (= 0 (hash-table-count camulus-reader--requests))))
           (should (equal (camulus-mail-test-visible) '("mail:b@example.test" "mail:c@example.test")))
           (funcall old-callback `((kind . "feed") (rows . ,rows)))
           (should (equal (camulus-mail-test-visible) '("mail:b@example.test" "mail:c@example.test")))
           ;; A successful move didn't write an ordinary feed dismissal.
           (let ((db (sqlite-open (expand-file-name "state.sqlite" directory))))
             (unwind-protect
                 (should (equal (sqlite-select db "SELECT count(*) FROM dismissals") '((0))))
               (sqlite-close db))))
       (camulus-probe-session-stop)
       (delete-directory directory t)))))
(ert-run-tests-batch-and-exit)
