;;; compose-integration.el --- Contextual composers and drafts -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'camulus)

(defmacro with-compose-runtime (&rest body)
  `(let* ((directory (make-temp-file "camulus-compose-test" t))
          (camulus-state-file (expand-file-name "state.sqlite" directory))
          (process-environment (copy-sequence process-environment))
          (camulus-mail-root directory)
          (camulus-mail-sender "primary@example.test")
          (camulus-mail-accounts '(("/primary" "primary@example.test" "primary")
                                   ("/other" "other@example.test" "other")))
          (camulus-mail-sync-command nil)
          (camulus-reddit-username "test-user"))
     (setenv "XDG_STATE_HOME" directory)
     (unwind-protect (progn ,@body)
       (camulus-probe-session-stop)
       (dolist (buffer (buffer-list))
         (when (string-prefix-p "*Camulus post:" (buffer-name buffer)) (kill-buffer buffer)))
       (delete-directory directory t))))

(defun compose-test-press (key)
  (call-interactively (key-binding (kbd key))))

(defun compose-test-summary (source group)
  (switch-to-buffer (get-buffer-create "*Compose summary*"))
  (camulus-reader-feed-mode)
  (setq-local camulus-reader--entry `((source . ,source) (group . ,group))))

(ert-deftest mail-uses-chosen-account-and-a-uses-group ()
  (with-compose-runtime
   (compose-test-summary (concat "maildir:" directory) "/other/INBOX")
   (let (requested)
     (cl-letf (((symbol-function 'camulus-mail-submit)
                (lambda (request) (push request requested) (length requested)))
               ((symbol-function 'completing-read)
                (lambda (_prompt choices &rest _)
                  (should (equal choices '("Mail · primary@example.test (/primary)"
                                           "Mail · other@example.test (/other)")))
                  (car choices))))
       (compose-test-press "m")
       (compose-test-press "a")
       (should (equal (mapcar (lambda (r) (alist-get 'group r)) requested)
                      '("/other/INBOX" "/primary")))
       (should (cl-every (lambda (r) (equal (alist-get 'op r) "draft-new")) requested))
       (should (eq (key-binding (kbd "+")) #'camulus-reader-save))))))

(ert-deftest reddit-new-post-resumes-after-draft-close ()
  (with-compose-runtime
   (compose-test-summary "reddit:test-user" "emacs")
   (let ((summary (current-buffer)))
     (compose-test-press "a")
     (should (string-match-p "Reddit · r/emacs" (buffer-name)))
     (insert "标题")
     (goto-char (point-max))
     (insert "Markdown 正文")
     (compose-test-press "C-c C-d")
     (switch-to-buffer summary)
     (compose-test-press "a")
     (should (equal (buffer-string) "Subject: 标题\n\nMarkdown 正文"))
     (should (eq (key-binding (kbd "C-c C-c")) #'camulus-writing-send)))))

(defun compose-test-await (predicate)
  (let ((deadline (+ (float-time) 8)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(ert-deftest discourse-category-picker-and-post-receipt ()
  (with-compose-runtime
   (let* (posted
          (server
           (make-network-process
            :name "compose-forum-http" :server t :host "127.0.0.1" :service t :noquery t
            :filter
            (lambda (process bytes)
              (let ((request (concat (or (process-get process 'request) "") bytes)))
                (process-put process 'request request)
                (when (and (not (process-get process 'sent))
                           (string-match "\r\n\r\n" request))
                  (let* ((body (substring request (match-end 0)))
                         (length (if (string-match "Content-Length: \\([0-9]+\\)" request)
                                     (string-to-number (match-string 1 request)) 0)))
                    (when (>= (string-bytes body) length)
                      (process-put process 'sent t)
                      (let ((response
                             (if (string-prefix-p "GET /categories.json?" request)
                                 "{\"category_list\":{\"categories\":[{\"id\":42,\"name\":\"General\",\"slug\":\"general\"}]}}"
                               (setq posted body)
                               "{\"topic_id\":3,\"id\":5,\"post_number\":1,\"cooked\":\"<p>Body</p>\",\"username\":\"test\",\"created_at\":\"2026-11-21T00:00:00Z\"}")))
                        (process-send-string process
                         (format "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                                 (string-bytes response) response))
                        (process-send-eof process)))))))))
          (site (format "http://127.0.0.1:%d" (process-contact server :service)))
          (camulus-discourse-sites (list site)))
     (unwind-protect
         (cl-letf (((symbol-function 'discourse-auth-headers)
                    (lambda (_) '(("User-Api-Key" . "fixture") ("User-Api-Client-Id" . "test"))))
                   ((symbol-function 'completing-read)
                    (lambda (_prompt choices &rest _) (should (equal choices '("General [42]"))) (car choices))))
           (compose-test-summary (concat "discourse:" site) "latest")
           ;; Timers and filters may change window buffers under save-current-buffer.
           (compose-test-press "a")
           (compose-test-await (lambda () (string-prefix-p "*Camulus post:" (buffer-name (window-buffer)))))
           (let ((draft (window-buffer)))
             (with-current-buffer draft
               (insert "Fixture topic")
               (goto-char (point-max))
               (insert "Fixture body")
               (compose-test-press "C-c C-c"))
             (compose-test-await (lambda () (not (buffer-live-p draft))))
             (should (string-match-p "category=42" posted))
             (should (string-match-p "title=Fixture%20topic" posted))
             (should (string-match-p "raw=Fixture%20body" posted))))
       (delete-process server)))))

(ert-run-tests-batch-and-exit)
