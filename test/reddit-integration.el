;;; reddit-integration.el --- Offline native backend/UI checks -*- lexical-binding: t; -*-
(require 'ert)
(require 'camulus)
(setq camulus-probe-record-events t)

(defmacro camulus-reddit-test-session (&rest body)
  `(progn
     (camulus-reddit-configure "fixture-user" "/nonexistent/reddit-test-token"
                          "5oagOpX2_NKVDej_iuZjFA" "http://127.0.0.1:17973")
     (camulus-probe-session-start)
     (unwind-protect
         (progn (camulus-probe-await (lambda () (equal (camulus-probe-status) "running"))) ,@body)
       (camulus-probe-session-stop))))

(defun camulus-reddit-test-data (id)
  (when-let* ((event (cl-find-if (lambda (e) (and (eq (car e) 'reddit) (= (cadr e) id))) camulus-probe--events)))
    (nth 3 event)))

(ert-deftest reddit-native-invalid-input ()
  (camulus-reddit-test-session
   (let ((id (camulus-reddit-submit '((op . "subreddit") (name . "../invalid")))))
     (camulus-probe-await (lambda () (camulus-reddit-test-data id)))
     (let ((data (camulus-reddit-test-data id)))
       (should (equal (alist-get 'kind data) "error"))
       (should (string-match-p "subreddit" (alist-get 'message data)))))))

(ert-deftest reddit-native-authorization-is-service-owned ()
  (camulus-reddit-test-session
   (let ((id (camulus-reddit-submit '((op . "authorize")))))
     (camulus-probe-close-view)
     (camulus-probe-await (lambda () (camulus-reddit-test-data id)))
     (let ((data (camulus-reddit-test-data id)))
       (should (equal (alist-get 'kind data) "authorize"))
       (should (string-match-p "5oagOpX2_NKVDej_iuZjFA" (alist-get 'url data)))
       (should (string-match-p "vote" (alist-get 'url data)))))))

(ert-deftest reddit-native-missing-token-is-local-error ()
  (camulus-reddit-test-session
   (let ((id (camulus-reddit-submit '((op . "identity")))))
     (camulus-probe-await (lambda () (camulus-reddit-test-data id)))
     (should (equal (alist-get 'kind (camulus-reddit-test-data id)) "error"))
     (should (equal (camulus-probe-status) "running")))))

(ert-deftest reddit-reader-tree-is-expanded-and-chronological ()
  (with-temp-buffer
    (camulus-reader-article-mode) (setq camulus-reader--source "reddit:fixture-user")
    (setq camulus-reader--messages (make-hash-table :test #'equal))
    (dolist (m '(((id . "t3_r") (author . "root") (body . "root text") (created . 1))
                 ((id . "t1_b") (parent_id . "t3_r") (author . "second") (body . "later") (created . 3))
                 ((id . "t1_a") (parent_id . "t3_r") (author . "first") (body . "earlier") (created . 2))))
      (puthash (alist-get 'id m) m camulus-reader--messages))
    (setq camulus-reader--complete t) (maphash (lambda (id _) (puthash id t camulus-reader--loaded)) camulus-reader--messages) (camulus-reader--render)
    (should (string-match-p "root text" (buffer-string)))
    (should (< (string-match "earlier" (buffer-string)) (string-match "later" (buffer-string))))))

(ert-deftest reddit-vote-shared-optimistic-state-and-rejection ()
  (camulus-reddit-test-session
   (let ((summary (generate-new-buffer " *vote-summary*"))
         (thread (generate-new-buffer " *vote-thread*"))
         (item '((id . "t3_vote") (author . "test") (title . "Post")
                 (body . "Text") (created . 1) (score . 42) (my_vote . -1))))
     (unwind-protect
         (progn
           (with-current-buffer summary
             (camulus-reader-feed-mode) (setq camulus-reader--source "reddit:fixture-user")
             (setq camulus-reader--messages (make-hash-table :test #'equal))
             (puthash "t3_vote" item camulus-reader--messages)
             (camulus-reader--set-rows (list (append '((source . "reddit:fixture-user")) item)))
             (goto-char (point-min)))
           (with-current-buffer thread
             (camulus-reader-article-mode) (setq camulus-reader--source "reddit:fixture-user")
             (setq camulus-reader--messages (make-hash-table :test #'equal))
             (puthash "t3_vote" item camulus-reader--messages)
             (setq camulus-reader--complete t) (maphash (lambda (id _) (puthash id t camulus-reader--loaded)) camulus-reader--messages) (camulus-reader--render)
             (goto-char (point-min))
             (camulus-reddit-upvote)
             (should (equal (seq-take (camulus-reddit-vote-state "t3_vote") 2) '(1 pending)))
             (should (string-match-p "+ pending" (buffer-string)))
             (should-error (camulus-reddit-clear-vote))
             (should-not camulus-reader--active))
           (with-current-buffer summary
             (should (string-match-p "\\+ pending" (aref (cadar tabulated-list-entries) 3))))
           ;; Votes outlive the reading view. The nonexistent token prevents HTTP.
           (camulus-probe-close-view)
           (camulus-probe-await (lambda () (eq (cadr (camulus-reddit-vote-state "t3_vote")) 'rejected)))
           (should (= (car (camulus-reddit-vote-state "t3_vote")) -1))
           (with-current-buffer summary
             (should (string-match-p "- failed" (aref (cadar tabulated-list-entries) 3))))
           (with-current-buffer thread
             (should (string-match-p "- failed" (buffer-string)))
             (should (string-suffix-p "loaded" (apply #'concat header-line-format)))))
       (kill-buffer summary) (kill-buffer thread)))))

(ert-deftest reddit-vote-validates-before-optimistic-change ()
  (camulus-reddit-test-session
   (should-error (camulus-reddit-vote "t3_valid" 0 2))
   (should-error (camulus-reddit-vote "../invalid" 0 1))
   (should-not (camulus-reddit-vote-state "t3_valid"))))

(ert-deftest reddit-vote-does-not-replace-or-cancel-thread-read ()
  (camulus-reddit-test-session
   (with-temp-buffer
     (camulus-reader-article-mode) (setq camulus-reader--source "reddit:fixture-user")
     (setq camulus-reader--messages (make-hash-table :test #'equal)
           camulus-reader--active 900000)
     (puthash "t1_vote" '((id . "t1_vote") (author . "test") (body . "Text") (created . 1))
              camulus-reader--messages)
     (camulus-reader--render)
     (goto-char (point-min))
     (camulus-reddit-downvote)
     (should (= camulus-reader--active 900000))
     (should (string-suffix-p "loading / partial" (apply #'concat header-line-format)))
     (setq camulus-reader--active nil))))

(ert-run-tests-batch-and-exit)
