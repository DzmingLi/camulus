;;; reddit-integration.el --- Offline native backend/UI checks -*- lexical-binding: t; -*-
(require 'ert)
(require 'camulus)
(setq ef-probe-record-events t)

(defmacro ef-reddit-test-session (&rest body)
  `(progn
     (ef-reddit-configure "fixture-user" "/nonexistent/reddit-test-token"
                          "5oagOpX2_NKVDej_iuZjFA" "http://127.0.0.1:17973")
     (ef-probe-session-start)
     (unwind-protect
         (progn (ef-probe-await (lambda () (equal (ef-probe-status) "running"))) ,@body)
       (ef-probe-session-stop))))

(defun ef-reddit-test-data (id)
  (when-let* ((event (cl-find-if (lambda (e) (and (eq (car e) 'reddit) (= (cadr e) id))) ef-probe--events)))
    (nth 3 event)))

(ert-deftest reddit-native-invalid-input ()
  (ef-reddit-test-session
   (let ((id (camulus-reddit-submit '((op . "subreddit") (name . "../invalid")))))
     (ef-probe-await (lambda () (ef-reddit-test-data id)))
     (let ((data (ef-reddit-test-data id)))
       (should (equal (alist-get 'kind data) "error"))
       (should (string-match-p "subreddit" (alist-get 'message data)))))))

(ert-deftest reddit-native-authorization-is-service-owned ()
  (ef-reddit-test-session
   (let ((id (camulus-reddit-submit '((op . "authorize")))))
     (ef-probe-close-view)
     (ef-probe-await (lambda () (ef-reddit-test-data id)))
     (let ((data (ef-reddit-test-data id)))
       (should (equal (alist-get 'kind data) "authorize"))
       (should (string-match-p "5oagOpX2_NKVDej_iuZjFA" (alist-get 'url data)))
       (should (string-match-p "vote" (alist-get 'url data)))))))

(ert-deftest reddit-native-missing-token-is-local-error ()
  (ef-reddit-test-session
   (let ((id (camulus-reddit-submit '((op . "identity")))))
     (ef-probe-await (lambda () (ef-reddit-test-data id)))
     (should (equal (alist-get 'kind (ef-reddit-test-data id)) "error"))
     (should (equal (ef-probe-status) "running")))))

(ert-deftest reddit-reader-tree-is-expanded-and-chronological ()
  (with-temp-buffer
    (ef-reader-article-mode) (setq ef-reader--source "reddit:fixture-user")
    (setq ef-reader--messages (make-hash-table :test #'equal))
    (dolist (m '(((id . "t3_r") (author . "root") (body . "root text") (created . 1))
                 ((id . "t1_b") (parent_id . "t3_r") (author . "second") (body . "later") (created . 3))
                 ((id . "t1_a") (parent_id . "t3_r") (author . "first") (body . "earlier") (created . 2))))
      (puthash (alist-get 'id m) m ef-reader--messages))
    (setq ef-reader--complete t) (maphash (lambda (id _) (puthash id t ef-reader--loaded)) ef-reader--messages) (ef-reader--render)
    (should (string-match-p "root text" (buffer-string)))
    (should (< (string-match "earlier" (buffer-string)) (string-match "later" (buffer-string))))))

(ert-deftest reddit-vote-shared-optimistic-state-and-rejection ()
  (ef-reddit-test-session
   (let ((summary (generate-new-buffer " *vote-summary*"))
         (thread (generate-new-buffer " *vote-thread*"))
         (item '((id . "t3_vote") (author . "test") (title . "Post")
                 (body . "Text") (created . 1) (score . 42) (my_vote . -1))))
     (unwind-protect
         (progn
           (with-current-buffer summary
             (ef-reader-feed-mode) (setq ef-reader--source "reddit:fixture-user")
             (setq ef-reader--messages (make-hash-table :test #'equal))
             (puthash "t3_vote" item ef-reader--messages)
             (ef-reader--set-rows (list (append '((source . "reddit:fixture-user")) item)))
             (goto-char (point-min)))
           (with-current-buffer thread
             (ef-reader-article-mode) (setq ef-reader--source "reddit:fixture-user")
             (setq ef-reader--messages (make-hash-table :test #'equal))
             (puthash "t3_vote" item ef-reader--messages)
             (setq ef-reader--complete t) (maphash (lambda (id _) (puthash id t ef-reader--loaded)) ef-reader--messages) (ef-reader--render)
             (goto-char (point-min))
             (ef-reddit-upvote)
             (should (equal (seq-take (ef-reddit-vote-state "t3_vote") 2) '(1 pending)))
             (should (string-match-p "+ pending" (buffer-string)))
             (should-error (ef-reddit-clear-vote))
             (should-not ef-reader--active))
           (with-current-buffer summary
             (should (string-match-p "\\+ pending" (aref (cadar tabulated-list-entries) 3))))
           ;; Votes outlive the reading view. The nonexistent token prevents HTTP.
           (ef-probe-close-view)
           (ef-probe-await (lambda () (eq (cadr (ef-reddit-vote-state "t3_vote")) 'rejected)))
           (should (= (car (ef-reddit-vote-state "t3_vote")) -1))
           (with-current-buffer summary
             (should (string-match-p "- failed" (aref (cadar tabulated-list-entries) 3))))
           (with-current-buffer thread
             (should (string-match-p "- failed" (buffer-string)))
             (should (string-suffix-p "loaded" header-line-format))))
       (kill-buffer summary) (kill-buffer thread)))))

(ert-deftest reddit-vote-validates-before-optimistic-change ()
  (ef-reddit-test-session
   (should-error (ef-reddit-vote "t3_valid" 0 2))
   (should-error (ef-reddit-vote "../invalid" 0 1))
   (should-not (ef-reddit-vote-state "t3_valid"))))

(ert-deftest reddit-vote-does-not-replace-or-cancel-thread-read ()
  (ef-reddit-test-session
   (with-temp-buffer
     (ef-reader-article-mode) (setq ef-reader--source "reddit:fixture-user")
     (setq ef-reader--messages (make-hash-table :test #'equal)
           ef-reader--active 900000)
     (puthash "t1_vote" '((id . "t1_vote") (author . "test") (body . "Text") (created . 1))
              ef-reader--messages)
     (ef-reader--render)
     (goto-char (point-min))
     (ef-reddit-downvote)
     (should (= ef-reader--active 900000))
     (should (string-suffix-p "loading / partial" header-line-format))
     (setq ef-reader--active nil))))

(ert-run-tests-batch-and-exit)
