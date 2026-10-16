;;; reader-presentation.el --- Thread layout and window behavior -*- lexical-binding: t; -*-
(require 'ert)
(require 'camulus)
(setq ef-probe-record-events t)

(ert-deftest reader-summary-read-rows-are-muted-without-hiding ()
  (with-temp-buffer
    (ef-reader-feed-mode)
    (let ((row '((source . "test") (id . "one") (title . "Read title") (author . "Alice") (unread . nil))))
      (ef-reader--set-rows (list row))
      (goto-char (point-min))
      (search-forward "Read title")
      (should (memq 'ef-reader-read (ensure-list (get-text-property (1- (point)) 'face))))
      (should (= 1 (hash-table-count ef-reader--rows)))
      (setf (alist-get 'unread row) t)
      (ef-reader--set-rows (list row))
      (goto-char (point-min))
      (search-forward "Read title")
      (should-not (memq 'ef-reader-read (ensure-list (get-text-property (1- (point)) 'face))))
      (should-not (get-text-property 0 'face (alist-get 'title row))))))

(ert-deftest reader-thread-layout-keeps-bodies-and-attachments-in-the-tree ()
  (let ((ef-reader--states (make-hash-table :test #'equal)))
    (with-temp-buffer
      (ef-reader-article-mode)
      (setq ef-reader--source "discourse:test")
      (dolist (message '(((id . "root") (author . "Alice") (title . "Discussion") (created . 1.25) (body . "Root body"))
                         ((id . "reply") (parent_id . "root") (author . "Bob") (created . 2.5) (body . "Reply body\nSecond line"))
                         ((id . "nested") (parent_id . "reply") (author . "Carol") (created . 3)
                          (body . "") (html . "<p>HTML reply <code>code()</code></p>"))))
        (puthash (alist-get 'id message) message ef-reader--messages)
        (puthash (alist-get 'id message) t ef-reader--loaded))
      (puthash "reply" '(((name . "notes.txt") (part . 1) (size . 10))) ef-reader--attachments)
      (cl-letf (((symbol-function 'ef-reader--schedule-visible) #'ignore)) (ef-reader--render))
      (should visual-line-mode)
      (goto-char (point-min))
      (should (looking-at "From: "))
      (search-forward "Reply body")
      (should (equal (get-text-property (1- (point)) 'ef-reader-id) "reply"))
      (should (= (length (get-text-property (1- (point)) 'line-prefix)) 6))
      (should (= (length (get-text-property (1- (point)) 'wrap-prefix)) 6))
      (search-forward "Save notes.txt")
      (should (= (length (get-text-property (1- (point)) 'line-prefix)) 6))
      (should (button-at (1- (point))))
      (search-forward "code()")
      (should (memq 'fixed-pitch (ensure-list (get-text-property (1- (point)) 'face))))
      (should (= (length (get-text-property (1- (point)) 'wrap-prefix)) 10))
      (should (= 3 (let ((pos (point-min)) (count 0))
                     (while (< pos (point-max))
                       (when (eq (get-text-property pos 'face) 'ef-reader-separator) (cl-incf count))
                       (setq pos (next-single-property-change pos 'face nil (point-max)))) count))))))

(ert-deftest reader-thread-layout-preserves-anchor-on-new-reply ()
  (let ((ef-reader--states (make-hash-table :test #'equal)))
    (with-temp-buffer
      (ef-reader-article-mode)
      (setq ef-reader--source "discourse:test")
      (dolist (message '(((id . "root") (author . "A") (title . "Topic") (created . 1.25) (body . "Root body"))
                         ((id . "reply") (parent_id . "root") (author . "B") (created . 3) (body . "Reading here"))))
        (puthash (alist-get 'id message) message ef-reader--messages)
        (puthash (alist-get 'id message) t ef-reader--loaded))
      (puthash '("discourse:test" "root") '((read . t)) ef-reader--states)
      (puthash '("discourse:test" "reply") '((read . nil)) ef-reader--states)
      (cl-letf (((symbol-function 'ef-reader--schedule-visible) #'ignore))
        (ef-reader--render)
        (should (equal (get-text-property (point) 'ef-reader-id) "reply"))
        (search-forward "Reading")
        (let ((anchor (ef-reader--anchor (point))))
          (puthash "earlier" '((id . "earlier") (parent_id . "root") (author . "C") (created . 2) (body . "Arrived late")) ef-reader--messages)
          (ef-reader--render)
          (should (equal anchor (ef-reader--anchor (point)))))))))





(ert-deftest reader-ui-install-is-idempotent ()
  (let ((mode (symbol-function 'ef-reader-article-mode)))
    (ef-reader-ui-install)
    (should (eq mode (symbol-function 'ef-reader-article-mode)))))

(ert-deftest reader-home-unknown-time-is-blank ()
  (let ((ef-reader-home--entries nil))
    (should (equal "" (aref (ef-reader-home-row '((occurred . 0.0))) 1)))))

(ert-deftest camulus-has-no-old-request-or-main-command-aliases ()
  (should (fboundp 'camulus))
  (dolist (command '(ef-reader ef-publication-request ef-rss-request ef-mail-request
                    ef-reddit-request ef-discourse-request ef-content-request ef-subscription-request
                    ef-reader--request ef-reader--event ef-reader--load-archive))
    (should-not (fboundp command)))
  (should (string-suffix-p "/camulus/config.json" ef-reader-config-file)))

(defun reader-test-rows (ids)
  (mapcar (lambda (id) `((source . "test") (id . ,id) (author . "A") (title . ,id))) ids))

(ert-deftest reader-summary-removal-selects-next-surviving-row ()
  (with-temp-buffer
    (ef-reader-feed-mode)
    (ef-reader--set-rows (reader-test-rows '("a" "b" "c" "d")))
    (forward-line 1)
    ;; A refresh may insert an item above us while removing the selected item.
    (ef-reader--set-rows (reader-test-rows '("new" "a" "c" "d")))
    (should (equal (alist-get 'id (ef-reader--selected)) "c"))
    (ef-reader--set-rows (reader-test-rows '("new" "a" "d")))
    (should (equal (alist-get 'id (ef-reader--selected)) "d"))
    ;; Removing the last row falls back to the previous surviving row.
    (ef-reader--set-rows (reader-test-rows '("new" "a")))
    (should (equal (alist-get 'id (ef-reader--selected)) "a"))
    (ef-reader--set-rows nil)
    (should (= (point) (point-min)))))

(ert-deftest reader-summary-refresh-keeps-view-and-user-movement ()
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *Reader cursor test*")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (ef-reader-feed-mode)
            (let ((rows (reader-test-rows (mapcar #'number-to-string (number-sequence 1 60)))))
              (ef-reader--set-rows rows)
              (forward-line 30)
              (set-window-start (selected-window) (save-excursion (forward-line -5) (point)))
              (let ((top (tabulated-list-get-id (window-start))))
                ;; The user moves while an asynchronous deletion is in flight.
                (forward-line 1)
                (ef-reader--set-rows (cl-remove "31" rows :key (lambda (r) (alist-get 'id r)) :test #'equal))
                (should (equal (alist-get 'id (ef-reader--selected)) "32"))
                (should (equal (tabulated-list-get-id (window-start)) top)))))
        (kill-buffer buffer)))))

(ert-deftest reader-summary-removal-follows-displayed-sort-order ()
  (with-temp-buffer
    (ef-reader-feed-mode)
    (setq tabulated-list-sort-key '("Thread" . t))
    (ef-reader--set-rows (reader-test-rows '("a" "b" "c" "d")))
    (forward-line 1)
    (should (equal (alist-get 'id (ef-reader--selected)) "c"))
    (ef-reader--set-rows (reader-test-rows '("a" "b" "d")))
    (should (equal (alist-get 'id (ef-reader--selected)) "b"))))

(ert-deftest reader-header-click-does-not-prompt-for-an-event ()
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *Reader header test*")) called)
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (ef-reader-feed-mode)
            (ef-reader-home--header)
            (let* ((label (cl-find-if (lambda (s) (and (stringp s) (equal s "[Discourse]"))) header-line-format))
                   (map (get-text-property 0 'local-map label))
                   (command (lookup-key map [header-line mouse-1]))
                   (last-input-event (list 'mouse-1 (list (selected-window) 'header-line '(0 . 0) 0))))
              (should (equal (interactive-form command) '(interactive "")))
              (cl-letf (((symbol-function 'ef-reader-discourse) (lambda () (interactive) (setq called t)))
                        ((symbol-function 'read-string) (lambda (&rest _) (ert-fail "Unexpected event prompt"))))
                (call-interactively command))
              (should called)))
        (kill-buffer buffer)))))

(ert-run-tests-batch-and-exit)
