;;; reader-presentation.el --- Thread layout and window behavior -*- lexical-binding: t; -*-
(require 'ert)
(require 'camulus)
(setq ef-probe-record-events t)

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
  (should (string-suffix-p "/camulus/entries.json" ef-reader-config-file)))

(ert-run-tests-batch-and-exit)
