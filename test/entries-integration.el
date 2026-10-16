;;; entries-integration.el --- Entries and optional backend subscriptions -*- lexical-binding: t; -*-
(require 'ert)
(require 'camulus)
(setq ef-probe-record-events t)

(ert-deftest entries-config-is-data-and-does-not-subscribe ()
  (let* ((directory (make-temp-file "reader-entries" t))
         (ef-reader-config-file (expand-file-name "entries.json" directory))
         (ef-reader-entries nil) (ef-reader-home nil) (ef-discourse-sites nil))
    (unwind-protect
        (progn
          (with-temp-file ef-reader-config-file
            (insert "{\"version\":1,\"home\":\"Forum\",\"entries\":[{\"name\":\"Forum\",\"source\":\"discourse:https://example.test\",\"group\":\"watching\"},{\"name\":\"Browse\",\"source\":\"reddit:alice\",\"group\":\"ocaml\"}]}"))
          (cl-letf (((symbol-function 'camulus-subscription-submit) (lambda (&rest _) (ert-fail "Configuration caused a subscription"))))
            (ef-reader-load-config))
          (should (equal ef-reader-home "Forum"))
          (should (= 2 (length ef-reader-entries)))
          (should (equal ef-discourse-sites '("https://example.test")))
          (let ((previous ef-reader-entries))
            (with-temp-file ef-reader-config-file
              (insert "{\"version\":1,\"home\":\"missing\",\"entries\":[]}"))
            (should-error (ef-reader-load-config) :type 'user-error)
            (should (equal previous ef-reader-entries))))
      (delete-directory directory t))))

(defun ef-entries-test-await (id)
  (ef-probe-await (lambda () (cl-find-if (lambda (e) (and (eq (car e) 'finished) (= id (cadr e)))) ef-probe--events)) 10)
  (let ((event (cl-find-if (lambda (e) (and (eq (car e) 'finished) (= id (cadr e)))) ef-probe--events)))
    (should (eq (nth 3 event) 'completed)))
  (let ((event (cl-find-if (lambda (e) (and (eq (car e) 'content-state) (= id (cadr e)))) ef-probe--events)))
    (nth 3 event)))

(ert-deftest entries-rss-subscription-without-content-database ()
  (let* ((directory (make-temp-file "reader-subscriptions" t))
         (file (expand-file-name "rss.json" directory))
         (request '((source . "rss:https://example.test/feed") (target . "https://example.test/feed"))))
    (unwind-protect
        (progn
          (ef-content-state-configure "")
          (ef-rss-subscriptions-configure file)
          (ef-probe-session-start)
          (should (equal "unsubscribed" (alist-get 'setting (ef-entries-test-await (camulus-subscription-submit request)))))
          (ef-entries-test-await (camulus-subscription-submit (cons '(setting . "subscribed") request)))
          (should (= 1 (length (alist-get 'rows (ef-entries-test-await (ef-rss-followed))))))
          (ef-probe-session-stop) (ef-probe-session-start)
          (should (equal "subscribed" (alist-get 'setting (ef-entries-test-await (camulus-subscription-submit request)))))
          ;; The backend rereads its authority; external changes are visible.
          (with-temp-file file (insert "[]"))
          (should (equal "unsubscribed" (alist-get 'setting (ef-entries-test-await (camulus-subscription-submit request)))))
          (should-not (directory-files directory nil "sqlite")))
      (ef-probe-session-stop)
      (delete-directory directory t))))



(ert-deftest entries-backend-settings-validated-before-application ()
  (let ((ef-reader-config-file (make-temp-file "backend-settings"))
        (ef-mail-sender "before") (ef-mail-accounts nil)
        (ef-reader-entries nil) (ef-reader-home nil) (ef-discourse-sites nil)
        (ef-reader-firefox-profile nil))
    (unwind-protect
        (progn
          (with-temp-file ef-reader-config-file
            (insert "{\"version\":1,\"entries\":[],\"backend_settings\":{\"maildir\":{\"sender\":\"after\",\"accounts\":[[\"/a\",\"a@test\",\"a\"]]},\"browser\":{\"firefox_profile\":\"/profile\"}}}"))
          (ef-reader-load-config)
          (should (equal ef-mail-sender "after"))
          (should (equal ef-mail-accounts '(("/a" "a@test" "a"))))
          (should (equal ef-reader-firefox-profile "/profile"))
          (with-temp-file ef-reader-config-file
            (insert "{\"version\":1,\"entries\":[],\"backend_settings\":{\"maildir\":{\"sender\":\"bad\",\"smtp_profiles\":[[\"a\",\"smtp.test\",70000,\"a\",\"/secret-file\",\"\"]]}}}"))
          (should-error (ef-reader-load-config) :type 'user-error)
          (should (equal ef-mail-sender "after"))
          (dolist (settings '("[]" "{\"maildir\":[]}" "{\"maildir\":{\"sender\":null}}"))
            (with-temp-file ef-reader-config-file
              (insert "{\"version\":1,\"entries\":[],\"backend_settings\":" settings "}"))
            (should-error (ef-reader-load-config) :type 'user-error)
            (should (equal ef-mail-sender "after"))))
      (delete-file ef-reader-config-file))))

(ert-deftest entries-publication-sources-use-article-target-except-douban ()
  (dolist (source '("atlantic" "janestreet" "zhihu" "douban" "netease" "public-inbox" "blogger" "gcores"))
    (with-temp-buffer
      (ef-reader-feed-mode)
      (ef-reader--set-rows (list `((source . ,source) (id . "one") (title . "Test") (author . "A"))))
      (goto-char (point-min))
      (should (equal (alist-get 'target (ef-reader--selected))
                     (if (member source '("douban" "public-inbox")) nil "article"))))))






(ert-deftest home-membership-is-explicit-and-rss-is-backend-owned ()
  (let* ((rss '((name . "Blog") (source . "rss:url") (group . "url") (timeline . t)))
         (personal '((name . "Replies") (source . "forum") (group . "replies") (timeline . t)))
         (ef-reader-entries (list rss personal '((name . "Browse") (source . "forum") (group . "latest")))))
    (should (equal (ef-reader-home--merge-entries nil) (list personal)))
    (should (= 2 (length (ef-reader-home--merge-entries (list rss rss)))))
    (should (equal (alist-get 'name (car (ef-reader-home--merge-entries (list rss)))) "Blog"))))

(ert-deftest home-group-directory-browses-without-following ()
  (let* ((entry '((name . "Unfollowed group") (source . "reddit:alice") (group . "ocaml")))
         (ef-reader-entries (list entry))
         (ef-discourse-sites '("https://example.test")) opened refreshed)
    (unwind-protect
        (cl-letf (((symbol-function 'ef-reader-load-config) #'ignore)
                  ((symbol-function 'camulus) (lambda (value) (setq opened value)))
                  ((symbol-function 'ef-reader--load-list) (lambda (sync) (setq refreshed sync)))
                  ((symbol-function 'camulus-subscription-submit)
                   (lambda (&rest _) (ert-fail "Browsing caused a subscription"))))
          (ef-reader-browse-groups)
          (goto-char (point-min))
          (search-forward "Unfollowed group")
          (button-activate (button-at (1- (point))))
          (should (equal opened entry))
          (should refreshed)
          (should-not (ef-reader-home--merge-entries nil))
          (should (equal ef-reader-entries (list entry))))
      (when (get-buffer "*Reader groups*") (kill-buffer "*Reader groups*")))))

(ert-run-tests-batch-and-exit)
