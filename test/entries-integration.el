;;; entries-integration.el --- Entries and optional backend subscriptions -*- lexical-binding: t; -*-
(require 'ert)
(require 'camulus)
(setq camulus-probe-record-events t)

(ert-deftest camulus-options-and-commands-use-package-namespace ()
  (dolist (option '(camulus-groups camulus-firefox-profile camulus-mail-accounts
                    camulus-discourse-auth-sources camulus-reddit-username))
    (should (custom-variable-p option)))
  (dolist (command '(camulus-reader-browse-groups camulus-mail-compose
                     camulus-reader-discourse-like camulus-reddit-upvote))
    (should (commandp command)))
  ;; Old entry points must not survive as compatibility aliases or variables.
  (let (obsolete)
    (mapatoms (lambda (symbol)
                (when (and (string-prefix-p "ef-" (symbol-name symbol))
                           (or (boundp symbol) (fboundp symbol)))
                  (push symbol obsolete))))
    (should-not obsolete)))

(ert-deftest entries-config-is-data-and-does-not-subscribe ()
  (let* ((directory (make-temp-file "reader-entries" t))
         (camulus-reader-config-file (expand-file-name "config.json" directory))
         (camulus-reader-groups-file (expand-file-name "groups.json" directory))

         (camulus-groups nil) (camulus-discourse-sites nil))
    (unwind-protect
        (progn
          (with-temp-file camulus-reader-groups-file
            (insert "{\"version\":1,\"groups\":[{\"name\":\"Forum\",\"source\":\"discourse:https://example.test\",\"group\":\"watching\"},{\"name\":\"Browse\",\"source\":\"reddit:alice\",\"group\":\"ocaml\"}]}"))
          (cl-letf (((symbol-function 'camulus-subscription-submit) (lambda (&rest _) (ert-fail "Configuration caused a subscription"))))
            (camulus-import-json-config camulus-reader-config-file camulus-reader-groups-file))
          (should (= 2 (length camulus-groups)))
          (should-not (camulus-reader-home--merge-entries nil))
          (should (equal camulus-discourse-sites '("https://example.test")))
          (let ((previous camulus-groups))
            (with-temp-file camulus-reader-groups-file
              (insert "{\"version\":1,\"groups\":[{\"name\":\"Invalid\"}]}"))
            (should-error (camulus-import-json-config camulus-reader-config-file camulus-reader-groups-file) :type 'user-error)
            (should (equal previous camulus-groups))))
      (delete-directory directory t))))

(defun camulus-entries-test-await (id)
  (camulus-probe-await (lambda () (cl-find-if (lambda (e) (and (eq (car e) 'finished) (= id (cadr e)))) camulus-probe--events)) 10)
  (let ((event (cl-find-if (lambda (e) (and (eq (car e) 'finished) (= id (cadr e)))) camulus-probe--events)))
    (should (eq (nth 3 event) 'completed)))
  (let ((event (cl-find-if (lambda (e) (and (eq (car e) 'content-state) (= id (cadr e)))) camulus-probe--events)))
    (nth 3 event)))

(ert-deftest entries-local-subscriptions-are-custom-options ()
  (let* ((directory (make-temp-file "custom-subscriptions" t))
         (custom-file (expand-file-name "custom.el" directory))
         (user-init-file (expand-file-name "init.el" directory))
         (previous (default-value 'camulus-rss-subscriptions)))
    (unwind-protect
        (progn
          (dolist (option '(camulus-rss-subscriptions camulus-jsonfeed-subscriptions
                            camulus-mu-subscriptions camulus-reddit-subscriptions
                            camulus-theatlantic-subscriptions camulus-janestreet-subscriptions
                            camulus-blogger-subscriptions camulus-douban-subscriptions
                            camulus-zhihu-subscriptions camulus-netease-subscriptions
                            camulus-public-inbox-subscriptions))
            (should (custom-variable-p option)))
          (dolist (command '(camulus-group-subscriptions-configure camulus-rss-subscriptions-configure
                             camulus-jsonfeed-subscriptions-configure camulus-group-subscription-submit
                             camulus-group-followed camulus-rss-followed camulus-jsonfeed-followed camulus-gcores camulus-reader-search
                             camulus-reader-rss-search camulus-reader-discourse-search))
            (should-not (fboundp command)))
          (customize-save-variable 'camulus-rss-subscriptions '("https://example.test/feed"))
          (set-default 'camulus-rss-subscriptions nil)
          (load custom-file nil t)
          (should (equal (default-value 'camulus-rss-subscriptions) '("https://example.test/feed")))
          (should-not (directory-files directory nil "\\.json$")))
      (set-default 'camulus-rss-subscriptions previous)
      (delete-directory directory t))))

(ert-deftest entries-backend-settings-validated-before-application ()
  (let ((camulus-reader-config-file (make-temp-file "backend-settings"))
        (camulus-reader-groups-file "/nonexistent-camulus-groups.json")
        (camulus-mail-sender "before") (camulus-mail-accounts nil)
        (camulus-groups nil) (camulus-discourse-sites nil)
        (camulus-firefox-profile nil))
    (unwind-protect
        (progn
          (with-temp-file camulus-reader-config-file
            (insert "{\"version\":1,\"backend_settings\":{\"mu\":{\"sender\":\"after\",\"accounts\":[[\"/a\",\"a@test\",\"a\"]]},\"browser\":{\"firefox_profile\":\"/profile\"}}}"))
          (camulus-import-json-config camulus-reader-config-file camulus-reader-groups-file)
          (should (equal camulus-mail-sender "after"))
          (should (equal camulus-mail-accounts '(("/a" "a@test" "a"))))
          (should (equal camulus-firefox-profile "/profile"))
          (with-temp-file camulus-reader-config-file
            (insert "{\"version\":1,\"backend_settings\":{\"mu\":{\"sender\":\"bad\",\"smtp_profiles\":[[\"a\",\"smtp.test\",70000,\"a\",\"/secret-file\",\"\"]]}}}"))
          (should-error (camulus-import-json-config camulus-reader-config-file camulus-reader-groups-file) :type 'user-error)
          (should (equal camulus-mail-sender "after"))
          (dolist (settings '("[]" "{\"mu\":[]}" "{\"mu\":{\"sender\":null}}"))
            (with-temp-file camulus-reader-config-file
              (insert "{\"version\":1,\"backend_settings\":" settings "}"))
            (should-error (camulus-import-json-config camulus-reader-config-file camulus-reader-groups-file) :type 'user-error)
            (should (equal camulus-mail-sender "after"))))
      (delete-file camulus-reader-config-file))))

(ert-deftest entries-publication-sources-use-article-target-except-douban ()
  (dolist (command '(camulus-zhihu-login camulus-zhihu-logout camulus-zhihu-login-code camulus-zhihu-login-cancel))
    (should-not (fboundp command)))
  (should-not (boundp 'camulus-zhihu-session-file))
  (dolist (source '("atlantic" "janestreet" "zhihu" "douban" "netease" "public-inbox" "blogger"))
    (with-temp-buffer
      (camulus-reader-feed-mode)
      (camulus-reader--set-rows (list `((source . ,source) (id . "one") (title . "Test") (author . "A"))))
      (goto-char (point-min))
      (should (equal (alist-get 'target (camulus-reader--selected))
                     (if (member source '("douban" "public-inbox")) nil "article"))))))






(ert-deftest home-membership-is-entirely-backend-owned ()
  (let* ((rss '((name . "Blog") (source . "rss:url") (group . "url")))
         (personal '((name . "Replies") (source . "forum") (group . "replies")))
         (camulus-groups (list rss personal '((name . "Browse") (source . "forum") (group . "latest")))))
    (should-not (camulus-reader-home--merge-entries nil))
    (should (= 2 (length (camulus-reader-home--merge-entries (list personal rss rss)))))
    (should (equal (alist-get 'name (car (camulus-reader-home--merge-entries (list rss)))) "Blog"))))

(ert-deftest home-group-directory-browses-without-following ()
  (let* ((entry '((name . "Unfollowed group") (source . "reddit:alice") (group . "ocaml")))
         (camulus-groups (list entry))
         (camulus-discourse-sites '("https://example.test")) opened refreshed)
    (unwind-protect
        (cl-letf (((symbol-function 'camulus-reader-load-config) #'ignore)
                  ((symbol-function 'camulus) (lambda (value) (setq opened value)))
                  ((symbol-function 'camulus-reader--load-list) (lambda (sync) (setq refreshed sync)))
                  ((symbol-function 'camulus-subscription-submit)
                   (lambda (&rest _) (ert-fail "Browsing caused a subscription"))))
          (camulus-reader-browse-groups)
          (goto-char (point-min))
          (search-forward "Unfollowed group")
          (button-activate (button-at (1- (point))))
          (should (equal opened entry))
          (should refreshed)
          (should-not (camulus-reader-home--merge-entries nil))
          (should (equal camulus-groups (list entry))))
      (when (get-buffer "*Reader groups*") (kill-buffer "*Reader groups*")))))

(ert-deftest home-backend-buttons-open-filtered-registered-groups ()
  (let* ((directory (make-temp-file "reader-backend-groups" t))
         (camulus-reader-config-file (expand-file-name "config.json" directory))
         (camulus-reader-groups-file (expand-file-name "groups.json" directory))
         (camulus-state-file (expand-file-name "state.sqlite3" directory))

         (camulus-groups nil) (camulus-discourse-sites nil) opened)
    (unwind-protect
        (progn
          (with-temp-file camulus-reader-groups-file
            (insert "{\"version\":1,\"groups\":[{\"name\":\"NixOS Forum\",\"source\":\"discourse:https://nixos.test\",\"group\":\"latest\"},{\"name\":\"Emacs Forum\",\"source\":\"discourse:https://emacs.test\",\"group\":\"latest\"},{\"name\":\"OCaml Subreddit\",\"source\":\"reddit:alice\",\"group\":\"ocaml\"}]}"))
          (camulus-import-json-config camulus-reader-config-file camulus-reader-groups-file)
          (camulus)
          (let ((home (current-buffer)))
            (dolist (case '(("[Discourse]" "NixOS Forum" "Emacs Forum" "OCaml Subreddit")
                            ("[Reddit]" "OCaml Subreddit" nil "NixOS Forum")))
              (switch-to-buffer home)
              (pcase-let* ((`(,label ,first ,second ,excluded) case)
                           (link (seq-find (lambda (part) (equal part label)) header-line-format))
                           (action (lookup-key (get-text-property 0 'local-map link) [header-line mouse-1]))
                           (last-input-event (list 'mouse-1 (list (selected-window) 'header-line))))
                (call-interactively action)
                (should (eq major-mode 'camulus-reader-groups-mode))
                (should (equal first (alist-get 'name (get-text-property (point) 'camulus-reader-entry))))
                (when second (should (string-match-p second (buffer-string))))
                (should-not (string-match-p excluded (buffer-string)))
                (call-interactively (key-binding (kbd "g")))
                (should-not (string-match-p excluded (buffer-string)))
                (cl-letf (((symbol-function 'camulus) (lambda (entry) (setq opened entry)))
                          ((symbol-function 'camulus-reader--load-list) #'ignore))
                  (call-interactively (key-binding (kbd "RET"))))
                (should (equal first (alist-get 'name opened)))))))
      (dolist (name '("*Camulus groups: Discourse*" "*Camulus groups: Reddit*" "*Reader*"))
        (when (get-buffer name) (kill-buffer name)))
      (camulus-probe-session-stop)
      (delete-directory directory t))))

(ert-deftest group-subscriptions-use-current-options-independent-of-registration ()
  (let ((camulus-janestreet-subscriptions nil)
        (camulus-groups '(((name . "Jane Street") (source . "janestreet") (group . "posts"))))
        (camulus-mail-sender nil) (camulus-reddit-username nil))
    (unwind-protect
        (progn
          (camulus-content-state-configure "")
          (camulus-subscriptions)
          (should-not camulus-reader-subscriptions--entries)
          (setopt camulus-janestreet-subscriptions '("posts"))
          (camulus-subscriptions-refresh)
          (should (equal (mapcar (lambda (e) (alist-get 'group e)) camulus-reader-subscriptions--entries) '("posts")))
          (setopt camulus-janestreet-subscriptions nil)
          (camulus-subscriptions-refresh)
          (should-not camulus-reader-subscriptions--entries)
          (should (= 1 (length camulus-groups))))
      (when (get-buffer "*Camulus subscriptions*") (kill-buffer "*Camulus subscriptions*"))
      (camulus-probe-session-stop))))

(ert-deftest subscriptions-view-reads-options-and-opens-backend-customization ()
  (let* ((entry '((name . "Jane Street") (source . "janestreet") (group . "posts")))
         (camulus-groups (list entry '((name . "Browse only") (source . "zhihu") (group . "answers.test"))))
         (camulus-janestreet-subscriptions '("posts"))
         (camulus-rss-subscriptions '("https://example.test/feed"))
         (camulus-mail-sender nil) (camulus-reddit-username nil))
    (unwind-protect
        (save-window-excursion
          (camulus-content-state-configure "")
          (camulus-subscriptions) (camulus-subscriptions)
          (should (= 0 camulus-reader-subscriptions--pending))
          (should-not camulus-reader-subscriptions--errors)
          (should (string-match-p "example.test/feed" (buffer-string)))
          (should-not (string-match-p "Browse only" (buffer-string)))
          (goto-char (point-min)) (search-forward "Jane Street")
          (cl-letf (((symbol-function 'customize-variable)
                     (lambda (option &rest _)
                       (should (eq option 'camulus-janestreet-subscriptions))
                       (setopt camulus-janestreet-subscriptions nil))))
            (call-interactively (key-binding (kbd "s"))))
          (camulus-subscriptions-refresh)
          (should-not (string-match-p "Jane Street" (buffer-string)))
          (should (equal (car camulus-groups) entry))
          ;; Invalid option values are surfaced as provider errors.
          (setq camulus-janestreet-subscriptions '(nil))
          (camulus-subscriptions-refresh)
          (should camulus-reader-subscriptions--errors)
          (should (string-match-p "could not be read" (buffer-string)))
          (should-not (string-match-p "No subscriptions" (buffer-string))))
      (when (get-buffer "*Camulus subscriptions*") (kill-buffer "*Camulus subscriptions*"))
      (camulus-probe-session-stop))))

(ert-run-tests-batch-and-exit)
