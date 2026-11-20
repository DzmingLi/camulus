;;; content-cache-integration.el --- Durable reads and offline thread refresh -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'camulus)

(defun camulus-cache-test-await (predicate)
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))
(defun camulus-cache-test-open (entry id)
  (switch-to-buffer (get-buffer-create " *cache-test-list*"))
  (camulus-reader-feed-mode)
  (setq-local camulus-reader--entry entry)
  (camulus-reader--set-rows
   `(((source . ,(alist-get 'source entry)) (id . ,id) (title . "Fixture")
      (author . "Fixture") (unread . t))))
  (goto-char (point-min))
  (camulus-reader-open))
(defun camulus-cache-test-body (id)
  (when-let* ((buffer (get-buffer "*Reader thread*")))
    (with-current-buffer buffer
      (let* ((message (gethash id camulus-reader--messages))
             (body (alist-get 'body message)))
        (or (and body (not (string-empty-p body)) body)
            (alist-get 'html message))))))
(defun camulus-cache-test-idle ()
  (and (= 0 (hash-table-count camulus-reader--loads))
       (= 0 (hash-table-count camulus-reader--requests))))

(ert-deftest ordinary-body-survives-worker-restart-without-network-or-favorite ()
  (let* ((directory (make-temp-file "camulus-body-cache" t))
         (camulus-state-file (expand-file-name "state.sqlite3" directory))
         (requests 0)
         (server
          (make-network-process
           :name "camulus-cache-http" :server t :host "127.0.0.1" :service t :noquery t
           :filter
           (lambda (process bytes)
             (let ((request (concat (or (process-get process 'request) "") bytes)))
               (process-put process 'request request)
               (when (and (string-match-p "\r\n\r\n" request)
                          (not (process-get process 'sent)))
                 (process-put process 'sent t)
                 (cl-incf requests)
                 (let ((body "<?xml version=\"1.0\"?><rss version=\"2.0\"><channel><title>Fixture</title><link>https://example.test/</link><description>Fixture</description><item><guid isPermaLink=\"false\">one</guid><title>Saved article</title><link>https://example.test/one</link><description>Persistent body fixture</description></item></channel></rss>"))
                   (process-send-string process (format "HTTP/1.1 200 OK\r\nContent-Type: application/rss+xml\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s" (string-bytes body) body))
                   (process-send-eof process)))))))
         (url (format "http://127.0.0.1:%d/feed" (process-contact server :service)))
         (entry `((source . ,(concat "rss:" url)) (group . ,url))))
    (unwind-protect
        (progn
          (camulus-reader-start)
          (camulus-cache-test-open entry "one")
          (camulus-cache-test-await (lambda () (camulus-cache-test-body "one")))
          (camulus-cache-test-await #'camulus-cache-test-idle)
          (should (= requests 1))
          (camulus-probe-session-stop)
          (delete-process server)
          (camulus-reader-start)
          (camulus-cache-test-open entry "one")
          (camulus-cache-test-await (lambda () (camulus-cache-test-body "one")))
          (camulus-cache-test-await #'camulus-cache-test-idle)
          (should (string-match-p "Persistent body fixture" (camulus-cache-test-body "one")))
          (should (= requests 1))
          (let ((db (sqlite-open camulus-state-file)))
            (unwind-protect
                (progn
                  (should (equal (sqlite-select db "SELECT count(*) FROM favorites") '((0))))
                  (should (equal (sqlite-select db "SELECT resources_complete FROM archives") '((0)))))
              (sqlite-close db))))
      (camulus-probe-session-stop)
      (when (process-live-p server) (delete-process server))
      (delete-directory directory t))))

(ert-deftest thread-headers-preserve-already-cached-full-body ()
  (with-temp-buffer
    (camulus-reader-article-mode)
    (setq-local camulus-reader--source "discourse:https://example.test"
                camulus-reader--active 1)
    (puthash "one" '((id . "one") (body . "Cached full body")
                     (html . "<p>Cached full body</p>")) camulus-reader--messages)
    (puthash "one" t camulus-reader--loaded)
    (cl-letf (((symbol-function 'camulus-reader--render) #'ignore))
      (camulus-reader--receive
       '((kind . "thread") (messages . (((id . "one") (body . "") (title . "New header"))))
         (complete . nil))))
    (should (equal (alist-get 'body (gethash "one" camulus-reader--messages)) "Cached full body"))
    (should (equal (alist-get 'html (gethash "one" camulus-reader--messages)) "<p>Cached full body</p>"))))

(ert-deftest cached-thread-remains-visible-when-refresh-fails ()
  (let* ((directory (make-temp-file "camulus-thread-cache" t))
         (camulus-state-file (expand-file-name "state.sqlite3" directory))
         (source "discourse:https://127.0.0.1:1")
         (entry `((source . ,source) (group . "latest")))
         failed)
    (unwind-protect
        (progn
          ;; An empty home initializes the normal state database, without
          ;; fetching any source or creating a favorite.
          (camulus)
          (camulus-cache-test-await #'camulus-cache-test-idle)
          (let ((db (sqlite-open camulus-state-file)))
            (unwind-protect
                (progn
                  (sqlite-execute db "INSERT INTO items(source,id,thread,title,author,url,created) VALUES(?,?,?,?,?,?,?)"
                                  (list source "post:2" "topic:1" "Saved reply" "Fixture" "https://example.test/" 1.0))
                  (sqlite-execute db "INSERT INTO archives(source,id,payload,resources_complete) VALUES(?,?,?,0)"
                                  (list source "post:2"
                                        (json-encode '((message . ((id . "post:2") (thread_id . "topic:1")
                                                                   (title . "Saved reply") (author . "Fixture")
                                                                   (body . "Saved thread body") (created . 1.0)))
                                                       (attachments . [])))))
                  (sqlite-execute db "INSERT INTO cached_threads VALUES(?,?,1)" (list source "topic:1")))
              (sqlite-close db)))
          (let ((listener (lambda (event)
                            (when (and (eq (car event) 'finished) (eq (nth 2 event) 'failed))
                              (setq failed t)))))
            (add-hook 'camulus-probe-event-hook listener)
            (unwind-protect
                (progn
                  (camulus-cache-test-open entry "topic:1")
                  (camulus-cache-test-await (lambda () (camulus-cache-test-body "post:2")))
                  (camulus-cache-test-await (lambda () failed))
                  (camulus-cache-test-await #'camulus-cache-test-idle)
                  (should (equal (camulus-cache-test-body "post:2") "Saved thread body")))
              (remove-hook 'camulus-probe-event-hook listener))))
      (camulus-probe-session-stop)
      (delete-directory directory t))))

(ert-deftest remote-refresh-merges-and-retains-absent-rows ()
  (let* ((directory (make-temp-file "camulus-merge-feed" t))
         (camulus-state-file (expand-file-name "state.sqlite3" directory))
         (items '(("older" . "Older article") ("repeat" . "Initial title")))
         (files-dir (concat camulus-state-file ".rss"))
         (server
          (make-network-process
           :name "camulus-merge-http" :server t :host "127.0.0.1" :service t :noquery t
           :filter
           (lambda (process bytes)
             (let ((request (concat (process-get process 'request) bytes)))
               (process-put process 'request request)
               (when (and (not (process-get process 'sent)) (string-match-p "\r\n\r\n" request))
                 (process-put process 'sent t)
                 (let ((body
                        (concat "<rss version=\"2.0\"><channel><title>Fixture</title><link>https://example.test/</link><description>Feed</description>"
                                (mapconcat (lambda (item)
                                             (format "<item><guid isPermaLink=\"false\">%s</guid><title>%s</title><description>%s body</description></item>"
                                                     (car item) (cdr item) (cdr item)))
                                           items "")
                                "</channel></rss>")))
                   (process-send-string process
                    (format "HTTP/1.1 200 OK\r\nContent-Type: application/rss+xml\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                            (string-bytes body) body))
                   (process-send-eof process)))))))
         (url (format "http://127.0.0.1:%d/feed" (process-contact server :service)))
         (source (concat "rss:" url))
         (entry `((name . "Fixture") (source . ,source) (group . ,url)))
         (camulus-rss-subscriptions (list url)))
    (unwind-protect
        (progn
          (camulus entry)
          (camulus-cache-test-await #'camulus-cache-test-idle)
          (camulus-reader--load-list t)
          (camulus-cache-test-await #'camulus-cache-test-idle)
          (should (= 2 (length camulus-reader--all-rows)))
          (let* ((article-path (lambda (id)
                                 (expand-file-name
                                  (concat (md5 (json-encode (vector source id))) ".json")
                                  files-dir)))
                 (repeat (funcall article-path "repeat"))
                 (older (funcall article-path "older")))
            (should (= 2 (length (directory-files files-dir nil "\\.json\\'"))))
            (should (file-exists-p repeat))
            (should (file-exists-p older))
            ;; Bookmark and mark read before refreshing the same GUID.
            (let ((db (sqlite-open camulus-state-file)))
              (unwind-protect
                  (sqlite-execute db "UPDATE items SET is_read=1 WHERE source=? AND id='repeat'" (list source))
                (sqlite-close db)))
            (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "saved")))
              (goto-char (point-min))
              (while (not (equal (cadr (tabulated-list-get-id)) "repeat"))
                (should (= 0 (forward-line 1))))
              (camulus-reader-save)
              (camulus-cache-test-await #'camulus-cache-test-idle))
            (setq items '(("recent" . "Recent article") ("repeat" . "Updated title")))
            (camulus-reader--load-list t)
            (camulus-cache-test-await #'camulus-cache-test-idle)
            (let* ((rows camulus-reader--all-rows)
                   (row (cl-find "repeat" rows :test #'equal :key (lambda (row) (alist-get 'id row)))))
              (should (equal (sort (mapcar (lambda (row) (alist-get 'id row)) rows) #'string<)
                             '("older" "recent" "repeat")))
              (should (equal "Updated title" (alist-get 'title row)))
              (should-not (alist-get 'unread row))
              (should (alist-get 'favorite row)))
            (should (= 3 (length (directory-files files-dir nil "\\.json\\'"))))
            (with-temp-buffer
              (insert-file-contents repeat)
              (should (search-forward "Updated title body" nil t)))
            (let ((modified (file-attribute-modification-time (file-attributes repeat))))
              (camulus-reader--load-list t)
              (camulus-cache-test-await #'camulus-cache-test-idle)
              (should (equal modified (file-attribute-modification-time (file-attributes repeat)))))
            ;; Group dismissal leaves the local copy and home membership.
            (goto-char (point-min))
            (while (not (equal (cadr (tabulated-list-get-id)) "older"))
              (should (= 0 (forward-line 1))))
            (camulus-reader-delete)
            (camulus-cache-test-await #'camulus-cache-test-idle)
            (should (file-exists-p older))
            (camulus)
            (camulus-cache-test-await #'camulus-cache-test-idle)
            (should (cl-find "older" camulus-reader--all-rows :test #'equal :key
                             (lambda (row) (alist-get 'id row))))
            (goto-char (point-min))
            (while (not (equal (cadr (tabulated-list-get-id)) "older"))
              (should (= 0 (forward-line 1))))
            (camulus-reader-delete)
            (camulus-cache-test-await #'camulus-cache-test-idle)
            (should-not (file-exists-p older))
            (setq items '(("older" . "Revised after deletion") ("repeat" . "Updated title")))
            (camulus-reader--load-list t)
            (camulus-cache-test-await #'camulus-cache-test-idle)
            (should-not (file-exists-p older))
            (let ((db (sqlite-open camulus-state-file)))
              (unwind-protect
                  (progn
                    (should (equal (sqlite-select db "SELECT id FROM rss_deletions") '(("older"))))
                    (should (equal (sqlite-select db "SELECT count(*) FROM items WHERE id='older'") '((0))))
                    (should (equal (sqlite-select db "SELECT count(*) FROM archives WHERE length(payload)=32") '((2)))))
                (sqlite-close db))))
          (camulus-probe-session-stop)
          (delete-process server)
          (camulus-reader-start)
          (camulus entry)
          (camulus-reader--load-list nil)
          (camulus-cache-test-await #'camulus-cache-test-idle)
          (should (= 2 (length camulus-reader--all-rows)))
          (camulus-cache-test-open entry "repeat")
          (camulus-cache-test-await (lambda () (equal (camulus-cache-test-body "repeat") "Updated title body"))))
      (camulus-probe-session-stop)
      (when (process-live-p server) (delete-process server))
      (delete-directory directory t))))

(ert-run-tests-batch-and-exit)
