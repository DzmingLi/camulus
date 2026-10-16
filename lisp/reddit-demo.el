;;; reddit-demo.el --- Thin interaction harness for the OCaml backend -*- lexical-binding: t; -*-
(require 'effects-probe)
(require 'tabulated-list)
(require 'json)
(require 'browse-url)
(require 'subr-x)

(defvar ef-reddit-username nil)
(defvar ef-reddit-token-file (expand-file-name "~/News/nnreddit/refresh-token"))
(defvar ef-reddit-client-id "5oagOpX2_NKVDej_iuZjFA")
(defvar ef-reddit-redirect-uri "http://127.0.0.1:17973")
(defvar ef-reddit--requests (make-hash-table :test #'eql))
(defvar-local ef-reddit--name nil)
(defvar-local ef-reddit--query nil)
(defvar-local ef-reddit--after nil)
(defvar-local ef-reddit--messages nil)
(defvar-local ef-reddit--active-request nil)
(defvar-local ef-reddit--thread-data nil)

(defun ef-reddit--vote-label (m)
  (let* ((state (ef-reddit-vote-state (alist-get 'id m)))
         (direction (if state (car state) (or (alist-get 'my_vote m) 0))))
    (concat (pcase direction (1 "+") (-1 "-") (_ "·"))
            (pcase (cadr state)
              ('pending " pending") ('rejected " failed") ('uncertain " unknown") (_ "")))))

(defun ef-reddit--redraw-item (id)
  ;; The OCaml overlay is shared across every buffer showing this identity.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (hash-table-p ef-reddit--messages) (gethash id ef-reddit--messages))
        (cond ((derived-mode-p 'ef-reddit-summary-mode) (ef-reddit--render-summary))
              ((derived-mode-p 'ef-reddit-thread-mode)
               (ef-reddit--render-thread ef-reddit--thread-data)))))))

(defun ef-reddit-set-vote (direction)
  "Set the selected post or comment's vote to DIRECTION (1, -1 or 0)."
  (let* ((id (if (derived-mode-p 'ef-reddit-summary-mode)
                 (tabulated-list-get-id) (get-text-property (point) 'ef-reddit-id)))
         (m (and id (hash-table-p ef-reddit--messages) (gethash id ef-reddit--messages))))
    (unless m (user-error "Select a post or comment first"))
    (let ((state (ef-reddit-vote-state id)))
      (ef-reddit-vote id (if state (car state) (or (alist-get 'my_vote m) 0)) direction))
    (ef-reddit--redraw-item id)))

(defun ef-reddit-upvote () (interactive) (ef-reddit-set-vote 1))
(defun ef-reddit-downvote () (interactive) (ef-reddit-set-vote -1))
(defun ef-reddit-clear-vote () (interactive) (ef-reddit-set-vote 0))

(defun ef-reddit--vote-event (event)
  (when (eq (car event) 'reddit-vote)
    (ef-reddit--redraw-item (nth 2 event))
    (unless (string-empty-p (nth 5 event)) (message "%s" (nth 5 event)))))
(add-hook 'ef-probe-event-hook #'ef-reddit--vote-event)

(defun ef-reddit-start ()
  "Start the OCaml Reddit backend using the same app/token format as nnreddit."
  (interactive)
  (when (equal (ef-probe-status) "idle")
    (clrhash ef-reddit--requests)
    (setq ef-reddit-username
          (or ef-reddit-username (and (boundp 'nnreddit-username) nnreddit-username)
              (read-string "Reddit username: ")))
    (ef-reddit-configure ef-reddit-username
                         (if (boundp 'nnreddit-refresh-token-file)
                             nnreddit-refresh-token-file ef-reddit-token-file)
                         (if (boundp 'nnreddit-client-id) nnreddit-client-id ef-reddit-client-id)
                         (if (boundp 'nnreddit-redirect-uri) nnreddit-redirect-uri ef-reddit-redirect-uri))
    (ef-probe-session-start)))

(defun ef-reddit--request (request buffer)
  (let ((id (camulus-reddit-submit request)))
    (puthash id buffer ef-reddit--requests)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (setq ef-reddit--active-request id)))
    id))

(defun ef-reddit--event (event)
  (let* ((id (nth 1 event)) (buffer (and (integerp id) (gethash id ef-reddit--requests))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eql id ef-reddit--active-request)
          (pcase (car event)
            ('reddit
             (let ((data (nth 3 event)))
               (pcase (alist-get 'kind data)
                 ("page"
                  (dolist (m (alist-get 'messages data))
                    (puthash (alist-get 'id m) m ef-reddit--messages))
                  (setq ef-reddit--after (alist-get 'after data))
                  (ef-reddit--render-summary))
                 ("thread"
                  (dolist (m (alist-get 'messages data))
                    (puthash (alist-get 'id m) m ef-reddit--messages))
                  (ef-reddit--render-thread data))
                 ("error" (message "%s" (alist-get 'message data))))))
            ('finished
             (when (eq (nth 3 event) 'failed) (message "Reddit: %s" (nth 4 event))))))))
    (when (eq (car event) 'finished) (remhash id ef-reddit--requests))))

(add-hook 'ef-probe-event-hook #'ef-reddit--event)

(define-derived-mode ef-reddit-summary-mode tabulated-list-mode "Reddit"
  (setq tabulated-list-format [("Vote" 12 nil) ("Score" 6 nil) ("Author" 20 t) ("Title" 80 t)]
        tabulated-list-padding 1
        header-line-format "RET: thread  g: refresh  s: search  n: next page  +: up  -: down  0: clear vote")
  (tabulated-list-init-header))
(define-key ef-reddit-summary-mode-map (kbd "RET") #'ef-reddit-open-thread)
(define-key ef-reddit-summary-mode-map (kbd "g") #'ef-reddit-refresh)
(define-key ef-reddit-summary-mode-map (kbd "s") #'ef-reddit-search)
(define-key ef-reddit-summary-mode-map (kbd "n") #'ef-reddit-next-page)

(defun ef-reddit--render-summary ()
  (setq tabulated-list-entries
        (mapcar (lambda (m) (list (alist-get 'id m)
                                  (vector (ef-reddit--vote-label m)
                                          (number-to-string (or (alist-get 'score m) 0))
                                          (alist-get 'author m) (alist-get 'title m))))
                (sort (hash-table-values ef-reddit--messages)
                      (lambda (a b) (> (alist-get 'created a) (alist-get 'created b))))))
  (tabulated-list-print t))

(defun ef-reddit--load-page (after)
  (when ef-reddit--active-request (ef-probe-cancel ef-reddit--active-request))
  (ef-reddit--request `((op . ,(if ef-reddit--query "search" "subreddit"))
                       (name . ,ef-reddit--name) (query . ,ef-reddit--query) (after . ,after))
                     (current-buffer)))
(defun ef-reddit-refresh ()
  (interactive)
  (setq ef-reddit--messages (make-hash-table :test #'equal) ef-reddit--after nil)
  (ef-reddit--load-page nil))
(defun ef-reddit-next-page ()
  (interactive)
  (if ef-reddit--after (ef-reddit--load-page ef-reddit--after) (message "No further page")))
(defun ef-reddit-search (query)
  (interactive "sSearch this subreddit (empty resets): ")
  (setq ef-reddit--query (unless (string-empty-p query) query))
  (ef-reddit-refresh))

(defun ef-reddit-browse (subreddit)
  "Open a minimal subreddit summary; all API work runs in OCaml."
  (interactive "sSubreddit: ")
  (ef-reddit-start)
  (pop-to-buffer (get-buffer-create (format "*Reddit r/%s*" subreddit)))
  (ef-reddit-summary-mode)
  (setq ef-reddit--name subreddit)
  (ef-reddit-refresh))

(defun ef-reddit--await-result (id)
  (let (result)
    (ef-probe-await
     (lambda ()
       (when-let* ((event (cl-find-if (lambda (e) (and (eq (car e) 'reddit) (= (cadr e) id))) ef-probe--events)))
         (setq result (nth 3 event)))) 60)
    (when (equal (alist-get 'kind result) "error") (user-error "%s" (alist-get 'message result)))
    result))

(defun ef-reddit-authorize ()
  "Authorize the nnreddit app, including vote permission, in a browser."
  (interactive)
  (ef-reddit-start)
  (let* ((result (ef-reddit--await-result (camulus-reddit-submit '((op . "authorize")))))
         (url (alist-get 'url result)))
    (unless url (user-error "No authorization URL returned"))
    (browse-url url)
    (let* ((history-length 0)
           (callback (read-string "Paste the complete Reddit redirect URL: ")))
      (ef-reddit--await-result
       (camulus-reddit-submit `((op . "authorize-complete") (callback . ,callback))))
      (message "Reddit authorization saved"))))

(define-derived-mode ef-reddit-thread-mode special-mode "Reddit thread"
  (when (fboundp 'olivetti-mode) (olivetti-mode 1))
  (add-hook 'kill-buffer-hook
            (lambda () (when (and ef-reddit--active-request
                                  (not (equal (ef-probe-status) "idle")))
                         (ef-probe-cancel ef-reddit--active-request))) nil t))

(dolist (map (list ef-reddit-summary-mode-map ef-reddit-thread-mode-map))
  (define-key map (kbd "+") #'ef-reddit-upvote)
  (define-key map (kbd "-") #'ef-reddit-downvote)
  (define-key map (kbd "0") #'ef-reddit-clear-vote))
(define-key ef-reddit-thread-mode-map (kbd "g") #'ef-reddit-refresh-thread)

(defun ef-reddit-refresh-thread ()
  (interactive)
  (unless ef-reddit--name (user-error "No thread is open"))
  (when ef-reddit--active-request (ef-probe-cancel ef-reddit--active-request))
  (ef-reddit--request `((op . "thread") (id . ,ef-reddit--name)) (current-buffer)))

(defun ef-reddit--anchor (position)
  (when-let* ((id (get-text-property position 'ef-reddit-id))
              (start (or (previous-single-property-change (1+ position) 'ef-reddit-id) (point-min))))
    (cons id (- position start))))
(defun ef-reddit--anchor-position (anchor)
  (when anchor
    (when-let* ((position (text-property-any (point-min) (point-max) 'ef-reddit-id (car anchor))))
      (min (point-max) (+ position (cdr anchor))))))

(defun ef-reddit--render-thread (data)
  (setq ef-reddit--thread-data data)
  (let* ((inhibit-read-only t) (anchor (ef-reddit--anchor (point)))
         (windows (mapcar (lambda (w) (cons w (ef-reddit--anchor (window-start w))))
                          (get-buffer-window-list (current-buffer) nil t)))
         (messages (sort (hash-table-values ef-reddit--messages)
                         (lambda (a b) (if (= (alist-get 'created a) (alist-get 'created b))
                                          (string< (alist-get 'id a) (alist-get 'id b))
                                        (< (alist-get 'created a) (alist-get 'created b))))))
         (seen (make-hash-table :test #'equal)))
    (erase-buffer)
    (cl-labels ((render (m depth)
                  (let ((id (alist-get 'id m)))
                    (unless (gethash id seen)
                      (puthash id t seen)
                      (let ((start (point)) (indent (make-string (* 2 (min depth 12)) ?\s)))
                        (insert indent (alist-get 'author m) "  " id
                                "  [" (ef-reddit--vote-label m) "]\n"
                                (or (alist-get 'title m) "") "\n"
                                (if (alist-get 'unavailable m) "[deleted or removed]" (alist-get 'body m)) "\n")
                        (when-let* ((url (alist-get 'link_url m))) (insert url "\n"))
                        (insert "\n")
                        (add-text-properties start (point) `(ef-reddit-id ,id)))
                      (dolist (child messages)
                        (when (equal (alist-get 'parent_id child) id) (render child (1+ depth))))))))
      (dolist (m messages) (unless (gethash (alist-get 'parent_id m) ef-reddit--messages) (render m 0)))
      (dolist (m messages) (render m 0)))
    (setq header-line-format (format "+: up  -: down  0: clear vote  g: refresh — %d messages — %s"
                                    (hash-table-count ef-reddit--messages)
                                    (if (alist-get 'complete data) "loaded" "loading / partial")))
    (goto-char (or (ef-reddit--anchor-position anchor) (point-min)))
    (dolist (entry windows)
      (when-let* ((position (ef-reddit--anchor-position (cdr entry))))
        (set-window-start (car entry) position t)))))

(defun ef-reddit-open-thread ()
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "Select a post first"))
    (let ((buffer (get-buffer-create "*Reddit thread*")))
      (with-current-buffer buffer
        (when ef-reddit--active-request (ef-probe-cancel ef-reddit--active-request))
        (ef-reddit-thread-mode)
        (setq ef-reddit--messages (make-hash-table :test #'equal)
              ef-reddit--name id)
        (let ((inhibit-read-only t)) (erase-buffer) (insert "Loading thread…"))
        (ef-reddit--request `((op . "thread") (id . ,id)) buffer))
      (display-buffer-in-direction buffer '((direction . right) (window-width . 0.6))))))

(provide 'reddit-demo)
