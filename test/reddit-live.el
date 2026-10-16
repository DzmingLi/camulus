;;; reddit-live.el --- Opt-in, read-only end-to-end check -*- lexical-binding: t; -*-
;; Requires explicit REDDIT_USERNAME and REDDIT_TOKEN_FILE environment variables.
;; No posting, voting, authorization replacement or notification read marks.
(require 'reddit-demo)
(setq ef-reddit-username (getenv "REDDIT_USERNAME")
      ef-reddit-token-file (getenv "REDDIT_TOKEN_FILE"))
(unless (and ef-reddit-username ef-reddit-token-file) (error "Reddit test credentials not configured"))
(defun ef-reddit-live-finished (id)
  (cl-find-if (lambda (e) (and (eq (car e) 'finished) (= (cadr e) id))) ef-probe--events))
(defun ef-reddit-live-await (id)
  (ef-probe-await (lambda () (ef-reddit-live-finished id)) 60)
  (unless (eq (nth 3 (ef-reddit-live-finished id)) 'completed)
    (error "%s" (nth 4 (ef-reddit-live-finished id)))))
(let* ((beats 0) (gc-count 0)
       (timer (run-at-time 0.02 0.02
                           (lambda ()
                             (cl-incf beats)
                             (when (< gc-count 4) (cl-incf gc-count) (ef-probe-gc) (garbage-collect))))))
  (unwind-protect
      (progn
        (ef-reddit-browse "ocaml")
        (ef-reddit-live-await ef-reddit--active-request)
        (unless (> (hash-table-count ef-reddit--messages) 0) (error "Empty live subreddit"))
        (message "PASS: native Reddit subreddit, %d posts" (hash-table-count ef-reddit--messages))
        (goto-char (point-min))
        (ef-reddit-open-thread)
        (with-current-buffer "*Reddit thread*"
          (ef-reddit-live-await ef-reddit--active-request)
          (unless (> (hash-table-count ef-reddit--messages) 0) (error "Empty live thread"))
          (message "PASS: native Reddit reply tree, %d messages" (hash-table-count ef-reddit--messages)))
        (ef-reddit-search "effects")
        (ef-reddit-live-await ef-reddit--active-request)
        (message "PASS: native Reddit scoped search, %d posts" (hash-table-count ef-reddit--messages))
        (unless (> beats 0) (error "Emacs heartbeat did not run"))
        (unless (ef-probe-signals-unchanged) (error "Reddit worker changed host signal handlers"))
        (message "PASS: native HTTP + UI heartbeat (%d ticks), GC and host signals" beats))
    (cancel-timer timer)
    (ef-probe-session-stop)))
