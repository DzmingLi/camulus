;;; reddit-live.el --- Opt-in, read-only end-to-end check -*- lexical-binding: t; -*-
;; Requires explicit REDDIT_USERNAME and REDDIT_TOKEN_FILE environment variables.
;; No posting, voting, authorization replacement or notification read marks.
(require 'camulus)
(setq camulus-probe-record-events t)
(setq camulus-reddit-username (getenv "REDDIT_USERNAME")
      camulus-reddit-token-file (getenv "REDDIT_TOKEN_FILE"))
(unless (and camulus-reddit-username camulus-reddit-token-file) (error "Reddit test credentials not configured"))
(defun camulus-reddit-live-finished (id)
  (cl-find-if (lambda (e) (and (eq (car e) 'finished) (= (cadr e) id))) camulus-probe--events))
(defun camulus-reddit-live-await (id)
  (camulus-probe-await (lambda () (camulus-reddit-live-finished id)) 60)
  (unless (eq (nth 3 (camulus-reddit-live-finished id)) 'completed)
    (error "%s" (nth 4 (camulus-reddit-live-finished id)))))
(let* ((beats 0) (gc-count 0)
       (timer (run-at-time 0.02 0.02
                           (lambda ()
                             (cl-incf beats)
                             (when (< gc-count 4) (cl-incf gc-count) (camulus-probe-gc) (garbage-collect))))))
  (unwind-protect
      (progn
        (camulus-reader-browse-reddit "ocaml")
        (camulus-probe-await (lambda () (= 0 (hash-table-count camulus-reader--requests))) 60)
        (unless (> (hash-table-count camulus-reader--rows) 0) (error "Empty live subreddit"))
        (message "PASS: native Reddit subreddit, %d posts" (hash-table-count camulus-reader--rows))
        (goto-char (point-min)) (camulus-reader-open)
        (camulus-probe-await (lambda () (= 0 (hash-table-count camulus-reader--loads))) 60)
        (with-current-buffer "*Reader thread*"
          (unless (> (hash-table-count camulus-reader--messages) 0) (error "Empty live thread"))
          (message "PASS: native Reddit reply tree, %d messages" (hash-table-count camulus-reader--messages)))
        (with-current-buffer "*Reader*"
          (camulus-reader-search "effects")
          (camulus-probe-await (lambda () (= 0 (hash-table-count camulus-reader--requests))) 60))
        (unless (> beats 0) (error "Emacs heartbeat did not run"))
        (unless (camulus-probe-signals-unchanged) (error "Reddit worker changed host signal handlers"))
        (message "PASS: native HTTP + UI heartbeat (%d ticks), GC and host signals" beats))
    (cancel-timer timer)
    (camulus-probe-session-stop)))
