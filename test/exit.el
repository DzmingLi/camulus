;;; exit.el --- Quit with live worker tasks -*- lexical-binding: t; -*-
(require 'camulus)
(setq camulus-probe-record-events t)
(camulus-probe-session-start)
(camulus-probe-await (lambda () (equal (camulus-probe-status) "running")))
(camulus-probe-submit 60000 60 t nil 0)
(camulus-probe-submit 60000 60 nil nil 0)
;; Exercise the actual kill-emacs hook while both owners still have work.
;; Registered after the cleanup hook, so failure cannot masquerade as exit 0.
(add-hook 'kill-emacs-hook
          (lambda ()
            (unless (equal (camulus-probe-status) "idle")
              (kill-emacs 1))
            (message "PASS: Emacs exit reclaimed active worker"))
          90)
(kill-emacs 0)
