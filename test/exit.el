;;; exit.el --- Quit with live worker tasks -*- lexical-binding: t; -*-
(require 'effects-probe)
(ef-probe-session-start)
(ef-probe-await (lambda () (equal (ef-probe-status) "running")))
(ef-probe-submit 60000 60 t nil 0)
(ef-probe-submit 60000 60 nil nil 0)
;; Exercise the actual kill-emacs hook while both owners still have work.
;; Registered after the cleanup hook, so failure cannot masquerade as exit 0.
(add-hook 'kill-emacs-hook
          (lambda ()
            (unless (equal (ef-probe-status) "idle")
              (kill-emacs 1))
            (message "PASS: Emacs exit reclaimed active worker"))
          90)
(kill-emacs 0)
