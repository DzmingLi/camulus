;;; effects-probe.el --- Disposable integration harness -*- lexical-binding: t; -*-
;; This is a test/demo UI, not a backend or a permanent user configuration.
(require 'cl-lib)
(require 'camulus-runtime)
(require 'host-process)

;; The demo/test harness opts into event recording; the application does not.
(setq ef-probe-record-events t)

(defvar ef-probe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") (lambda () (interactive) (message "View job %s" (ef-probe-submit 1500 5 nil nil 0))))
    (define-key map (kbd "s") (lambda () (interactive) (message "Service job %s" (ef-probe-submit 1500 5 t nil 0))))
    (define-key map (kbd "x") (lambda () (interactive) (ef-probe-close-view)))
    (define-key map (kbd "e") (lambda () (interactive) (ef-probe-submit 0 1 t t 0)))
    (define-key map (kbd "g") (lambda () (interactive) (ef-probe-submit 0 1 t nil 100) (ef-probe-gc)))
    (define-key map (kbd "q") #'ef-probe-demo-stop)
    map))

(define-derived-mode ef-probe-mode special-mode "Effects probe"
  (setq header-line-format "v: view job  s: service job  x: close view  e: fail  g: GC  q: stop"))

(defun ef-probe-demo ()
  "Display the probe; all work is synthetic and local."
  (interactive)
  (when (equal (ef-probe-status) "idle") (ef-probe-session-start))
  (setq ef-probe--demo-buffer (get-buffer-create "*Ecaml effects probe*"))
  (pop-to-buffer ef-probe--demo-buffer)
  (ef-probe-mode))

(defun ef-probe-demo-stop ()
  "Request shutdown without waiting in the interactive command."
  (interactive)
  (unless (equal (ef-probe-status) "idle") (ef-probe-stop))
  (let (timer)
    (setq timer
          (run-at-time 0.02 0.02
                       (lambda ()
                         (when (ef-probe-reap)
                           (cancel-timer timer)
                           (ef-probe-session-stop)
                           (message "Effects probe stopped")))))))

(add-hook 'kill-emacs-hook #'ef-probe-session-stop)
(provide 'effects-probe)
