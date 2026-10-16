;;; integration.el --- Fresh Emacs integration checks -*- lexical-binding: t; -*-
(require 'ert)
(require 'effects-probe)

(defmacro ef-probe-with-session (&rest body)
  `(progn
     (ef-probe-session-start)
     (unwind-protect
         (progn (ef-probe-await (lambda () (equal (ef-probe-status) "running"))) ,@body)
       (ef-probe-session-stop))))

(defun ef-probe-result (id)
  (cl-find-if (lambda (event) (and (eq (car event) 'finished) (= (cadr event) id))) ef-probe--events))

(ert-deftest probe-01-pipe-and-heartbeat ()
  (ef-probe-with-session
   (let* ((beats 0)
          (timer (run-at-time 0.01 0.01 (lambda () (cl-incf beats))))
          (start (float-time))
          (id (ef-probe-submit 250 5 nil nil 0)))
     (unwind-protect
         (progn
           (should (< (- (float-time) start) 0.2))
           (ef-probe-await (lambda () (ef-probe-result id)))
           (should (eq (nth 3 (ef-probe-result id)) 'completed))
           (should (> beats 3))
           (should (> ef-probe--wakeups 0))
           (should (> (cadr (cl-find 'ready ef-probe--events :key #'car)) 0)))
       (cancel-timer timer)))))

(ert-deftest probe-02-explicit-cancel ()
  (ef-probe-with-session
   (let ((id (ef-probe-submit 5000 1 nil nil 0)))
     (should (ef-probe-cancel id))
     (should (ef-probe-cancel id))
     (ef-probe-await (lambda () (ef-probe-result id)))
     (should (eq (nth 3 (ef-probe-result id)) 'cancelled)))))

(ert-deftest probe-03-view-close-preserves-service ()
  (ef-probe-with-session
   (let ((view (ef-probe-submit 1500 2 nil nil 0))
         (service (ef-probe-submit 40 1 t nil 0)))
     (ef-probe-close-view)
     (ef-probe-await (lambda () (and (ef-probe-result service) (> (ef-probe-stale-count) 0))))
     (should-not (ef-probe-result view))
     (should (eq (nth 3 (ef-probe-result service)) 'completed)))))

(ert-deftest probe-04-queued-late-results ()
  (ef-probe-with-session
   (setq ef-probe--hold t)
   (ef-probe-submit 0 1 nil nil 0)
   (sleep-for 0.1)
   (ef-probe-close-view)
   (setq ef-probe--hold nil)
   (ef-probe-await (lambda () (> (ef-probe-stale-count) 0)))
   (should-not (cl-find 'finished ef-probe--events :key #'car))))

(ert-deftest probe-05-errors-are-local ()
  (ef-probe-with-session
   (let ((bad (ef-probe-submit 0 1 t t 0))
         (good (ef-probe-submit 20 1 t nil 0)))
     (ef-probe-await (lambda () (and (ef-probe-result bad) (ef-probe-result good))))
     (should (eq (nth 3 (ef-probe-result bad)) 'failed))
     (should (eq (nth 3 (ef-probe-result good)) 'completed))
     (should (equal (ef-probe-status) "running")))))

(ert-deftest probe-06-concurrent-gc-and-host-child ()
  (ef-probe-with-session
   (let ((id (ef-probe-submit 0 1 t nil 300))
         (child (make-process :name "probe-child" :buffer nil :noquery t
                              :command (list shell-file-name "-c" "exit 7"))))
     (unwind-protect
         (progn
           (dotimes (_ 12) (ef-probe-gc) (garbage-collect))
           (ef-probe-await (lambda () (and (ef-probe-result id) (not (process-live-p child)))) 10)
           (should (= (process-exit-status child) 7))
           (should (eq (nth 3 (ef-probe-result id)) 'completed))
           (should (ef-probe-signals-unchanged)))
       (when (process-live-p child) (delete-process child))))))

(ert-deftest probe-07-shutdown-with-full-results ()
  (ef-probe-with-session
   (setq ef-probe--hold t)
   (ef-probe-submit 0 1000 t nil 0)
   (sleep-for 0.1)
   (ef-probe-stop)
   (ef-probe-await (lambda () (equal (ef-probe-status) "stopped")))
   (should (ef-probe-signals-unchanged))
   (setq ef-probe--hold nil)))

(ert-deftest probe-08-restart-and-descriptor-cleanup ()
  (let ((before (length (directory-files "/proc/self/fd" nil "^[0-9]"))))
    (dotimes (_ 5)
      (ef-probe-with-session
       (let ((id (ef-probe-submit 1 1 t nil 0)))
         (ef-probe-await (lambda () (ef-probe-result id))))))
    (garbage-collect)
    (should (<= (length (directory-files "/proc/self/fd" nil "^[0-9]")) (+ before 2)))))

(ert-deftest probe-09-deleted-notification-process ()
  (ef-probe-with-session
   (delete-process ef-probe--process)
   (let ((id (ef-probe-submit 50 5 t nil 0)))
     ;; Timer fallback continues delivery even when the native wakeup pipe dies.
     (ef-probe-await (lambda () (ef-probe-result id)))
     (should (eq (nth 3 (ef-probe-result id)) 'completed))
     (should (ef-probe-signals-unchanged)))))

(ert-run-tests-batch-and-exit)
