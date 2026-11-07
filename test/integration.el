;;; integration.el --- Fresh Emacs integration checks -*- lexical-binding: t; -*-
(require 'ert)
(require 'camulus)
(setq camulus-probe-record-events t)

(ert-deftest firefox-cookie-callback-without-reader-startup ()
  (let* ((profile (make-temp-file "camulus-cookie-callback-" t))
         (camulus-firefox-profile profile)
         (database (expand-file-name "cookies.sqlite" profile)))
    (unwind-protect
        (progn
          ;; Synthetic credentials only. No real profile or network access.
          (should (= 0 (call-process "python3" nil nil nil "-c"
            "import sqlite3,sys
with sqlite3.connect(sys.argv[1]) as db:
 db.execute('CREATE TABLE moz_cookies (host,path,name,value,expiry,originAttributes,creationTime)')
 db.executemany('INSERT INTO moz_cookies VALUES (?,?,?,?,?,?,?)', [('.zhihu.com','/','session','fixture',4102444800,'',1),('.douban.com','/','other','private',4102444800,'',2)])"
            database)))
          (should (equal (camulus-probe-status) "idle"))
          (should (equal (camulus-firefox-cookies-get "https://www.zhihu.com/api/")
                         '(("session" . "fixture"))))
          (should-not (camulus-firefox-cookies-get "https://example.com/"))
          (should-error (camulus-firefox-cookies-get "http://www.zhihu.com/"))
          (let ((camulus-firefox-profile nil))
            (should-error (camulus-firefox-cookies-get "https://www.zhihu.com/")))
          (should (= 0 (call-process "python3" nil nil nil "-c"
            "import sqlite3,sys
with sqlite3.connect(sys.argv[1]) as db:
 db.execute(\"UPDATE moz_cookies SET value='updated' WHERE name='session'\")"
            database)))
          (should (equal (camulus-firefox-cookies-get "https://www.zhihu.com/")
                         '(("session" . "updated"))))
          (should (equal (camulus-probe-status) "idle")))
      (delete-directory profile t))))

(defmacro camulus-probe-with-session (&rest body)
  `(progn
     (camulus-probe-session-start)
     (unwind-protect
         (progn (camulus-probe-await (lambda () (equal (camulus-probe-status) "running"))) ,@body)
       (camulus-probe-session-stop))))

(defun camulus-probe-result (id)
  (cl-find-if (lambda (event) (and (eq (car event) 'finished) (= (cadr event) id))) camulus-probe--events))

(ert-deftest probe-01-pipe-and-heartbeat ()
  (camulus-probe-with-session
   (let* ((beats 0)
          (timer (run-at-time 0.01 0.01 (lambda () (cl-incf beats))))
          (start (float-time))
          (id (camulus-probe-submit 250 5 nil nil 0)))
     (unwind-protect
         (progn
           (should (< (- (float-time) start) 0.2))
           (camulus-probe-await (lambda () (camulus-probe-result id)))
           (should (eq (nth 3 (camulus-probe-result id)) 'completed))
           (should (> beats 3))
           (should (> camulus-probe--wakeups 0))
           (should (> (cadr (cl-find 'ready camulus-probe--events :key #'car)) 0)))
       (cancel-timer timer)))))

(ert-deftest probe-02-explicit-cancel ()
  (camulus-probe-with-session
   (let ((id (camulus-probe-submit 5000 1 nil nil 0)))
     (should (camulus-probe-cancel id))
     (should (camulus-probe-cancel id))
     (camulus-probe-await (lambda () (camulus-probe-result id)))
     (should (eq (nth 3 (camulus-probe-result id)) 'cancelled)))))

(ert-deftest probe-03-view-close-preserves-service ()
  (camulus-probe-with-session
   (let ((view (camulus-probe-submit 1500 2 nil nil 0))
         (service (camulus-probe-submit 40 1 t nil 0)))
     (camulus-probe-close-view)
     (camulus-probe-await (lambda () (and (camulus-probe-result service) (> (camulus-probe-stale-count) 0))))
     (should-not (camulus-probe-result view))
     (should (eq (nth 3 (camulus-probe-result service)) 'completed)))))

(ert-deftest probe-04-queued-late-results ()
  (camulus-probe-with-session
   (setq camulus-probe--hold t)
   (camulus-probe-submit 0 1 nil nil 0)
   (sleep-for 0.1)
   (camulus-probe-close-view)
   (setq camulus-probe--hold nil)
   (camulus-probe-await (lambda () (> (camulus-probe-stale-count) 0)))
   (should-not (cl-find 'finished camulus-probe--events :key #'car))))

(ert-deftest probe-05-errors-are-local ()
  (camulus-probe-with-session
   (let ((bad (camulus-probe-submit 0 1 t t 0))
         (good (camulus-probe-submit 20 1 t nil 0)))
     (camulus-probe-await (lambda () (and (camulus-probe-result bad) (camulus-probe-result good))))
     (should (eq (nth 3 (camulus-probe-result bad)) 'failed))
     (should (eq (nth 3 (camulus-probe-result good)) 'completed))
     (should (equal (camulus-probe-status) "running")))))

(ert-deftest probe-06-concurrent-gc-and-host-child ()
  (camulus-probe-with-session
   (let ((id (camulus-probe-submit 0 1 t nil 300))
         (child (make-process :name "probe-child" :buffer nil :noquery t
                              :command (list shell-file-name "-c" "exit 7"))))
     (unwind-protect
         (progn
           (dotimes (_ 12) (camulus-probe-gc) (garbage-collect))
           (camulus-probe-await (lambda () (and (camulus-probe-result id) (not (process-live-p child)))) 10)
           (should (= (process-exit-status child) 7))
           (should (eq (nth 3 (camulus-probe-result id)) 'completed))
           (should (camulus-probe-signals-unchanged)))
       (when (process-live-p child) (delete-process child))))))

(ert-deftest probe-07-shutdown-with-full-results ()
  (camulus-probe-with-session
   (setq camulus-probe--hold t)
   (camulus-probe-submit 0 1000 t nil 0)
   (sleep-for 0.1)
   (camulus-probe-stop)
   (camulus-probe-await (lambda () (equal (camulus-probe-status) "stopped")))
   (should (camulus-probe-signals-unchanged))
   (setq camulus-probe--hold nil)))

(ert-deftest probe-08-restart-and-descriptor-cleanup ()
  (let ((before (length (directory-files "/proc/self/fd" nil "^[0-9]"))))
    (dotimes (_ 5)
      (camulus-probe-with-session
       (let ((id (camulus-probe-submit 1 1 t nil 0)))
         (camulus-probe-await (lambda () (camulus-probe-result id))))))
    (garbage-collect)
    (should (<= (length (directory-files "/proc/self/fd" nil "^[0-9]")) (+ before 2)))))

(ert-deftest probe-09-deleted-notification-process ()
  (camulus-probe-with-session
   (delete-process camulus-probe--process)
   (let ((id (camulus-probe-submit 50 5 t nil 0)))
     ;; Timer fallback continues delivery even when the native wakeup pipe dies.
     (camulus-probe-await (lambda () (camulus-probe-result id)))
     (should (eq (nth 3 (camulus-probe-result id)) 'completed))
     (should (camulus-probe-signals-unchanged)))))

(ert-run-tests-batch-and-exit)
