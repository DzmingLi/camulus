;;; host-process.el --- Subprocess host adapter, no backend policy -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'subr-x)
(defvar ef-host--jobs (make-hash-table :test #'eql))

(defun ef-host--cancel (id)
  (when-let* ((entry (gethash id ef-host--jobs)))
    (remhash id ef-host--jobs)
    (dolist (p entry) (when (processp p)
                       (set-process-sentinel p #'ignore)
                       (when (process-live-p p) (delete-process p))))))

(defun ef-host-cancel-all ()
  (mapc #'ef-host--cancel (hash-table-keys ef-host--jobs)))

(defun ef-host--start (id argv input-path limit)
  (let ((chunks nil) (size 0) (done nil) (status nil) (err-done nil) proc err)
    (cl-labels
        ((finish ()
           ;; A separate stderr pipe may deliver its EOF after the child exits.
           (when (and (not done) status err-done)
             (setq done t)
             (remhash id ef-host--jobs)
             (ef-host-process-complete id status
                                      (decode-coding-string (apply #'concat (nreverse chunks)) 'utf-8-unix))
             (when (process-live-p err) (delete-process err))))
         (fail ()
           (unless done
             (setq done t)
             (ef-host-process-complete id -2 "")
             (ef-host--cancel id))))
      (condition-case nil
          (progn
            (setq err (make-pipe-process
                       :name "ef-host-stderr" :noquery t :coding 'binary
                       ;; Do not echo private mail or credentials in error output.
                       :filter (lambda (_p bytes)
                                 (cl-incf size (length bytes))
                                 (when (> size limit) (fail)))
                       :sentinel (lambda (p _event)
                                   (unless (process-live-p p) (setq err-done t) (finish)))))
            (setq proc
                  (make-process
                   :name "ef-host-command"
                   ;; Fixed redirection script; all paths/arguments are positional
                   ;; data, never shell source. exec leaves Emacs owning the child.
                   :command (if (string-empty-p input-path) argv
                              (append (list (or (executable-find "sh") "/bin/sh") "-c"
                                            "file=$1; shift; exec \"$@\" < \"$file\""
                                            "ef-host-stdin" input-path) argv))
                   :connection-type 'pipe
                   :noquery t :coding 'binary :stderr err
                   :filter (lambda (_p bytes)
                             (cl-incf size (length bytes))
                             (if (> size limit) (fail) (push bytes chunks)))
                   :sentinel (lambda (p _event)
                               (when (memq (process-status p) '(exit signal failed))
                                 (setq status (if (eq (process-status p) 'exit)
                                                  (process-exit-status p) -1))
                                 (finish)))))
            (puthash id (list proc err) ef-host--jobs)
            (process-send-eof proc))
        (error
         (unless done (setq done t) (ef-host-process-complete id -1 ""))
         (ef-host--cancel id)
         (when (process-live-p err) (delete-process err)))))))

(defun ef-host-handle-event (event)
  "Handle a native process request without publishing its arguments to UI logs."
  (pcase event
    (`(host-start ,id ,argv ,input ,limit) (ef-host--start id argv input limit) t)
    (`(host-cancel ,id) (ef-host--cancel id) t)
    (_ nil)))
(provide 'host-process)
