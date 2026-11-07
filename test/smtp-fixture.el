;;; smtp-fixture.el --- Only loopback test endpoints -*- lexical-binding: t; -*-
(defun camulus-test-smtp (base profile username &optional port-variable host ca-file)
  (camulus-mail-configure-smtp profile (or host "localhost")
                          (string-to-number (getenv (or port-variable "SMTP_TEST_PORT")))
                          username (expand-file-name "smtp-password" base)
                          (or ca-file (expand-file-name "smtp-cert.pem" base))))
(provide 'smtp-fixture)
