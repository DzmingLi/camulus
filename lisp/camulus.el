;;; camulus.el --- Load the Camulus native module -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;;;###autoload (autoload 'camulus "camulus" "Open the Camulus home page." t)
;;;###autoload (autoload 'camulus-open-view "camulus" "Open a configured Camulus view." t)
;;;###autoload (autoload 'camulus-mail-new "camulus" "Compose a new mail from a configured account." t)
;;;###autoload (autoload 'camulus-mailto "camulus" "Open a Camulus draft for a mailto URI." t)
;;;###autoload (autoload 'camulus-douban-group "camulus" "Browse a Douban group by URL, ID or slug." t)
;;;###autoload (autoload 'camulus-subscriptions "camulus" "List current Camulus subscription sources." t)
;;;###autoload (autoload 'camulus-manage-subscriptions "camulus" "Manage a backend's remote topic tracking." t)
;;;###autoload (autoload 'camulus-mail-sync "camulus" "Synchronize and index Camulus mail now." t)
;;;###autoload (autoload 'camulus-github-repository "camulus" "Open the current local repository's GitHub topics in Camulus." t)
;;;###autoload (autoload 'camulus-firefox-cookies-get "camulus" "Return Firefox cookies for an HTTPS URL as a string alist." nil)
(require 'camulus_native)
(provide 'camulus)
;;; camulus.el ends here
