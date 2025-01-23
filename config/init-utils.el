;;; init-utils.el --- General utils -*- lexical-binding: t -*-
;;; Commentary:

;;; This file sets up some helper utils

;;; Code:

(defconst *is-a-mac* (eq system-type 'darwin)
  "Non-nil if this is running on a Mac system.")
  
;; We define a named function instead of a lambda for better debugging
(defun my/confirm-emacs-exit ()
  "Ask for confirmation before exiting Emacs."
  (y-or-n-p "Quit Emacs? "))
  
;; Add our named function to the kill-emacs hooks
(add-hook 'kill-emacs-query-functions #'my/confirm-emacs-exit 'append)


(provide 'init-utils)
;;; init-utils.el ends here
