;;; init-helm.el --- Initialize helm -*- lexical-binding: t -*-
;;; Commentary:

;;; This file sets up help

;;; Code:

(use-package helm
  :ensure t
  :init
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-split-window-in-side-p           t 
	helm-move-to-line-cycle-in-source     t 
	helm-ff-search-library-in-sexp        t 
	helm-scroll-amount                    8 
	helm-ff-file-name-history-use-recentf t
	helm-echo-input-in-header-line t)
  :config
  (helm-autoresize-mode 1)
  (helm-mode)
  :bind
  ("M-x" . helm-M-x))

(provide 'init-helm)
;;; init-helm.el ends here
