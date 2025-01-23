;;; init-flyspell.el --- Initialize flyspell -*- lexical-binding: t -*-
;;; Commentary:

;;; This file sets up flyspell so that spell checking is enabled

;;; Code:

(use-package flyspell
  :ensure t
  :defer t   ; don't load until needed
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (markdown-mode . flyspell-mode)
         (org-mode . flyspell-mode))
  :config
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] #'undefined))

(provide 'init-flyspell)
;;; init-flyspell.el ends here
