;;; init-audiovisual.el --- Configs for audio and visual -*- lexical-binding: t -*-
;;; Commentary:

;;; This file sets up Emacs configurations for when it is run inside the terminal (emacs -nw)
;;; It also sets the 

;;; Code:

;; No audo bells
(setq ring-bell-function 'ignore)

;; use the mouse in the termianl
(xterm-mouse-mode 1)

;; Display line numbers always in programming mode
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Configuration that doesn't depend on other modules
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

;; Add indent guides
;; (can be configured on a per-language basis)
(use-package highlight-indent-guides
  :ensure t
  :init
  ;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'column)
  (setq highlight-indent-guides-auto-enabled nil))

;; Able to resize the windows
(use-package windresize
  :ensure t)


(provide 'init-audiovisual)
;;; init-terminal.el ends here
