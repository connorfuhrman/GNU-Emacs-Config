(setq custom-file "/dev/null") ; Don't care about custom file
(setq custom-safe-themes t)   ; Accept all themes as safe
(setq enable-local-variables :all) ; Accept all local variables

;; Package setup
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(require `package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Bring in all the configs
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(require 'config-shim)



;; ;; Color codes for Tramp compile
;; (require 'ansi-color)
;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region compilation-filter-start (point))
;;   (toggle-read-only))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;; ;; Compile mode auto-scroll
;; (setq compilation-scroll-output t)



;; (setq c-default-style "linux"
;;       c-basic-offset 2)


;; (use-package julia-mode
;;   :ensure t)



;; ;; Python black (formatter) configuration
;; ;; (use-package python-black
;; ;;   :ensure t
;; ;;   :demand t
;; ;;   :after python
;; ;;   :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; (use-package elvish-mode
;;   :ensure t)

;; (use-package rust-mode
;;   :ensure t)

;; ;; ======================================================================
;; ;; Container setup
;; (use-package dockerfile-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
;;   (add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-mode)))
;; (use-package docker
;;   :ensure t
;;   :bind ("C-c d" . docker))
;; ;; ======================================================================


;; ;; ======================================================================
;; ;; org mode setup
;; (use-package org
;;   :ensure t
;;   :pin gnu)

;; (use-package htmlize
;;   :ensure t)

;; ;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; ;;(use-package org-contrib
;; ;;  :ensure t)

;; ;;(use-package org-edit-latex
;; ;;  :ensure t)

;; ;; Babel setup
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '(
;;    (python . t)
;;    (shell . t)
;;    (latex . t)
;;    )
;;  )

;; (setq org-export-allow-bind-keywords t)

;; ;; taskjuggler setup
;; ;;(require 'ox-taskjuggler)

;; ;; calfw setup
;; ;;(use-package calfw
;; ;;  :ensure t)
;; ;;(use-package calfw-org
;; ;;  :ensure t)

;; ;;(setq org-agenda-entry-text-maxlines
;; ;;      50)

;; ;;(setq org-agenda-skip-scheduled-if-done t)

;; ;;(setq org-agenda-custom-commands
;; ;;      '(("u" "Upcoming deadlines" agenda "" 
;; ;;         ((org-agenda-time-grid nil)
;; ;;          (org-deadline-warning-days 0)
;; ;;          (org-agenda-entry-types '(:deadline))
;; ;;          ))
;; ;;	("w" todo "WORKING")
;; ;;	))

;; ;; Always export entry text in agenda mode
;; ;;(add-hook 'org-agenda-before-write-hook 'org-agenda-add-entry-text)
;; ;;(setq org-agenda-add-entry-text-maxlines 50)

;; ;; ======================================================================

;; ;; ======================================================================
;; ;; Load the local emacs init file
;; ;;
;; ;; This file holds the following
;; ;;  - org agenda files for this particular machine
;; (let ((local_init_file  "~/.local/emacs.d/local_init.el"))
;;   (when (file-exists-p local_init_file)
;;     (load-file local_init_file))
;;   )
;; ;; ======================================================================


