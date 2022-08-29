;; Package setup
(package-initialize)
(add-to-list `package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list `package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(require `package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(set-keyboard-coding-system nil)

;; Enable mouse mode for terminal
(xterm-mouse-mode)

;; Display line numbers by default
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Confirm to exit
(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
          'append)

;; Color codes for Tramp compile
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;; Compile mode auto-scroll
(setq compilation-scroll-output t)


;; Set org mode agenda files
;; (setq org-agenda-files (list "/Users/connorfuhrman/iCloud/UArizona/VAESRL/org/general.org"
;; 			     "/Users/connorfuhrman/iCloud/UArizona/VAESRL/org/ICE_Rover.org"
;; 			     "/Users/connorfuhrman/iCloud/UArizona/VAESRL/org/Breadcrumbs.org"
;; 			     "/Users/connorfuhrman/iCloud/UArizona/ECE275/ECE275_FS2021/org/ECE275.org"))


;; Backup files
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


(setq c-default-style "linux"
      c-basic-offset 2)

;; flyspell map mouse-2 to correct word
(eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined)))


(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))


;; Auctex settings
;;    source https://kevinlanguasco.wordpress.com/2019/04/29/latex-editing-with-emacs-on-manjaro/
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Theme conveniences
(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

;; Enable indent guides by default
(use-package highlight-indent-guides
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (require 'highlight-indent-guides)
  (setq highlight-indent-guides-method 'column)
  (setq highlight-indent-guides-auto-enabled nil))

;; direnv setup
(use-package envrc
  :ensure t
  :init (envrc-global-mode))

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

;; ==================================================
;; LSP setup
(use-package 'lsp-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'c-mode-hook 'lsp-deferred)
  (add-hook 'c++-mode-hook 'lsp-deferred)
  (setq lsp-clients-clangd-args
	'("--header-insertion=never")))
(use-package lsp-java
  :ensure t
  :config (add-hook `java-mode-hook `lsp-deferred))
(use-package dap-java
  :ensure nil)
(use-package helm-lsp
  :ensure t)
(use-package lsp-treemacs
  :ensure t)

;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
;; 		  :major-modes '(python-mode)
;; 		  :remote? t
;; 		  :server-id 'pylsp-remote))
;;(setq lsp-pylsp-plugins-flake8-enabled nil)

(use-package yasnippet
  :ensure t)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

;; ==================================================
;; RealGUD Debugger Setup
(use-package realgud
  :ensure t )

;; ==================================================

;; Python black (formatter) configuration
;; (use-package python-black
;;   :ensure t
;;   :demand t
;;   :after python
;;   :hook (python-mode . python-black-on-save-mode-enable-dwim))


;; pyenv setup
(use-package pyenv-mode
  :ensure t
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project))
(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
    (if python-version-directory
        (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
               (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
          (pyenv-mode-set pyenv-current-version)
          (message (concat "Setting virtualenv to " pyenv-current-version))))))
;; (defvar pyenv-current-version nil nil)

;; (defun pyenv-init()
;;   "Initialize pyenv's current version to the global one."
;;   (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
;;     (message (concat "Setting pyenv version to " global-pyenv))
;;     (pyenv-mode-set global-pyenv)
;;     (setq pyenv-current-version global-pyenv)))

					;(add-hook 'python-mode-hook 'pyenv-init)


;; ======================================================================
;; Container setup
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-mode)))
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
;; ======================================================================

;; ======================================================================
;; Docview automatic resize to fit page, width, and height
;; source: https://stackoverflow.com/questions/23236555/making-document-view-in-emacs-fit-to-width-of-page

(require 'cl)

;;;; Automatic fitting minor mode
(defcustom doc-view-autofit-timer-start 1.0
  "Initial value (seconds) for the timer that delays the fitting when
`doc-view-autofit-fit' is called (Which is when a window
configuration change occurs and a document needs to be fitted)."
  :type 'number
  :group 'doc-view)

(defcustom doc-view-autofit-timer-inc 0.02
  "Value to increase (seconds) the timer (see `doc-view-autofit-timer-start')
by, if there is another window configuration change occuring, before
it runs out."
  :type 'number
  :group 'doc-view)

(defcustom doc-view-autofit-default-fit 'width
  "The fitting type initially used when mode is enabled.
Valid values are: width, height, page."
  :type 'symbol
  :group 'doc-view)

(defvar doc-view-autofit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c W") 'doc-view-autofit-width)
    (define-key map (kbd "C-c H") 'doc-view-autofit-height)
    (define-key map (kbd "C-c P") 'doc-view-autofit-page)
    map)
  "Keymap used by `doc-view-autofit-mode'.")

(defun doc-view-autofit-set (type)
  "Set autofitting to TYPE for current buffer."
  (when doc-view-autofit-mode
    (setq doc-view-autofit-type type)
    (doc-view-autofit-fit)))

(defun doc-view-autofit-width ()
  "Set autofitting to width for current buffer."
  (interactive) (doc-view-autofit-set 'width))

(defun doc-view-autofit-height ()
  "Set autofitting to height for current buffer."
  (interactive) (doc-view-autofit-set 'height))

(defun doc-view-autofit-page ()
  "Set autofitting to page for current buffer."
  (interactive) (doc-view-autofit-set 'page))

(defun doc-view-autofit-fit ()
  "Fits the document in the selected window's buffer
delayed with a timer, so multiple calls in succession
don't cause as much overhead."
  (lexical-let
      ((window (selected-window)))
    (if (equal doc-view-autofit-timer nil)
        (setq doc-view-autofit-timer
              (run-with-timer
               doc-view-autofit-timer-start nil
               (lambda ()
                 (if (window-live-p window)
                     (save-selected-window
                       (select-window window)
                       (cancel-timer doc-view-autofit-timer)
                       (setq doc-view-autofit-timer nil)
                       (cond
                        ((equal 'width doc-view-autofit-type)
                         (doc-view-fit-width-to-window))
                        ((equal 'height doc-view-autofit-type)
                         (doc-view-fit-height-to-window))
                        ((equal 'page doc-view-autofit-type)
                         (doc-view-fit-page-to-window))))))))
      (timer-inc-time doc-view-autofit-timer doc-view-autofit-timer-inc))))

(define-minor-mode doc-view-autofit-mode
  "Minor mode for automatic (timer based) fitting in DocView."
  :lighter " AFit" :keymap doc-view-autofit-mode-map :group 'doc-view
  (when doc-view-autofit-mode
    (set (make-local-variable 'doc-view-autofit-type)
         doc-view-autofit-default-fit)
    (set (make-local-variable 'doc-view-autofit-timer) nil)
    (add-hook 'window-configuration-change-hook
              'doc-view-autofit-fit nil t)
    (doc-view-autofit-fit))
  (when (not doc-view-autofit-mode)
    (remove-hook 'window-configuration-change-hook
                 'doc-view-autofit-fit t)
    (when doc-view-autofit-timer
      (cancel-timer doc-view-autofit-timer)
      (setq doc-view-autofit-timer nil))
    (setq doc-view-autofit-type nil)))

(add-hook 'doc-view-mode-hook 'doc-view-autofit-mode)

;; ===========================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("776c1ab52648f98893a2aa35af2afc43b8c11dd3194a052e0b2502acca02bfce" default))
 '(package-selected-packages
   '(realgud htmlize buffer-env minimap ubuntu-theme windresize use-package treemacs sr-speedbar python-black popup org-edna lsp-ui kconfig-mode julia-mode hl-todo highlight-indent-guides haskell-mode gnuplot gitlab-ci-mode flycheck elpy dockerfile-mode docker-tramp cmake-mode better-defaults auctex async arduino-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
