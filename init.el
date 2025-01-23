;; Package setup
(package-initialize)
(add-to-list `package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list `package-archives
;;	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(require `package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(set-keyboard-coding-system nil)

;; Enable mouse mode for terminal
(xterm-mouse-mode)

;; No sounds
(setq ring-bell-function 'ignore)

;; MacOS use option key as Meta
(setq mac-command-key-is-meta nil)
(setq mac-option-key-is-meta t)


;; Display line numbers by default
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Confirm to exit
(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
          'append)


;; vterm
(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=ON -DLIBVTERM_INCLUDE_DIR=/opt/homebrew/opt/libvterm/include -DLIBVTERM_LIBRARY=/opt/homebrew/opt/libvterm/lib/libvterm.dylib")
(use-package vterm
  :ensure t

  :config
    ;; Add a hook to set up local keybindings for vterm buffers
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Bind C-v to vterm-yank only in vterm buffers
              (local-set-key (kbd "C-y") 'vterm-yank)
              
              ;; You can keep other local settings here, like:
              ;; (setq-local cursor-type nil))))
	      ))
  )





;; Color codes for Tramp compile
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;; Compile mode auto-scroll
(setq compilation-scroll-output t)

;; Nix mode
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;; YAML-mode
(use-package yaml-mode
  :ensure t)


;; Backup files
(setq
   backup-by-copying t                 ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)                  ; use versioned backups


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

;; Theme conveniences
(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;; (defadvice load-theme (before disable-themes-first activate)
;;   (disable-all-themes))

(use-package doom-themes
  :ensure t)

;; Enable indent guides by default
;; (use-package highlight-indent-guides
;;   :ensure t
;;   :init
;;   (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;   (require 'highlight-indent-guides)
;;   (setq highlight-indent-guides-method 'column)
;;   (setq highlight-indent-guides-auto-enabled nil))


(use-package windresize
  :ensure t)


;; envrc config (thanks Claude)
(use-package envrc
  :ensure t
  :init (envrc-global-mode))

;; Define which hosts should use direnv
(defcustom my-direnv-enabled-hosts '("mac-mini")  ; Add your hosts here, e.g. '("remote1" "remote2")
  "List of remote hosts to use Direnv on.
Each host must have `direnv' executable accessible in the default
environment."
  :type '(repeat string)
  :group 'my)

;; Advice to wrap commands with direnv on enabled hosts
(defun tramp-sh-handle-start-file-process@my-direnv (args)
  "Enable Direnv for hosts in `my-direnv-enabled-hosts'."
  (with-parsed-tramp-file-name (expand-file-name default-directory) nil
    (if (and host (member host my-direnv-enabled-hosts))
        (pcase-let ((`(,name ,buffer ,program . ,args) args))
          `(,name
            ,buffer
            "direnv"
            "exec"
            ,localname
            ,program
            ,@args))
      args)))

;; Apply the advice after TRAMP is loaded
(with-eval-after-load "tramp-sh"
  (advice-add 'tramp-sh-handle-start-file-process
              :filter-args #'tramp-sh-handle-start-file-process@my-direnv))

;; (with-eval-after-load "tramp-sh"
;;   (advice-add 'tramp-sh-handle-start-file-process
;;               :filter-args #'tramp-sh-handle-start-file-process@my-direnv))

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

(use-package cmake-mode
  :ensure t)

(use-package julia-mode
  :ensure t)

;; ==================================================
;; LSP setup
(use-package lsp-docker
  :ensure t)

;; (use-package lsp-julia
;;   :ensure t
;;   :config
;;   (setq lsp-julia-default-environment "~/.julia/environments/v1.11"))


(use-package helm-lsp
  :ensure t)
(use-package helm-xref
  :ensure t)

(use-package lsp-treemacs
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq lsp-completion-provider :capf))

;; (use-package projectile
;;   :ensure t)

(use-package hydra
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package avy
  :ensure t)

(use-package which-key
  :ensure t)

(use-package dap-mode
  :ensure t)

(use-package flymake
  :ensure t)


;; ;; Basic LSP setup
;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((python-mode . lsp-deferred)
;;          (shell-script-mode . lsp-deferred)
;;          (c-mode . lsp-deferred)
;;          (c++-mode . lsp-deferred)
;; 	 (rust-mode . lsp-deferred))
;;   :config
;;   ;; General LSP settings
;;   (setq lsp-auto-configure t
;;         lsp-prefer-flymake nil
        
;;         ;; Adjust performance settings for remote sessions
;;         gc-cons-threshold (* 100 1024 1024)  ; Reduced from 500MB to 100MB
;;         read-process-output-max (* 512 1024)  ; Reduced from 1MB to 512KB
        
;;         ;; Company settings
;;         company-minimum-prefix-length 1
;;         company-idle-delay 0.1  ; Slightly increased from 0.0
        
;;         ;; LSP settings
;;         lsp-idle-delay 0.5     ; Reduced from 5
;;         lsp-response-timeout 10 ; Add explicit timeout
;;         lsp-keep-workspace-alive nil ; Don't keep workspace alive when buffer closes
        
;;         ;; Clangd specific settings
;;         lsp-clients-clangd-args '("--header-insertion=never"
;;                                  "--background-index=false"    ; Disable background indexing for remote
;;                                  "--clang-tidy=false"         ; Disable clang-tidy for performance
;;                                  "--completion-style=detailed"
;;                                  "--cross-file-rename=true"
;;                                  "--header-insertion-decorators=false"))

;;   ;; Debug logging (uncomment if needed)
;;   (setq lsp-log-io t)
;;   (setq lsp-print-performance t)
  
;;   ;; Remote client registration
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
;;                     :major-modes '(c-mode c++-mode)
;;                     :remote? t
;;                     :server-id 'clangd-remote
;;                     :synchronize-sections '(document)  ; Minimize synchronization
;;                     :initialization-options
;;                     '(:clangd (:background-index nil
;;                              :compilation-database-path "build")))))

(setq tramp-remote-path
      (append tramp-remote-path
              '("~/.nix-profile/bin" "~/.local/bin" "/run/current-system/sw/bin")))

(setq my-direnv-enabled-hosts '("mac-mini"))

(use-package lsp-mode
  :ensure t
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :config
  (setq lsp-auto-configure t
        gc-cons-threshold (* 50 1024 1024)
        read-process-output-max (* 256 1024)
        lsp-idle-delay 1.0
        lsp-log-io t)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote)))

;; ;; LSP UI with optimized settings for remote sessions
;; (use-package lsp-ui
;;   :ensure t
;;   :config
;;   (setq lsp-ui-sideline-enable t
;;         lsp-ui-sideline-show-hover t
;;         lsp-ui-sideline-show-flycheck t
;;         lsp-ui-sideline-show-code-actions t
;;         lsp-ui-sideline-show-diagnostics t
;;         lsp-ui-sideline-delay 0.5           ; Increased delay
;;         lsp-ui-doc-enable t 
;;         lsp-ui-doc-delay 0.5                ; Increased delay if doc is enabled
;;         lsp-ui-doc-show-with-cursor nil))   ; Only show doc on mouse hover

;; TRAMP specific settings
;; (with-eval-after-load 'tramp
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;;   (setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
  
;;   ;; Increase TRAMP timeout
;;   (setq tramp-connection-timeout 10)
  
;;   ;; Disable version control for remote files
;;   (setq vc-ignore-dir-regexp
;;         (format "\\(%s\\)\\|\\(%s\\)"
;;                 vc-ignore-dir-regexp
;;                 tramp-file-name-regexp)))

(use-package yasnippet
  :ensure t)
;;(with-eval-after-load 'lsp-mode
;;  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;  (require 'dap-cpptools)
;;  (yas-global-mode))


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
(defvar pyenv-current-version nil nil)

(defun pyenv-init()
  "Initialize pyenv's current version to the global one."
  (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
    (message (concat "Setting pyenv version to " global-pyenv))
    (pyenv-mode-set global-pyenv)
    (setq pyenv-current-version global-pyenv)))

					(add-hook 'python-mode-hook 'pyenv-init)

;; pyvenv setup
;; (use-package pyvenv
  ;; :ensure t)

(use-package elvish-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

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
;; org mode setup
(use-package org
  :ensure t
  :pin gnu)

(use-package htmlize
  :ensure t)

;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;;(use-package org-contrib
;;  :ensure t)

;;(use-package org-edit-latex
;;  :ensure t)

;; Babel setup
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (shell . t)
   (latex . t)
   )
 )

(setq org-export-allow-bind-keywords t)

;; taskjuggler setup
;;(require 'ox-taskjuggler)

;; calfw setup
;;(use-package calfw
;;  :ensure t)
;;(use-package calfw-org
;;  :ensure t)

;;(setq org-agenda-entry-text-maxlines
;;      50)

;;(setq org-agenda-skip-scheduled-if-done t)

;;(setq org-agenda-custom-commands
;;      '(("u" "Upcoming deadlines" agenda "" 
;;         ((org-agenda-time-grid nil)
;;          (org-deadline-warning-days 0)
;;          (org-agenda-entry-types '(:deadline))
;;          ))
;;	("w" todo "WORKING")
;;	))

;; Always export entry text in agenda mode
;;(add-hook 'org-agenda-before-write-hook 'org-agenda-add-entry-text)
;;(setq org-agenda-add-entry-text-maxlines 50)

;; ======================================================================

;; ======================================================================
;; Load the local emacs init file
;;
;; This file holds the following
;;  - org agenda files for this particular machine
(let ((local_init_file  "~/.local/emacs.d/local_init.el"))
  (when (file-exists-p local_init_file)
    (load-file local_init_file))
  )
;; ======================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
