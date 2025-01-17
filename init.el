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

;; ;; Emacs Application Framework
;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework")
;; (use-package eaf-terminal
;;   :load-path "~/.emacs.d/site-lisp/eaf-terminal")

;; (require 'eaf-image-viewer)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-browser)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-file-manager)
;; (require 'eaf-video-player)
;; (require 'eaf-org-previewer)
;; (require 'eaf-jupyter)
;; (require 'eaf-git)
;; (require 'eaf-system-monitor)
;; (require 'eaf-pyqterminal)


;; vterm
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
	      )))





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
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)

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

;; direnv setup
;; (use-package envrc
;;   :ensure t
;;   :init (envrc-global-mode))
;; (defcustom my-direnv-enabled-hosts nil
;;   "List of remote hosts to use Direnv on.
;; Each host must have `direnv' executable accessible in the default
;; environment."
;;   :type '(repeat string)
;;   :group 'my)

;; (defun tramp-sh-handle-start-file-process@my-direnv (args)
;;   "Enable Direnv for hosts in `my-direnv-enabled-hosts'."
;;   (with-parsed-tramp-file-name (expand-file-name default-directory) nil
;;     (if (member host my-direnv-enabled-hosts)
;;         (pcase-let ((`(,name ,buffer ,program . ,args) args))
;;           `(,name
;;             ,buffer
;;             "direnv"
;;             "exec"
;;             ,localname
;;             ,program
;;             ,@args))
;;       args)))

;; envrc config (thanks Claude)
(use-package envrc
  :ensure t
  :init (envrc-global-mode))

;; Define which hosts should use direnv
(defcustom my-direnv-enabled-hosts '("episci-impulse")  ; Add your hosts here, e.g. '("remote1" "remote2")
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

;; (use-package lsp-mode
;;   :ensure t
;;   :hook (
;; 	 (python-mode . lsp-deferred)
;; 	 (shell-script-mode . lsp-deferred)
;; 	 (c-mode . lsp-deferred)
;; 	 (c++-mode . lsp-deferred))
;;   :config (setq lsp-auto-configure t
;; 		lsp-prefer-flymake nil
;; 		gc-cons-threshold (* 500 1024 1024)
;; 		read-process-output-max (* 1024 1024)
;; 		company-minimum-prefix-length 1
;; 		company-idle-delay 0.0
;; 		company-minimum-prefix-length 1
;; 		lsp-idle-delay 5
;; 		lsp-clients-clangd-args '("--header-insertion=never"))
;;   )

  ;; lsp-auto-configure t
  ;; 		lsp-prefer-flymake nil
  ;; 		gc-cons-threshold (* 500 1024 1024)
  ;; 		read-process-output-max (* 1024 1024)
  ;; 		treemacs-space-between-root-nodes nil
  ;; 		company-minimum-prefix-length 1
  ;; 		company-idle-delay 0.0
  ;; 		company-minimum-prefix-length 1
  ;; 		lsp-idle-delay 5
  ;; 		lsp-clients-clangd-args '("--header-insertion=never")

;; (use-package lsp-julia
;;   :ensure t
;;   :config
;;   (setq lsp-julia-default-environment "~/.julia/environments/v1.11"))

;; (setq lsp-log-io t)
;; (setq lsp-docker-log-level "debug")

;;  :config
;;  (setq lsp-clients-clangd-args
;;   	'("--header-insertion=never"))
;;  (setq lsp-clients-clangd-remote-args
;;   	'("--header-insertion=never"))

;; (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
;;(use-package lsp-javap
;;  :ensure t
;;  :config (add-hook `java-mode-hook `lsp-deferred))
;;(use-package dap-java
;;  :ensure nil)
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


;; (use-package lsp-ui
;;   :ensure t
;;   :init
;;   (setq lsp-ui-sideline-enable t
;; 	lsp-ui-sideline-show-hover t
;; 	lsp-ui-sideline-show-flycheck t
;; 	lsp-ui-sideline-show-code-actions t
;; 	lsp-ui-sideline-show-diagnostics t
;; 	lsp-ui-sideline-delay 0.25
;; 	lsp-ui-imenu-refresh-delay 0.25
;; 	lsp-ui-imenu-auto-refresh t))


;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
;; 		  :major-modes '(python-mode)
;; 		  :remote? t
;; 		  :server-id 'pylsp-remote))
;;(setq lsp-pylsp-plugins-flake8-enabled nil)

;; (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
;;                      :major-modes '(c-mode c++-mode)
;;                      :remote? t
;;                      :server-id 'clangd-remote))

;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
;;                      :major-modes '(python-mode)
;;                      :remote? t
;;                      :server-id 'pylsp-remote))

;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; (setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave") ;; TRAMP save buffers locally

;; Basic LSP setup
(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp-deferred)
         (shell-script-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :config
  ;; General LSP settings
  (setq lsp-auto-configure t
        lsp-prefer-flymake nil
        
        ;; Adjust performance settings for remote sessions
        gc-cons-threshold (* 100 1024 1024)  ; Reduced from 500MB to 100MB
        read-process-output-max (* 512 1024)  ; Reduced from 1MB to 512KB
        
        ;; Company settings
        company-minimum-prefix-length 1
        company-idle-delay 0.1  ; Slightly increased from 0.0
        
        ;; LSP settings
        lsp-idle-delay 0.5     ; Reduced from 5
        lsp-response-timeout 10 ; Add explicit timeout
        lsp-keep-workspace-alive nil ; Don't keep workspace alive when buffer closes
        
        ;; Clangd specific settings
        lsp-clients-clangd-args '("--header-insertion=never"
                                 "--background-index=false"    ; Disable background indexing for remote
                                 "--clang-tidy=false"         ; Disable clang-tidy for performance
                                 "--completion-style=detailed"
                                 "--cross-file-rename=true"
                                 "--header-insertion-decorators=false"))

  ;; Debug logging (uncomment if needed)
  (setq lsp-log-io t)
  (setq lsp-print-performance t)
  
  ;; Remote client registration
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote
                    :synchronize-sections '(document)  ; Minimize synchronization
                    :initialization-options
                    '(:clangd (:background-index nil
                             :compilation-database-path "build")))))

;; LSP UI with optimized settings for remote sessions
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil      ; Disable hover for performance
        lsp-ui-sideline-show-flycheck t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-delay 0.5           ; Increased delay
        lsp-ui-doc-enable nil               ; Disable doc window for performance
        lsp-ui-doc-delay 2.0                ; Increased delay if doc is enabled
        lsp-ui-doc-show-with-cursor nil))   ; Only show doc on mouse hover

;; TRAMP specific settings
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
  
  ;; Increase TRAMP timeout
  (setq tramp-connection-timeout 10)
  
  ;; Disable version control for remote files
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package yasnippet
  :ensure t)
;;(with-eval-after-load 'lsp-mode
;;  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;  (require 'dap-cpptools)
;;  (yas-global-mode))

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


;; ======================================================================
;; Docview automatic resize to fit page, width, and height
;; source: https://stackoverflow.com/questions/23236555/making-document-view-in-emacs-fit-to-width-of-page

;; (require 'cl)

;; ;;;; Automatic fitting minor mode
;; (defcustom doc-view-autofit-timer-start 1.0
;;   "Initial value (seconds) for the timer that delays the fitting when
;; `doc-view-autofit-fit' is called (Which is when a window
;; configuration change occurs and a document needs to be fitted)."
;;   :type 'number
;;   :group 'doc-view)

;; (defcustom doc-view-autofit-timer-inc 0.02
;;   "Value to increase (seconds) the timer (see `doc-view-autofit-timer-start')
;; by, if there is another window configuration change occuring, before
;; it runs out."
;;   :type 'number
;;   :group 'doc-view)

;; (defcustom doc-view-autofit-default-fit 'width
;;   "The fitting type initially used when mode is enabled.
;; Valid values are: width, height, page."
;;   :type 'symbol
;;   :group 'doc-view)

;; (defvar doc-view-autofit-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-c W") 'doc-view-autofit-width)
;;     (define-key map (kbd "C-c H") 'doc-view-autofit-height)
;;     (define-key map (kbd "C-c P") 'doc-view-autofit-page)
;;     map)
;;   "Keymap used by `doc-view-autofit-mode'.")

;; (defun doc-view-autofit-set (type)
;;   "Set autofitting to TYPE for current buffer."
;;   (when doc-view-autofit-mode
;;     (setq doc-view-autofit-type type)
;;     (doc-view-autofit-fit)))

;; (defun doc-view-autofit-width ()
;;   "Set autofitting to width for current buffer."
;;   (interactive) (doc-view-autofit-set 'width))

;; (defun doc-view-autofit-height ()
;;   "Set autofitting to height for current buffer."
;;   (interactive) (doc-view-autofit-set 'height))

;; (defun doc-view-autofit-page ()
;;   "Set autofitting to page for current buffer."
;;   (interactive) (doc-view-autofit-set 'page))

;; (defun doc-view-autofit-fit ()
;;   "Fits the document in the selected window's buffer
;; delayed with a timer, so multiple calls in succession
;; don't cause as much overhead."
;;   (lexical-let
;;       ((window (selected-window)))
;;     (if (equal doc-view-autofit-timer nil)
;;         (setq doc-view-autofit-timer
;;               (run-with-timer
;;                doc-view-autofit-timer-start nil
;;                (lambda ()
;;                  (if (window-live-p window)
;;                      (save-selected-window
;;                        (select-window window)
;;                        (cancel-timer doc-view-autofit-timer)
;;                        (setq doc-view-autofit-timer nil)
;;                        (cond
;;                         ((equal 'width doc-view-autofit-type)
;;                          (doc-view-fit-width-to-window))
;;                         ((equal 'height doc-view-autofit-type)
;;                          (doc-view-fit-height-to-window))
;;                         ((equal 'page doc-view-autofit-type)
;;                          (doc-view-fit-page-to-window))))))))
;;       (timer-inc-time doc-view-autofit-timer doc-view-autofit-timer-inc))))

;; (define-minor-mode doc-view-autofit-mode
;;   "Minor mode for automatic (timer based) fitting in DocView."
;;   :lighter " AFit" :keymap doc-view-autofit-mode-map :group 'doc-view
;;   (when doc-view-autofit-mode
;;     (set (make-local-variable 'doc-view-autofit-type)
;;          doc-view-autofit-default-fit)
;;     (set (make-local-variable 'doc-view-autofit-timer) nil)
;;     (add-hook 'window-configuration-change-hook
;;               'doc-view-autofit-fit nil t)
;;     (doc-view-autofit-fit))
;;   (when (not doc-view-autofit-mode)
;;     (remove-hook 'window-configuration-change-hook
;;                  'doc-view-autofit-fit t)
;;     (when doc-view-autofit-timer
;;       (cancel-timer doc-view-autofit-timer)
;;       (setq doc-view-autofit-timer nil))
;;     (setq doc-view-autofit-type nil)))

;; (add-hook 'doc-view-mode-hook 'doc-view-autofit-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "c517e98fa036a0c21af481aadd2bdd6f44495be3d4ac2ce9d69201fcb2578533" "6adeb971e4d5fe32bee0d5b1302bc0dfd70d4b42bad61e1c346599a6dc9569b5" "4fdbed4aa8bcb199d7f6a643886bac51178d1705b9b354ef3dd82d4ec48072d2" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "6f96a9ece5fdd0d3e04daea6aa63e13be26b48717820aa7b5889c602764cf23a" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd" "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "e87fd8e24e82eb94d63b1a9c79abc8161d25de9f2f13b64014d3bf4b8db05e9a" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "badd1a5e20bd0c29f4fe863f3b480992c65ef1fa63951f59aa5d6b129a3f9c4c" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" "b1acc21dcb556407306eccd73f90eb7d69664380483b18496d9c5ccc5968ab43" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" default))
 '(package-selected-packages
   '(lsp-haskell lsp-julia haskell-mode mermaid-mode vterm eaf cmake-mode company dap-mode docker dockerfile-mode doom-themes envrc flycheck gnuplot-mode helm-lsp helm-xref htmlize julia-mode lsp-docker lsp-treemacs lsp-ui matlab-mode nix-mode opencl-mode projectile protobuf-mode pyenv-mode realgud rust-mode which-key windresize yaml-mode yasnippet))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    '(org-edit-latex org-contrib htmlize docker dockerfile-mode pyvenv pyenv-mode realgud yasnippet lsp-ui dap-mode which-key flycheck projectile company lsp-treemacs helm-xref helm-lsp lsp-docker julia-mode cmake-mode helm envrc windresize doom-themes yaml-mode nix-mode)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

