;;; init-code-intel.el --- Initialize code intelligence -*- lexical-binding: t -*-
;;; Commentary:

;;; This file sets up code intelligence

;;; Code:

(use-package company
  :ensure t
  :config
  (setq lsp-completion-provider :capf
	company-minimum-prefix-length 2         ; Show completions after 2 chars
        company-idle-delay 0.1))                ; Wait 0.1s before showing completions	
	

;; lsp setup
(use-package lsp-mode
  :ensure t
  :after company 
  :config
  ;; Performance tuning for LSP
  ;; These values balance responsiveness with resource usage
  (setq gc-cons-threshold (* 100 1024 1024)     ; 100MB garbage collection threshold
        read-process-output-max (* 512 1024))   ; 512KB process output chunks

  ;; Core LSP settings
  ;; These control how LSP interacts with language servers
  (setq lsp-auto-configure nil                  ; Automatically configure language servers
        lsp-prefer-flymake nil                  ; Use flycheck instead of flymake
        lsp-idle-delay 0.5                      ; Wait 0.5s before running language server
        lsp-response-timeout 10                 ; Language server response timeout
        lsp-keep-workspace-alive nil)           ; Don't keep servers running when unused

  ;; Debug settings (disabled by default)
  ;; Toggle these with the function below when troubleshooting
  (setq lsp-log-io nil                         ; Don't log LSP communication
        lsp-print-performance nil)             ; Don't show performance info

  ;; Utility function to toggle debugging
  (defun toggle-lsp-debug ()
    "Toggle LSP debugging options and report the new state."
    (interactive)
    (setq lsp-log-io (not lsp-log-io)
          lsp-print-performance (not lsp-print-performance))
    (message "LSP debugging: %s" (if lsp-log-io "enabled" "disabled"))))

;; Place for future language-specific configurations
;; Example structure:
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (setq lsp-python-ms-python-executable-cmd "python3")))

;; LSP UI with optimized settings for remote sessions
(use-package lsp-ui
  :ensure t
  :after lsp
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-flycheck t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-delay 0.5           ; Increased delay
        lsp-ui-doc-enable t 
        lsp-ui-doc-delay 0.5                ; Increased delay if doc is enabled
        lsp-ui-doc-show-with-cursor nil)   ; Only show doc on mouse hover
  ;; UI and display settings
  ;; These control how LSP information is presented
  (setq lsp-headerline-breadcrumb-enable t      ; Show file path breadcrumb
        lsp-modeline-diagnostics-enable t       ; Show errors in modeline
        lsp-signature-auto-activate t           ; Show function signatures
        lsp-signature-render-documentation t))  ; Include docs in signatures


(use-package helm-lsp
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package lsp-docker
  :ensure t)

(use-package projectile
  :ensure t)



(provide 'init-code-intel)
;;; config-shim.el ends here
