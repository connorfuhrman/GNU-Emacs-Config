;;; init-direnv.el --- Initialize direnv -*- lexical-binding: t -*-
;;; Commentary:

;;; This file sets up direnv integration through the envrc package.
;;; It automatically enables direnv support on all remote hosts that have
;;; direnv installed, without requiring manual host configuration.

;;; Code:

(use-package envrc
  :ensure t
  :demand t
  
  :init
  (defun my-envrc-check-direnv-available ()
    "Check if direnv is available on the current system."
    (zerop (call-process "which" nil nil nil "direnv")))

  (defun my-envrc-check-remote-direnv-available (host)
    "Check if direnv is available on the remote HOST."
    (with-temp-buffer
      (let ((default-directory (format "/ssh:%s:~/" host)))
	(if (zerop (process-file "which" nil t nil "direnv"))
            t
          (message "direnv not found on remote host %s" host)
          nil))))

  (defun my-envrc-remote-setup ()
    "Configure envrc for remote hosts automatically.
     This function checks if we're on a remote host and if direnv
     is available, then enables envrc-mode appropriately."
    (let ((file-name (expand-file-name (or buffer-file-name default-directory))))
      (if (file-remote-p file-name)
          ;; Remote file handling
          (when-let* ((parsed-file (with-parsed-tramp-file-name file-name nil host))
                      (remote-host host))
            (when (my-envrc-check-remote-direnv-available remote-host)
              (envrc-mode 1)))
        ;; Local file handling
        (when (my-envrc-check-direnv-available)
          (envrc-mode 1)))))

  :config
  (envrc-global-mode)

  (setq envrc-mode-line-prefix "🌍"
        envrc-none-function #'envrc-none-with-indicator)

  (add-hook 'find-file-hook #'my-envrc-remote-setup)  ; Open a file
  (add-hook 'dired-mode-hook #'my-envrc-remote-setup) ; Enter directory
  
  :bind
  (:map envrc-mode-map
        ("C-c e" . envrc-command-map)    ; Main prefix for all envrc commands
        :map envrc-command-map
        ("r" . envrc-reload)             ; Reload the direnv environment
        ("a" . envrc-allow)              ; Allow the current .envrc
        ("b" . envrc-deny)               ; Block/deny the current .envrc
        ("s" . envrc-show)))             ; Show the current environment

(provide 'init-direnv)
;;; init-direnv.el ends here
