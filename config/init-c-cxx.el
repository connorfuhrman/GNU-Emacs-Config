;;; init-c-cxx.el --- Configs for C/C++ -*- lexical-binding: t -*-
;;; Commentary:

;;; This file sets up configs for C and C++

;;; Code:

(require 'init-code-intel)

; Run lsp for both C and C++
(with-eval-after-load 'init-code-intel
  (add-hook 'c-mode-hook 'lsp-deferred)
  (add-hook 'c++-mode-hook 'lsp-deferred)


  ;; Remote client registration
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote
                    :synchronize-sections '(document))))  ; Minimize synchronization

(provide 'init-c-cxx)
;;; init-c-cxx.el ends here
