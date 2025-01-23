;;; init-vterm.el --- Config for vterm -*- lexical-binding: t -*-
;;; Commentary:

;;; This file sets up the vterm package

;;; Code:


(require 'init-utils)

(use-package vterm
  :ensure t

  :config
  (with-eval-after-load 'init-utils
    (when *is-a-mac*
      (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=ON -DLIBVTERM_INCLUDE_DIR=/opt/homebrew/opt/libvterm/include -DLIBVTERM_LIBRARY=/opt/homebrew/opt/libvterm/lib/libvterm.dylib")))

  ;; Add a hook to set up local keybindings for vterm buffers
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Bind C-v to vterm-yank only in vterm buffers
              (local-set-key (kbd "C-y") 'vterm-yank))))

(provide 'init-vterm)
;;; init-vterm.el ends here
