;;; init-fs-backup.el --- Configs for filesystem backup -*- lexical-binding: t -*-
;;; Commentary:

;;; This file sets up the filesystem backup

;;; Code:
;; Backup files
(setq
   backup-by-copying t                 ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 10
   kept-old-versions 10
   version-control t)                  ; use versioned backups

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

(provide 'init-fs-backup)
;;; init-fs-backup.el ends here
