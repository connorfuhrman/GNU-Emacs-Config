;;; kconfig-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "kconfig-mode" "kconfig-mode.el" (0 0 0 0))
;;; Generated autoloads from kconfig-mode.el

(autoload 'kconfig-mode "kconfig-mode" "\
Major mode for editing Kconfig files in the Linux kernel.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\Kconfig\\'" . kconfig-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kconfig-mode" '("kconfig-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kconfig-mode-autoloads.el ends here
