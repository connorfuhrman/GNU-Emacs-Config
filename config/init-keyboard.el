;;; init-keyboard.el --- Keyboard config -*- lexical-binding: t -*-
;;; Commentary:

;;; This file sets 

;;; Code:


;; Disable input encoding translation
(set-keyboard-coding-system nil)  

;; Set keybindings for MacOS:
(setq mac-command-key-is-meta nil) ; Use command key normally
(setq mac-option-key-is-meta t) ; Use option key as Meta

(provide 'init-keyboard)
;;; init-keyboard.el ends here
