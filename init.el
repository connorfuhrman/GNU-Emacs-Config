(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(julia-mode windresize helm sr-speedbar kconfig-mode arduino-mode docker-tramp gitlab-ci-mode dockerfile-mode markdown-mode gnuplot flycheck better-defaults elpy org-edna auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;(add-to-list 'load-path "~/.emacs.d/")
;(load "chpl-mode.el")

(set-keyboard-coding-system nil)

;; Enable mouse mode for terminal
(xterm-mouse-mode)

;; Display line numbers by default
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; helm-M-x as default M-x

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
(setq org-agenda-files (list "/Users/connorfuhrman/iCloud/UArizona/VAESRL/org/general.org"
			     "/Users/connorfuhrman/iCloud/UArizona/VAESRL/org/ICE_Rover.org"
			     "/Users/connorfuhrman/iCloud/UArizona/VAESRL/org/Breadcrumbs.org"
			     "/Users/connorfuhrman/iCloud/UArizona/ECE275/ECE275_FS2021/org/ECE275.org"))

;; MELPA setup
(require `package)
(add-to-list `package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list `package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;; CMake syntax highlighting
(add-to-list 'load-path "~/.emacs.d/cmake-emacs/")
(require 'cmake-mode)

;; Backup files
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
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


;; Python configuration:
(elpy-enable)
;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--colors=Linux --profile=default --simple-prompt"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


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
