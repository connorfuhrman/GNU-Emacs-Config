;;; config-shim.el --- Shim to load all configs -*- lexical-binding: t -*-
;;; Commentary:

;;; This file loads all the configurations in this directory. It is a convenience
;;; to only have to have one 'require' in the 'init.el'

;;; Code:

(require 'init-utils)
(require 'init-audiovisual)
(require 'init-keyboard)
(require 'init-vterm)
(require 'init-nix)
(require 'init-fs-backup)
(require 'init-helm)
(require 'init-flyspell)
(require 'init-config-files)
(require 'init-direnv)
(require 'init-code-intel)
(require 'init-c-cxx)

(provide 'config-shim)
;;; config-shim.el ends here
