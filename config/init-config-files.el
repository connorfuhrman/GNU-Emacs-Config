;;; init-config-files.el --- Initialize config-files -*- lexical-binding: t -*-
;;; Commentary:

;;; This file sets up configs for config files
;;; Includes yaml, toml, json

;;; Code:

(use-package yaml-mode
  :ensure t)

(use-package toml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package jq-format
  :ensure t
  :after json-mode)

(provide 'init-config-files)
;;; init-config-files.el ends here
