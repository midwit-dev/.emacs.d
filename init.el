;;; init.el --- My Emacs config                      -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "modules"))

(require 'mw-elpaca)

(setq user-emacs-directory (expand-file-name "~/.emacs.d.midwit/"))

(use-package emacs
  :ensure nil
  :custom
  (native-comp-async-report-warnings-errors nil)
  (user-full-name "The Midwit Developer")
  (user-mail-address "yo@midwit.dev")
  (use-short-answers t)
  :config
  ;; This is set to "C" to use english day names in org-mode
  (setq system-time-locale "C"))

;; Enable all commands
(use-package novice
  :ensure nil
  :custom
  (disabled-command-function nil))

;; Set options via elisp
(use-package cus-edit
  :ensure nil
  :custom
  (custom-file null-device))

(use-package time
  :ensure nil
  :custom
  (display-time-24hr-format t))

;; Always select the help window
(use-package help
  :ensure nil
  :custom
  (help-window-select t))

(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode 1))

(require 'mw-opt)
(require 'mw-lib)
(require 'mw-evil)
(require 'mw-ui)
(require 'mw-secrets)
(require 'mw-git)
(require 'mw-term)
(require 'mw-editing)
(require 'mw-misc)
(require 'mw-reading)
(require 'mw-completion)
(require 'mw-org)
(require 'mw-notes)
(require 'mw-project)

(require 'mw-dev)
(require 'mw-email)

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
