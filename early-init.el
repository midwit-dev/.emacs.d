;;; early-init.el --- Early init                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  The Midwit Developer

;; Author: The Midwit Developer <yo@midwit.dev>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(setq package-enable-at-startup nil)
(setq emacs-build-time '(26246 57798 52146 403000))
(setenv "LSP_USE_PLISTS" "true")

;; Increased gc treshold for faster startup times
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)
(defun mw/gc-after-focus-change ()
  "Collect garbage when frame is unfocused for 5 secs."
  (run-with-idle-timer 5 nil
                       (lambda () (unless (frame-focus-state)
                                    (garbage-collect)))))
(defun mw/after-init ()
  ;; 100 MiB
  (setq gc-cons-threshold (* 1048 1024 1024)
        gc-cons-percentage 0.1)
  (message "gc-cons-threshold set to 1024MiB.")
  (when (boundp 'after-focus-change-function)
    (add-function :after after-focus-change-function #'mw/gc-after-focus-change)))
(add-hook 'elpaca-after-init-hook #'mw/after-init)

;; 1mb, useful for lsp
(setq read-process-output-max (* 1024 1024))

;; Remove security vulnerability
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

;; Dont use custom-file
(setq custom-file nil)

;; Write all backup files to its own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Write all autosave files in the tmp dir
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Don't write lock-files
(setq create-lockfiles nil)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(provide 'early-init)
;;; early-init.el ends here
