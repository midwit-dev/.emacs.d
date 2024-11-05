;;; mw-project.el --- Project setup                  -*- lexical-binding: t; -*-

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

(defun mw/project-vterm (&optional new-vterm)
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer-name-string (project-prefixed-buffer-name "vterm")))
    (if new-vterm
        (vterm (project-prefixed-buffer-name "vterm"))
      (vterm (project-prefixed-buffer-name "vterm")))))

(use-package project
  :ensure nil
  :after general
  :init
  ;; https://grtcdr.tn/posts/2022-08-08.html
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?g))

  :custom
  (project-vc-extra-root-markers '( ".eproj"))

  :general
  (mw/global-project-def
   "/" #'consult-ripgrep
   "B" #'project-list-buffers
   "D" #'project-dired
   "b" #'project-switch-to-buffer
   "c" #'project-compile
   "d" #'project-find-dir
   "e" #'project-eshell
   "f" #'project-find-file
   "i" #'consult-imenu-multi
   "k" #'project-kill-buffers
   "o" #'consult-project-extra-find-other-window
   "p" #'project-switch-project
   "s" #'mw/project-vterm
   ;; "t" #'mw/project-todo
   "v" #'project-vc-dir
   "x" #'project-execute-extended-command))

(provide 'mw-project)
;;; mw-project.el ends here
