;;; mw-notes.el --- Note taking config               -*- lexical-binding: t; -*-

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


(use-package denote
  :custom
  (denote-store-link-to-heading t)
  (denote-directory (expand-file-name "notes" org-directory))
  (denote-templates '())
  (denote-org-capture-specifiers "%i\n%?")
  :config
  (require 'denote-org-extras)
  (defun mw/denote-extract-subtree-and-link ()
    "Extract subtree, and insert link."
    (declare (interactive-only t))
    (interactive nil ())
    (let ((denote-save-buffer-after-creation t))
      (denote-link-after-creating-with-command #'denote-org-extras-extract-org-subtree)))
  (defun mw/denote-extract-subtree-and-link-as-heading ()
    "Extract subtree, and insert link as heading at current level."
    (interactive)
    (let* ((denote-save-buffer-after-creation t)
           (denote-link-description-function-old denote-link-description-function)
           (level (org-outline-level)))
      (denote-link-after-creating-with-command #'denote-org-extras-extract-org-subtree)
      (save-excursion
        (let ((p (point)))
          (beginning-of-line)
          (insert (s-repeat level "*")
                  " ")
          (goto-char p)))
      )))

(use-package denote-explore
  :custom
  (denote-explore-network-directory (expand-file-name "graphs" denote-directory))
  (denote-explore-network-filename "denote-network")
  (denote-explore-network-format 'graphviz)
  (denote-explore-network-graphviz-filetype "pdf")
  (denote-explore-network-keywords-ignore '("bib")))

(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :custom
  (consult-notes-denote-display-id nil)
  :config
  (when (locate-library "denote")
    (consult-notes-denote-mode)))

(provide 'mw-notes)
;;; mw-notes.el ends here
