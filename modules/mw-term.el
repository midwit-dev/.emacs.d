;;; mw-term.el --- Terminal setup                    -*- lexical-binding: t; -*-

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

(require 'mw-opt)

(use-package ansi-color
  :ensure nil)

(defun mw/kill-vterm-buffer-query-function ()
  (or (not vterm--term)
      (not (process-running-child-p (get-buffer-process (current-buffer))))
      (y-or-n-p (format "Buffer %S has a running process; kill it? "
		        (buffer-name (current-buffer))
                        vterm--process))))

(defun mw/vterm-mode-hook ()
  (setq-local kill-buffer-query-functions '(mw/kill-vterm-buffer-query-function)))


(use-package vterm
  :demand t
  ;; :hook
  ;; (vterm-mode . #'mw/vterm-mode-hook)
  :general
  (general-define-key
   :keymaps 'vterm-mode-map
   :states '(normal motion)
   "ZZ"  #'kill-current-buffer))

(defun mw/vterm-buffers (&optional filters)
  (--filter (s-starts-with? "*vterm*" (buffer-name it)) (buffer-list)))

(provide 'mw-term)
;;; mw-term.el ends here
