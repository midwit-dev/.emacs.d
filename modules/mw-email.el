;;; mw-email.el --- Email config                     -*- lexical-binding: t; -*-

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
(require 'mu4e)

(defun mw/mu4e-init ()
  ;; (add-hook 'mu4e-view-mode-hook #'mw/mu4e-html-rendering-opts)
  )

(defun mw/mu4e-config ()
  (setq
   ;; use mu4e as default program for composing mails in emacs
   mail-user-agent 'mu4e-user-agent
   ;; use mu4e as preferred method for reading mail in emacs
   read-mail-command 'mu4e
   ;; avoid sync conflicts
   mu4e-change-filenames-when-moving t
   ;; check mail 10 minutes
   mu4e-update-interval              (* 10 60)
   ;; re-flow mail so it's not hard wrapped (for receipients)
   mu4e-compose-format-flowed        t
   mu4e-get-mail-command             "mbsync -a"
   ;; mu4e-maildir                      "~/mail/protonmail"

   mu4e-drafts-folder                "/proton/Drafts"
   mu4e-sent-folder                  "/proton/Sent"
   mu4e-refile-folder                "/proton/All Mail"
   mu4e-trash-folder                 "/proton/Trash"

   mu4e-maildir-shortcuts            '(("/proton/inbox"     . ?i)
	                               ("/proton/Sent"      . ?s)
	                               ("/proton/Trash"     . ?t)
	                               ("/proton/Drafts"    . ?d)
	                               ("/proton/All Mail"  . ?a))
   ;; Sending email
   message-send-mail-function        'smtpmail-send-it
   smtpmail-smtp-server              "127.0.0.1"
   smtpmail-smtp-service             1025
   smtpmail-stream-type              'starttls))


(defun mw/system-processes-by-name-from-args (process-name)
  "List system-processes by name from args"
  (let (result)
    (dolist (proc (list-system-processes) result)
      (let* ((attrs (process-attributes proc))
             (args (string-split (alist-get 'args attrs) nil t)))
        (when (string= (cl-first args) process-name)
          (push attrs result))))))

(defun mw/start-protonmail-bridge-maybe ()
  "Starts protonmail-bridge if it's not already running."
  (if (> (length (mw/system-processes-by-name-from-args "protonmail-bridge"))
         0)
      (message "Not starting ProtonMail-Bridge, it's already running...")
    (let* ((proc-buffer (get-buffer-create "*protonmail-bridge*"))
           (proc (start-process "protonmail-bridge" proc-buffer "protonmail-bridge" "-n"))))))

(add-hook 'mu4e-update-pre-hook #'mw/start-protonmail-bridge-maybe)
(mw/mu4e-config)

(provide 'mw-email)
;;; mw-email.el ends here
