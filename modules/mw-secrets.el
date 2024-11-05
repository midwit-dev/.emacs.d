;;; mw-secrets.el --- Secrets management             -*- lexical-binding: t; -*-

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

(use-package auth-source
  :ensure nil
  :init
  (setopt auth-sources '("~/.authinfo.gpg" "~/.authinfo")))

;; password-store doesnt work for me without setting this environment var
(setenv "PASSWORD_STORE_DIR" (expand-file-name "~/.local/share/password-store"))
(use-package password-store-otp)
(use-package password-store)

(setq epg-key-id "B9B2E6C9814181D9")
(setq epa-file-encrypt-to "yo@midwit.dev")

(setq org-crypt-key epg-key-id)

(provide 'mw-secrets)
;;; mw-secrets.el ends here
