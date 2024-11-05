;;; mw-reading.el --- Reading setup                  -*- lexical-binding: t; -*-

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

;; epub reader
(use-package nov
  :after (evil)
  :commands (nov-mode)
  :mode ("\\.epub\\'" . nov-mode))

;; pdf reader
(use-package pdf-tools
  :config
  (pdf-tools-install))


(use-package elfeed
  :general
  ;; (mw/local-leader-def '(motion normal) elfeed-search-mode-map
  ;;                      "tr" `("Toggle +rust" . ,(mw/elfeed-search-exclusive-filter-toggle "+rust"))
  ;;                      "tc" `("Toggle +clojure" . ,(mw/elfeed-search-exclusive-filter-toggle "+clojure")))
  )

(use-package elfeed-org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files (list (expand-file-name "feeds.org" org-directory))))

(provide 'mw-reading)
;;; mw-reading.el ends here
