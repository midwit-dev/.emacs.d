;;; mw-misc.el --- Misc improvements                 -*- lexical-binding: t; -*-

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

(use-package helpful
  :config
  (general-define-key
   :keymaps 'global-map
   [remap describe-command] 'helpful-command
   [remap describe-function] 'helpful-callable
   [remap describe-key] 'helpful-key
   [remap describe-variable] 'helpful-variable
   [remap describe-symbol] 'helpful-symbol))

;; TODO: Put under evil-window-map
(use-package ace-window
  :after 'evil
  :custom
  (aw-dispatch-always 't)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (mw/global-leader-def
   "ww" '("ace" . ace-window)))

(use-package consult)

(use-package which-key
  :demand t
  :init
  (setq which-key-operator " ")
  (setq which-key-idle-delay 0.5)
  (setq which-key-prefix-prefix "+")
  :config
  (mw/global-leader-def
   "?" 'which-key-show-top-level)
  (which-key-mode 1))

(use-package direnv
  :config
  (add-to-list 'warning-suppress-types '(direnv))
  (direnv-mode))


;; TODO: Maybe move out of mw-misc.el
(setq plantuml-default-exec-mode 'executable)
(setq plantuml-executable-path 'executable)

(use-package plantuml-mode
  :custom
  (plantuml-executable-path (executable-find "plantuml")))


(provide 'mw-misc)
;;; mw-misc.el ends here
