;;; mw-ui.el --- UI & related setup                  -*- lexical-binding: t; -*-

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

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

(setq font-lock-maximum-decoration t
      truncate-partial-width-windows nil)

(setq visible-bell nil)
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.05 nil 'invert-face 'mode-line)))

(global-hl-line-mode 1)

(setq mw/evil-modeline-faces '((normal ())
                               (insert ((bg bg-blue-intense)
                                        (fg fg-main)))
                               (visual ((bg bg-region)
                                        (fg fg-main)))
                               (replace ((bg bg-red-intense)
                                         (fg fg-main)))
                               (operator ((bg bg-green-subtle)
                                          (fg fg-main)))
                               (emacs ((bg bg-main)
                                       (fg fg-main)))))

(defun mw/get-evil-modeline-state-face (state attribute)
  (let* ((theme (symbol-name (cl-first custom-enabled-themes)))
         (get-color (cond
                     ((s-starts-with? "modus-" theme)
                      #'modus-themes-get-color-value)
                     ((s-starts-with? "standard-" theme)
                      #'standard-themes-get-color-value)
                     (lambda ()
                       'unspecified))))
    (funcall get-color (cl-first (alist-get attribute
                                            (cl-first (alist-get state mw/evil-modeline-faces))))))
  )

(defun mw/set-evil-modeline-faces ()
  (modify-face 'doom-modeline-evil-normal-state
               (mw/get-evil-modeline-state-face 'normal 'fg)
               (mw/get-evil-modeline-state-face 'normal 'bg))
  (modify-face 'doom-modeline-evil-insert-state
               (mw/get-evil-modeline-state-face 'insert 'fg)
               (mw/get-evil-modeline-state-face 'insert 'bg))
  (modify-face 'doom-modeline-evil-visual-state
               (mw/get-evil-modeline-state-face 'visual 'fg)
               (mw/get-evil-modeline-state-face 'visual 'bg))
  (modify-face 'doom-modeline-evil-visual-state
               (mw/get-evil-modeline-state-face 'visual 'fg)
               (mw/get-evil-modeline-state-face 'visual 'bg))
  (modify-face 'doom-modeline-evil-replace-state
               (mw/get-evil-modeline-state-face 'replace 'fg)
               (mw/get-evil-modeline-state-face 'replace 'bg))
  (modify-face 'doom-modeline-evil-operator-state
               (mw/get-evil-modeline-state-face 'operator 'fg)
               (mw/get-evil-modeline-state-face 'operator 'bg))
  (modify-face 'doom-modeline-evil-emacs-state
               (mw/get-evil-modeline-state-face 'emacs 'fg)
               (mw/get-evil-modeline-state-face 'emacs 'bg)))

(use-package modus-themes
  :after evil
  :custom
  (modus-themes-to-toggle '(modus-operandi modus-vivendi))
  :config
  (setq modus-themes-common-palette-overrides '(
                                                (border-mode-line-active unspecified)
                                                (border-mode-line-inactive unspecified)
                                                (bg-mode-line-active bg-diff-context)
                                                (bg-mode-line-inactive bg-main)
                                                (bg-tab-bar bg-dim)
                                                (bg-tab-current bg-main)
                                                (bg-tab-other bg-dim)))
  ;; (set-face-background 'doom-modeline-evil-insert-state (modus-themes-get-color-value 'bg-magenta-subtle))
  (load-theme 'modus-operandi :no-confirm :no-enable)
  (load-theme 'modus-vivendi :no-confirm)
  (general-define-key
   :states '(motion)
   "<f5>" #'modus-themes-toggle)
  (add-hook 'modus-themes-post-load-hook #'mw/set-evil-modeline-faces)
  )

(when (display-graphic-p)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(use-package doom-modeline
  :commands (doom-modeline-mode)
  :init
  (setopt doom-modeline-battery nil
          doom-modeline-time nil)
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-modal-icon nil)
  (doom-modeline-hud-min-height 20)
  (doom-modeline-modal-modern-icon nil))

(use-package unicode-fonts
  :commands (unicode-fonts-setup)
  :config
  (unicode-fonts-setup))

(use-package emojify)

(use-package nerd-icons)

(use-package fontaine
  :demand t
  :config
  (fontaine-mode 1)
  (fontaine-set-preset 'regular)
  :custom
  (fontaine-presets
   '((regular
      :default-height 120)
     (large
      :bold-weight semibold
      :default-weight 700)
     (t
      :default-family "Iosevka Term NF"
      :default-weight regular
      :default-slant normal
      :default-width normal
      :default-height 100))))

(use-package display-fill-column-indicator
  :ensure nil
  :config
  (display-fill-column-indicator-mode 1)
  :custom
  (display-fill-column-indicator-character ?┆))

(use-package all-the-icons)
(use-package all-the-icons-dired)

(provide 'mw-ui)
;;; mw-ui.el ends here
