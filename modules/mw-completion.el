;;; mw-completion.el --- Completion setup            -*- lexical-binding: t; -*-

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


(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-auto nil) ;; ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-cycle t)
  (corfu-popupinfo-mode 1)
  (corfu-popupinfo-delay 0.5)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width) ;; Always the same width
  (corfu-preview-current nil)
  (corfu-quit-at-boundrary nil)
  (corfu-preselect 'valid)
  ;; :bind
  ;; (:map corfu-map
  ;;       ("RET" . nil))
  :general
  (general-define-key
   :states '(insert emacs)
   :keymaps 'global-map
   "C-SPC" 'corfu-insert-separator
   "C-y"   'completion-at-point
   )
  (progn)

  (general-define-key
   :states '(insert emacs)
   :keymaps 'corfu-map
   "C-y"   #'corfu-complete
   "C-n"   #'corfu-next
   "C-p"   #'corfu-previous)

  :config
  (keymap-unset corfu-map "RET")
  (keymap-unset corfu-map "TAB")
  )

(use-package corfu-terminal
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package nerd-icons-corfu
  :autoload (nerd-icons-corfu-formatter)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :ensure t
  :bind (("C-c p p" . completion-at-point))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  (setq completion-cycle-threshold nil)
  (setq tab-always-indent t)
  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)
  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package vertico
  :init
  (vertico-mode)
  :config
  ;; Enable cycling for `vertico-next` and `vertico-previous`.
  (setq vertico-cycle t))

(use-package vertico-directory
  :after vertico
  :ensure nil ;; comes with vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("C-d" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Enable rich annotations using the MArginalia package
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; TODO: Consider removing
(use-package embark
  :bind (("C-\\" . embark-act)
         ("C-|" . embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'mw-completion)
;;; mw-completion.el ends here
