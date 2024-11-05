;;; mw-dev.el --- Base development setup                   -*- lexical-binding: t; -*-

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package reformatter)

(use-package elec-pair
  :ensure nil
  :config
  (electric-pair-mode -1))

(use-package paredit
  :hook ((emacs-lisp-mode) . enable-paredit-mode))

(use-package smartparens)

(use-package display-line-numbers
  :ensure nil
  :custom (display-line-numbers-type 'relative)
  :hook (prog-mode . display-line-numbers-mode))

(use-package prog-mode
  :ensure nil
  :init
  (defun mw/prog-mode-init ()
	(setq display-line-numbers-type 'relative)
	(electric-pair-mode -1)
	(display-line-numbers-mode 'relative)
	(setq evil-lookup-func #'eldoc-box-help-at-point))
  :hook (prog-mode . mw/prog-mode-init))

(use-package ws-butler
  :hook ((org-mode prog-mode) . ws-butler-mode)
  :custom
  (ws-butler-keep-whitespace-before-point nil))

(defun interrupting-flymake-start-syntax-check (base-function)
  (when (and (boundp 'flymake-syntax-check-process) (process-live-p flymake-syntax-check-process))
    (setq flymake-check-was-interrupted t)
    (flymake-kill-process flymake-syntax-check-process))
  (funcall base-function)
  (let ((proc (car flymake-processes)))
    (set-process-query-on-exit-flag proc nil)
    (set (make-local-variable 'flymake-syntax-check-process) proc)
    (setq flymake-check-was-interrupted t)
    (setq flymake-is-running nil)))


(defun mw/flymake-evil-insert-exit-hook ()
  (setq flymake-no-changes-timeout 0.5)
  (flymake-start nil t))

(defun mw/flymake-evil-insert-entry-hook ()
  (setq flymake-no-changes-timeout nil))

(use-package flymake
  :ensure nil
  :custom
  (flymake-no-changes-timeout 0.5)
  :config
  (add-hook 'evil-insert-state-exit-hook #'mw/flymake-evil-insert-exit-hook)
  (add-hook 'evil-insert-state-entry-hook #'mw/flymake-evil-insert-entry-hook))

(use-package eglot
  :ensure nil
  :after general
  :hook (eglot-mode . sideline-mode)
  :init
  (setq eglot-stay-out-of '(flymake))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :config
  (add-hook 'eglot-managed-mode-hook 'mw/eglot-managed-mode-initialize)
  (general-define-key
   :keymaps  'eglot-mode-map
   :states '(normal motion)
   [remap display-local-help nil])

  (setopt eglot-events-buffer-config '(:size 0 :format 'full))
  (set-face-attribute 'flymake-end-of-line-diagnostics-face nil :height 0.9 :weight 'bold
                      )
  (setopt flymake-show-diagnostics-at-end-of-line 'short)
  :general
  (mw/global-error-def
   "j" '("next" . flymake-goto-next-error)
   "k" '("previous" . flymake-goto-prev-error)
   "l" '("list" . flymake-show-buffer-diagnostics)
   "p" '("project" . flymake-show-project-diagnostics))
  (mw/local-leader-def eglot-mode-map
                       "r"  `("refactor" . ,(make-sparse-keymap))
                       "rr" '("rename" . eglot-rename)

                       "f" `("format" . ,(make-sparse-keymap))
                       "fb" '("format buffer" . eglot-format-buffer)
                       "fb" '("format" . eglot-format)

                       "T" `("toggle" . ,(make-sparse-keymap))
                       "Th" '("inlay hints" . eglot-inlay-hints-mode)

                       "RET" '("code actions" . eglot-code-actions)
                       "," '("quickfix" . eglot-code-action-quickfix))
  (general-define-key
   :keymaps 'eglot-mode-map
   :states '(normal motion)
   "K" #'eldoc-box-eglot-help-at-point)
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (fset #'jsonrpc--log-event #'ignore)
  (eglot-sync-connect nil)
  (eglot-send-changes-idle-time 3)
  (eglot-idle-delay 0.2)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-connect-timeout nil))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0))

(defun mw/eglot-managed-mode-initialize ()
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1)
  (setq-local eglot-documentation-functions '(eglot-signature-eldoc-function t))
  (general-define-key
   :states '(normal motion)
   :keymaps 'eglot-mode-map
   "K" (lambda ()
         (interactive)
         (funcall #'evil-lookup)))
  )

(defun mw/eglot-specific-eldoc ()
  (setq-local eldoc-documentation-functions
              (list #'eglot-signature-eldoc-function
                    #'eglot-hover-eldoc-function
                    t
                    #'flymake-eldoc-function)))

(use-package eglot-signature-eldoc-talkative
  :init
  (add-hook 'eglot-managed-mode-hook #'mw/eglot-specific-eldoc))

(use-package eglot-x
  :after eglot
  :ensure (eglot-x :host github
		           :repo "nemethf/eglot-x")
  :config (eglot-x-setup))

(use-package eglot-booster
  :after eglot
  :ensure
  (eglot-booster :host github
                 :repo "jdtsmith/eglot-booster")
  :config
  (eglot-booster-mode))

(use-package eldoc-box
  :after eglot
  :config
  (setopt evil-lookup-func #'eldoc-box-help-at-point)
  (setopt eldoc-box-position-function #'eldoc-box--default-at-point-position-function)
  (advice-add 'eldoc-box--update-childframe-geometry :around #'mw/eldoc-box-update-childframe-geometry)
  (eldoc-box--enable)
  :custom
  (eldoc-box-clear-with-C-g t))

(defun mw/evil-lookup-dwim ()
  (interactive)
  (cond
   ((and lsp-mode lsp-ui-doc-mode)
    (lsp-ui-doc-glance))

   (t (funcall #'eldoc-box-help-at-point))))

(defvar mw/lisp-modes-hooks '(emacs-lisp-mode-hook
                              clojure-mode-hook
                              clojurescript-mode-hook))


(defun mw/add-hook-to-lisp-modes (function)
  "Add FUNCTION to every hook defined in `mw/lisp-modes-hooks'."
  (dolist (hook mw/lisp-modes-hooks)
    (add-hook hook function)))

(use-package aggressive-indent
  :init
  (mw/add-hook-to-lisp-modes 'aggressive-indent-mode))

(use-package evil-cleverparens
  :after evil
  :init
  (mw/add-hook-to-lisp-modes 'evil-cleverparens-mode)
  :config
  (evil-define-key '(insert motion visual) 'evil-cleverparens-mode-map
    (kbd "<delete>") 'paredit-forward-delete)
  (mw/local-leader-def emacs-lisp-mode-map
                       "e"  `("eval" . ,(make-sparse-keymap))
                       "eb" '("buffer" . eval-buffer)
                       "er" '("region" . eval-region)
                       "ex" '("last sexp" . eval-last-sexp)))

(use-package yaml-mode
  :disabled)
(use-package restclient)
(use-package markdown-mode
  :disabled)

(use-package sql-indent
  :hook
  (sql-mode-hook . sqlind-minor-mode))

;; (use-package sqlformat)
;; (reformatter-define sql-format
;;   :program "sqlfluff"
;;   :args '("format" "-"))

(use-package nix-mode
  :after eglot
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :config
  (mw/local-leader-def nix-mode-map
                       "="  `("format" . ,(make-sparse-keymap))
                       "=b" '("buffer" . nix-format-buffer))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  :hook
  (nix-mode . eglot-ensure))

(use-package ebnf-mode)

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(provide 'mw-dev)
;;; mw-dev.el ends here
