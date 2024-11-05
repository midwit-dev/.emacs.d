;;; mw-evil.el --- Evil setup                        -*- lexical-binding: t; -*-

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

(defun mw/esc-command ()
  (interactive)
  (let ((ring-bell-function (lambda ())))
    (evil-ex-nohighlight)
    (evil-force-normal-state)))

(use-package evil
  :ensure t
  :demand t
  :autoload (evil-global-set-key
             evil-set-initial-state)
  :init (setq evil-want-integration t
              evil-want-C-u-scroll t
              evil-inhibit-esc t
              evil-want-keybinding nil
              evil-symbol-word-search t)
  :config (progn
            (evil-mode 1)
            (blink-cursor-mode -1)
            (evil-define-key 'normal 'global-map (kbd "<escape>") #'mw/esc-command)

            (evil-global-set-key 'normal "U" 'evil-redo)
            (evil-set-initial-state 'messages-buffer-mode 'normal)
            (evil-set-initial-state 'dashboard-mode 'normal)
            (setq evil-mode-line-format '(before . mode-line-front-space)))
  :custom
  (evil-undo-system 'undo-redo)
  (evil-goto-definition-functions '(evil-goto-definition-xref
                                    evil-goto-definition-imenu
                                    evil-goto-definition-semantic
                                    evil-goto-definition-search)))

(use-package evil-org
  :ensure (evil-org :type git :host github :repo "Somelauw/evil-org-mode")
  :after org
  :hook (org-mode . (lambda () (evil-org-mode 1)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-mc)

(use-package evil-goggles
  :ensure t
  :after (evil evil-cleverparens)
  :init
  (setq evil-goggles-duration 0.1)
  :config
  (push '(evil-operator-eval :face evil-goggles-yank-face
                             :switch evil-goggles-enable-yank
                             :advice evil-goggles--generic-async-advice)
        evil-goggles--commands)
  (add-to-list 'evil-goggles--commands
               '(evil-cp-yank :face evil-goggles-yank-face
                              :switch evil-goggles-enable-yank
                              :advice evil-goggles--generic-async-advice))
  (add-to-list 'evil-goggles--commands
               '(evil-cp-yank-line :face evil-goggles-yank-face
                                   :switch evil-goggles-enable-yank
                                   :advice evil-goggles--generic-async-advice))
  (add-to-list 'evil-goggles--commands
               '(evil-cp-yank-sexp :face evil-goggles-yank-face
                                   :switch evil-goggles-enable-yank
                                   :advice evil-goggles--generic-async-advice))
  (add-to-list 'evil-goggles--commands
               '(evil-cp-yank :face evil-goggles-yank-face
                              :switch evil-goggles-enable-yank
                              :advice evil-goggles--generic-async-advice))
  (add-to-list 'evil-goggles--commands
               '(evil-cp-yank :face evil-goggles-yank-face
                              :switch evil-goggles-enable-yank
                              :advice evil-goggles--generic-async-advice))
  (add-to-list 'evil-goggles--commands
               '(evil-cp-delete :face evil-goggles-delete-face
                                :switch evil-goggles-enable-delete
                                :advice evil-goggles--delete-line-advice))
  (add-to-list 'evil-goggles--commands
               '(evil-cp-delete-line :face evil-goggles-delete-face
                                     :switch
                                     evil-goggles-enable-delete
                                     :advice evil-goggles--delete-line-advice))
  (add-to-list 'evil-goggles--commands
               '(evil-cp-change :face evil-goggles-change-face
                                :switch evil-goggles-enable-change
                                :advice evil-goggles--generic-blocking-advice))
  (add-to-list 'evil-goggles--commands
               '(evil-cp-change-line :face evil-goggles-change-face
                                     :switch evil-goggles-enable-change
                                     :advice evil-goggles--generic-blocking-advice))
  (add-to-list 'evil-goggles--commands
               '(evil-cp-change-sexp :face evil-goggles-change-face
                                     :switch evil-goggles-enable-change
                                     :advice evil-goggles--generic-blocking-advice))
  (add-to-list 'evil-goggles--commands
               '(evil-cp-change-enclosing :face evil-goggles-change-face
                                          :switch evil-goggles-enable-change
                                          :advice evil-goggles--generic-blocking-advice))
  (add-to-list 'evil-goggles--commands
               '(evil-cp-change-whole-line :face evil-goggles-change-face
                                           :switch evil-goggles-enable-change
                                           :advice evil-goggles--generic-blocking-advice))
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; https://www.reddit.com/r/emacs/comments/des3cl/how_do_you_create_nested_menus_with_general/f2yw45k/
(defmacro general-global-menu-definer (def &rest body)
  "Create a definer named mw/global-DEF-def wrapping mw/global-leader-def.
The prefix map is named 'mw/global-DEF-map'."
  `(progn
     (general-create-definer ,(intern (concat "mw/global-" def "-def"))
       :wrapping mw/global-leader-def
       :prefix-map (quote ,(intern (concat "mw/global-" def "-map")))
       :prefix-command (quote ,(intern (concat "mw/global-" def "-command")))
       :keymaps (quote ,(intern (concat "mw/global-" def "-map")))
       ;; :wk-full-keys nil
       "" '(:ignore t ))
     (,(intern (concat "mw/global-" def "-def"))
      ,@body)))

(use-package general
  :ensure (:wait t)
  :demand t
  :autoload (general-evil-setup general-auto-unbind-keys general-define-key)
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)

  (general-define-key
   :states '(normal insert motion visual emacs)
   :keymaps 'override
   :prefix-map 'global-leader-map
   :prefix "SPC"
   :non-normal-prefix "M-SPC")

  (general-create-definer mw/global-leader-def :keymaps 'global-leader-map)
  (mw/global-leader-def "" nil)

  (general-global-menu-definer "apps")
  (general-global-menu-definer "buffers")
  (general-global-menu-definer "emacs")
  (general-global-menu-definer "org")
  (general-global-menu-definer "files")
  (general-global-menu-definer "project")
  (general-global-menu-definer "narrow")
  (general-global-menu-definer "text")
  (general-global-menu-definer "git")
  (general-global-menu-definer "error")

  (general-create-definer mw/local-leader-def
    :states '(normal insert motion visual emacs)
    :keymaps 'override
    :major-modes t
    :prefix ","
    :non-normal-prefix "M-,")
  (mw/local-leader-def "" nil)

  (general-def universal-argument-map'
    "SPC u" universal-argument-more)

  (mw/global-buffers-def
    "b" '("bury"                  . bury-buffer)
    "B" '("unbury"                . unbury-buffer)
    "s" '("switch"                . switch-to-buffer)
    "k" '("kill"                  . kill-current-buffer)
    "K" '("kill w/  window"       . kill-buffer-and-window)
    "S" '("scratch"               . scratch-buffer))

  (mw/global-emacs-def
    "R"  `("recursive"             . ,(make-sparse-keymap))
    "Ra" '("abort"                 . abort-recursive-edit)
    "Re" '("exit"                  . exit-recursive-edit)

    "d"  `("direnv"                . ,(make-sparse-keymap))
    "da" '("allow"                 . direnv-allow)
    "du" '("update"                . direnv-update-directory-environment)

    "p"  `("pkgs"                  . ,(make-sparse-keymap))
    "pt" '("try"                   . elpaca-try)
    "pu" '("update "               . elpaca-update)
    "pU" '("update all"            . elpaca-update-all)
    "pv" '("visit package"         . elpaca-visit-package)
    "pf" '("fetch"                 . elpaca-fetch)
    "pF" '("fetch all"             . elpaca-fetch-all)
    "pg" '("gui"                   . elpaca-manager)
    "k"                            #'save-buffers-kill-emacs


    "t"  `("toggle"                . ,(make-sparse-keymap))
    "td" '("debug-on-error"        . toggle-debug-on-error)

    "y"  `("snippets"              . ,(make-sparse-keymap))
    "yn" '("new"                   . yas-new-snippet)
    )

  (mw/global-files-def
    "d" '("dired"                  . dired)
    "o" '("open (find)"            . find-file)
    "i" '("insert"                 . insert-file)
    "s" '("save"                   . save-buffer)
    "S" '("save all"               . evil-write-all)
    "r" '("recentf"                 . recentf)
    "y" '("copy name"              . (lambda () (interactive) (kill-new (buffer-file-name))))
    )

  (mw/global-narrow-def
    "r" '("region"                 . narrow-to-region)
    "p" '("page"                   . narrow-to-page)
    "f" '("function"               . narrow-to-defun)
    "w" '("function"               . widen)
    )

  (mw/global-text-def
    "i" '("ins char"               . insert-char)
    "I" '("ins nerd"               . nerd-icons-insert)
    )

  (mw/global-apps-def
    "f"   '("elfeed"               . elfeed)
    "i"   '("irc"                  . erc-tls)
    "m"   '("mail"                 . mu4e)
    "k"   '("slack"                . slack-start)
    "p"   '("pass"                 . pass)
    "s"   '("vterm (shell)"        . vterm))

  (mw/global-git-def
    "s"   '("status"               . magit-status)
    "c"   `("conflict"             . ,(make-sparse-keymap))
    "ckc" '("keep current"         . smerge-keep-current)
    "cka" '("keep all"             . smerge-keep-all)
    "ckb" '("keep base"            . smerge-keep-base)
    "ckl" '("keep lower"           . smerge-keep-lower)
    "cku" '("keep upper"           . smerge-keep-upper)
    "cn"  '("next"                 . smerge-next)
    )

  (mw/global-leader-def
    "SPC" '("M-x"                   . execute-extended-command)
    "TAB" '("latest buffer"         . mode-line-other-buffer)
    "!"   '("sh"                    . shell-command)
    "'"   '("async sh"              . async-shell-command)
    "u"   '("C-u"                   . universal-argument)
    "h"  (general-simulate-key "C-h")
    "i"  'consult-imenu
    "a" '("apps" . mw/global-apps-command)
    "b" '("buffers" . mw/global-buffers-command)
    "E" '("emacs" . mw/global-emacs-command)
    "o" '("org" . mw/global-org-command)
    "f" '("files" . mw/global-files-command)
    "n" '("narrow".  mw/global-narrow-command)
    "t" '("text" . mw/global-text-command)
    "g" '("git" . mw/global-git-command)
    "p" '("project" . mw/global-project-command)
    "e" '("errors" . mw/global-error-command))

  (general-define-key
   :states 'normal
   "C-w N" 'make-frame-command
   "C-w C" 'delete-frame)

  (general-define-key
   :states 'motion
   "#"      'evil-ex-search-word-backward
   "*"      'evil-ex-search-word-forward
   "/"      'evil-ex-search-forward
   "?"      'evil-ex-search-backward
   "N"      'evil-ex-search-previous
   "n"      'evil-ex-search-next
   "g#"     'evil-ex-search-unbounded-word-backward
   "g*"     'evil-ex-search-unbounded-word-forward
   "<f9>"   '("cycle erc channels"       . mw/erc-cycle-channel-buffers)
   "C-<f9>" '("goto buffer before cycle" . mw/erc-cycle-channel-init-buffer))

  (define-key evil-normal-state-map "ZZ" 'mw/evil-save-modified-and-close)
  )

(evil-define-command mw/evil-save-modified-and-close (file &optional bang)
  "Save the current buffer and close the window."
  :repeat nil
  (interactive "<f><!>")
  (when (buffer-modified-p)
    (evil-write nil nil nil file bang))
  (bury-buffer)
  (delete-window))


(use-package evil-collection
  :ensure t
  :demand t
  :commands (evil-collection-init)
  :after (evil)
  :custom (evil-collection-setup-minibuffer nil)
  :init
  (setq forge-add-default-bindings nil)
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :demand t
  :commands (evil-commentary-mode)
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :ensure t
  :commands (global-evil-surround-mode)
  :config
  (general-define-key
   (:states 'operator
            "s" 'evil-surround-edit
            "S" 'evil-surround-edit)
   (:states 'visual
            "S" 'evil-surround-region
            "gS" 'evil-Surround-region)
   (global-evil-surround-mode 1)))

(use-package targets
  :after evil
  :ensure (targets :host github
                   :repo "noctuid/targets.el")
  :config
  (targets-setup t))

(provide 'mw-evil)
;;; mw-evil.el ends here
