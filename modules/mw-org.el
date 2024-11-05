;;; mw-org.el --- Org mode setup                     -*- lexical-binding: t; -*-

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

(require 'mw-evil)

(defun transform-square-brackets-to-round-ones (string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform)))

(use-package org-capture
  :ensure nil
  :hook (org-capture-mode . evil-insert-state)
  :custom
  (org-capture-templates
   `(("i" "Inbox" entry (file "inbox.org")
      "* %^{A few words}\n%U\n%?\n")
     ("t" "Task" entry (file "inbox.org")
      "* TODO %?\n%U\n\n")
     ("j" "Journal entry" entry (file+olp+datetree "journal.org")
      "* %?\n%U"
      :tree-type week
      :prepend t)
     ("P" "Protocol" entry (file+headline "inbox.org" "Web links")
      "* %^{Title}\nSource: %u, %:link\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?")
     ("L" "Protocol Link" entry (file+headline "inbox.org" "Web links")
      "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n"
      :no-save t)
     ("c" "Contacts" entry (file "contacts.org")
      "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")
     ("n" "New note (with Denote)" plain
      (file denote-last-path)
      #'denote-org-capture
      :no-save t
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t))))

(use-package org-agenda
  :ensure nil
  :custom
  (org-agenda-custom-commands
   `(("d" "Dashboard"
      ((agenda ""
               ((org-agenda-overriding-header "Tasks for today")
                (org-agenda-span 'day)
                (org-agenda-start-day "+0d")
                (org-agenda-use-time-grid nil)
                (org-scheduled-today)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-deadline-warning-days 1)
                (org-deadline-past-days 0)
                (org-scheduled-past-days 0)))

       (todo "STARTED"
             ((org-agenda-overriding-header "Started tasks")))
       (todo ""
             ((org-agenda-overriding-header "Missed tasks")
              (org-agenda-todo-ignore-deadlines 'future)
              (org-agenda-todo-ignore-scheduled 'future)
              (org-agenda-skip-function 'mw/org-agenda-skip-unless-missed)))))

     ("m" "Missed")

     ("T" "Today"
      ((agenda ""
               ((org-agenda-overriding-header "Tasks for today")
                (org-agenda-span 'day)
                ;; (org-agenda-time-grid nil)
                (org-agenda-start-day "+0d")
                (org-scheduled-today)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-deadline-past-days 1)
                (org-scheduled-past-days 1)))))

     ("r" "Review week"
      ((agenda ""
               ((org-agenda-overriding-header "Weekly review")
                (org-agenda-show-all-dates t)
                (org-agenda-start-with-log-mode t)
                (org-agenda-start-with-clockreport-mode t)
                (org-agenda-span 'week)
                (org-agenda-start-day "-1d")
                (org-agenda-use-time-grid nil)
                ;; start on monday
                ;; (org-agenda-start-on-weekday 1)
                (org-scheduled-past-days 6)
                (org-deadline-past-days 6)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONTDO")))
                (org-agenda-skip-function)
                )))))))

(use-package org-refile
  :ensure nil
  :custom
  (org-refile-targets
   `((
      ,(cons '("todo.org"
               "bookmarks.org"
               "feeds.org"
               "inbox.org")
             '(:maxlevel . 3))
      (nil . (:maxlevel . 3))))))

(use-package org-protocol
  :ensure nil)

(use-package org
  :ensure nil
  :after general
  :init
  (setq plantuml-exec-mode 'plantuml)
  :config
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-crypt)
  (add-to-list 'org-src-lang-modes '("js" . js-ts))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  ;; Unset keybindings to add and remove files to org-agenda-files
  :bind (("C-c [" . nil)
         ("C-c ]" . nil))
  :custom
  (org-directory "~/org")
  ;; refile to top
  (org-reverse-note-order t)
  (org-structure-template-alist '(("s" . "src")
                                  ("c" . "src clojure")
                                  ("e" . "src emacs-lisp :lexical t")
                                  ("E" . "src emacs-lisp :tangle FILENAME")
                                  ("g" . "src go")
                                  ("j" . "src js")
                                  ("n" . "src nix")
                                  ("o" . "src org")
                                  ("r" . "src rust")
                                  ("a" . "ai")))

  (org-src-content-indentation 2)
  (org-startup-folded 'content)
  (org-imenu-depth 2)
  (org-agenda-files '("todo.org" "inbox.org"))
  (org-M-RET-may-split-line '((default . nil)))
  (org-startup-indented t)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n!)" "STARTED(s!/@)" "SOMEDAY(S!)" "|" "DONE(d!)" "DONTDO(D@)")))
  (org-tags-column -110)
  (org-ellipsis nil)
  (org-hide-emphasis-markers t)
  (org-log-done 'time)
  (org-blank-before-new-entry '((heading . nil)
                                (plain-list-item . nil)))
  (org-cycle-separator-lines 2)
  (org-log-into-drawer t)
  (org-clock-persist t)
  (org-clock-out-remove-zero-time-clocks t)
  ;; use default ("<file>_archive.org")
  ;; (org-archive-location "archive.org")
  (org-clock-out-when-done t)
  :general
  (mw/local-leader-def '(motion normal) org-mode-map
			           "s"  (make-sparse-keymap)
			           "sr" 'org-refile
			           "sc" 'org-cut-subtree
			           "sn" 'org-toggle-narrow-to-subtree
			           "se" 'denote-org-extras-extract-org-subtree

			           "i" `("insert" . ,(make-sparse-keymap))
			           "ip" 'org-set-property
			           "ie" 'org-set-effort
			           "it" 'org-set-tags-command

			           "r"   (make-sparse-keymap)
			           "rn" '("refile to note" . mw/refile-to-note)
			           "rr" '("refile" . org-refile)

			           "c" `("clock" . ,(make-sparse-keymap))
			           "ci" 'org-clock-in
			           "co" 'org-clock-out

			           "d" `("date" . ,(make-sparse-keymap))
			           "dd" 'org-deadline
			           "ds" 'org-schedule)
  (mw/global-org-def
    "c" '("capture" . org-capture)
    "a" '("agenda" . org-agenda)
    "l" '("store link" . org-store-link)
    "n" '("open note" . consult-notes)
    "s" '("search" . org-search-view)
    "g" `("goto" ,(make-sparse-keymap))
    "gc" '("Go to the currently clocked-in entry, or to the most recently clocked one." . org-clock-goto)
    "gi" '("inbox.org" . (lambda ()
			               (interactive)
			               (find-file (expand-file-name "inbox.org" org-directory))))
    "gb" '("bookmark.org" . (lambda ()
			                  (interactive)
			                  (find-file (expand-file-name "bookmark.org" org-directory))))
    "gt" '("todo.org" . (lambda ()
			              (interactive)
			              (find-file (expand-file-name "todo.org" org-directory))))
    "gj" '("journal.org" . (lambda ()
			                 (interactive)
			                 (find-file (expand-file-name "journal.org" org-directory))))
    "gr" '("last refiled" . org-refile-goto-last-stored)
    "gc" '("last captured" . org-capture-goto-last-stored)
    ))

(use-package org-appear
  :demand t
  :hook org-mode)

(use-package org-modern
  :config
  (global-org-modern-mode 1))

(use-package org-download
  :after org
  :custom
  ;; TODO: Use other than flameshot
  (org-download-screenshot-method "flameshot")
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package ox-html-stable-ids
  :ensure (ox-html-stable-ids :type git
			                  :host github
			                  :repo "jeffkreeftmeijer/ox-html-stable-ids.el")
  :config
  (org-html-stable-ids-add))

(use-package org-pomodoro
  :after (org general)
  :autoload (org-pomodoro-active-p)
  :demand t
  :custom
  (org-pomodoro-manual-break t)
  (org-pomodoro-audio-player "mpv")
  (org-pomodoro-start-sound (expand-file-name "sounds/sound-theme-freedesktop/dialog-information.oga" user-emacs-directory))
  (org-pomodoro-start-sound-p t)
  (org-pomodoro-finished-sound (expand-file-name "sounds/sound-theme-freedesktop/bell.oga" user-emacs-directory))
  (org-pomodoro-overtime-sound (expand-file-name "sounds/sound-theme-freedesktop/alarm-clock-elapsed.oga" user-emacs-directory))
  (org-pomodoro-killed-sound (expand-file-name "sounds/sound-theme-freedesktop/service-logout.oga" user-emacs-directory))
  (org-pomodoro-killed-sound-p t)
  (org-pomodoro-short-break-sound (expand-file-name "sounds/sound-theme-freedesktop/bell.oga" user-emacs-directory))
  (org-pomodoro-long-break-sound (expand-file-name "sounds/sound-theme-freedesktop/bell.oga" user-emacs-directory))
  ;; (org-pomodoro-ticking-sound (expand-file-name "sounds/sound-theme-freedesktop/bell.oga" user-emacs-directory))
  :config
  (mw/global-org-def
    "p" #'org-pomodoro))

(use-package org-contacts
  :custom
  (org-contacts-files (list (expand-file-name "contacts.org" org-directory))))

(use-package org-alert
  :demand t
  :config
  (org-alert-enable)
  :custom
  (org-alert-interval 300)
  (org-arlert-notify-cutoff 10)
  (org-alert-notify-after-event-cutoff 10))

(provide 'mw-org)
;;; mw-org.el ends here
