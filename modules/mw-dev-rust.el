;;; mw-dev-rust.el --- Rust dev setup                -*- lexical-binding: t; -*-

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

(require 'treesit)
(require 's)
(require 'dash)
(require 'eglot)

(defvar mw/rust-last-compile-commands '())

(defun mw/rust-compile-function (command)
  "Create function to run `compile' using COMMAND."
  (lambda ()
    (interactive)
    (let ((compile-command (compilation-read-command command))
          (compilation-read-command nil))
      (delete command mw/rust-last-compile-commands)
      (setf (alist-get command mw/rust-last-compile-commands) compile-command)
      (call-interactively #'compile compile-command))))

(defun mw/rust-retest ()
  (interactive)
  (if-let ((cmd (cdr (assoc "cargo test" mw/rust-last-compile-commands))))
      (funcall #'compile cmd)
    (let ((compile-command "cargo test"))
      (call-interactively #'compile))))


(defun mw/cargo-add ()
  "Run =cargo add= command."
  (interactive)
  (let* ((cmd-prefix "cargo add ")
         (cmd (s-trim (read-string cmd-prefix))))
    (when (< 0 (length cmd))
      (async-shell-command (format "%s%s" cmd-prefix cmd)))))

(defvar rust-things '("let_declaration" "match_expression" "match_arm" "return_expression" "if_expression"))

(defun mw/rust-goto-parameters ()
  "Move point to the parameter list of the function definition at point."
  (interactive)
  (when-let ( (fn-at-point
               (treesit-defun-at-point)))
    (if-let ((parameters (treesit-query-capture fn-at-point '(parameters: (parameters) @parameter))))
        (goto-char (+ 1 (treesit-node-start (cdr (assoc 'parameter parameters))))))))

(defun mw/rust-goto-return-type ()
  "Move point to the return type of the function definition at point.

If the function definition at point does not have a return type,
move point to the start of the body block instead."
  (interactive)
  (when-let ( (fn-at-point
               (treesit-defun-at-point)))
    (if-let ((return-type (treesit-query-capture fn-at-point '( ("->" (type_identifier) @identifier)))))
        (goto-char (treesit-node-start (cdr (assoc 'identifier return-type))))
      ;; Move to start of body block if fn is missing return statement
      (goto-char (treesit-node-start (cdr (assoc 'block (treesit-query-capture fn-at-point '(body: (block) @block))))))
      )))

(defun mw/rust-next-parameter ()
  (interactive)
  (mw/treesit-goto-next-thing (treesit-query-capture (treesit-buffer-root-node) '((parameters (parameter) @parameter)))))

(defun mw/rust-previous-parameter ()
  (interactive)
  (mw/treesit-goto-next-thing (treesit-query-capture (treesit-buffer-root-node) '((parameters (parameter) @parameter))) t))

(defun mw/rust-next-return-type-identifier ()
  (interactive)
  (mw/treesit-goto-next-thing (treesit-query-capture (treesit-buffer-root-node) '("->" (type_identifier) @identifier))))

(defun mw/rust-previous-return-type-identifier ()
  (interactive)
  (mw/treesit-goto-next-thing (treesit-query-capture (treesit-buffer-root-node) '("->" (type_identifier) @identifier)) t))

(setq md-code-query '((fenced_code_block
                       ((info_string ((language) @lang))
                        ((code_fence_content) @content)))))

(defun extract-markdown-code-contents (md-string lang)
  "Parse MD-STRING, extracting all code blocks in LANG."
  (let ((root-node (treesit-parse-string md-string 'markdown)))
    (--map
     (let* ((lang-bounds (-first-item it))
            (snippet-lang (substring md-string
                                     (- (car lang-bounds) 1)
                                     (- (cdr lang-bounds) 1)))
            (code-bounds (-second-item it)))
       (when (string= snippet-lang lang)
         (substring md-string
                    (- (car code-bounds) 1)
                    (- (cdr code-bounds) 1))))
     (-partition 2 (treesit-query-range root-node md-code-query)))))

(defun +highligh-string (s lang)
  (treesit-parse-string s lang)
  )

(defun mw/rust-eldoc-echo-fix (content)
  (let* ((rust-code-blocks (extract-markdown-code-contents
                            (s-append "\n" (plist-get content :value))
                            "rust"))

         (snippets (s-join "\n" ;; join code
                           (--map (cl-first (and it
                                                 (s-lines (eglot--hover-info it))))
                                  (take 2 rust-code-blocks)))))

    ;; (s-join "\n" (-remove 's-blank? (s-lines snippets)))
    (thread-last
      snippets
      (s-lines)
      (-remove 's-blank?)
      (s-join "\n"))
    ))

(defun mw/rust-eglot-hover-eldoc-function (cb)
  "A member of `eldoc-documentation-functions', for hover."
  (when (eglot-server-capable :hoverProvider)
    (let ((buf (current-buffer)))
      (jsonrpc-async-request
       (eglot--current-server-or-lose)
       :textDocument/hover (eglot--TextDocumentPositionParams)
       :success-fn (eglot--lambda ((Hover) contents range)
                     (eglot--when-buffer-window buf
                       (let* ((info (unless (seq-empty-p contents)
                                      (eglot--hover-info contents range)))
                              (echo-info (and (not (seq-empty-p contents))
                                              (mw/rust-eldoc-echo-fix contents))))
                         (funcall cb info
                                  :echo echo-info))))
       :deferred :textDocument/hover))
    (eglot--highlight-piggyback cb)
    t))



(defun +rust-eglot-eldoc-setup ()
  ;; (advice-add #'eglot-hover-eldoc-function
  ;;             :override
  ;;             #'mw/rust-eglot-hover-eldoc-function)
  (setq-local eldoc-documentation-functions
              (list #'eglot-signature-eldoc-function
                    #'mw/rust-eglot-hover-eldoc-function
                    't)))

(defun mw/remove-rust-ts-flymake ()
  (setq flymake-diagnostic-functions (delete 'rust-ts-flymake flymake-diagnostic-functions)))

(use-package rust-mode
  :mode ("\\.rs\\'")
  :ensure (:wait t)
  :after general
  :hook (rust-mode . eglot-ensure)
  :init
  ;; (add-hook 'rust-mode-hook #'prettify-symbols-mode)
  (add-hook 'rust-mode-hook #'mw/remove-rust-ts-flymake)
  :config
  :custom
  (rust-mode-treesitter-derive t)
  (rust-format-show-buffer nil) ;; dont show buffer on format error
  (rust-format-goto-problem nil) ;; dond jump to problem on format error
  (rust-ts-flymake-command nil)
  :general
  (mw/local-leader-def '( rust-ts-mode-map rustic-mode-map)
    "c"  `(make-sparse-keymap "cargo")
    "ca" '("add" . mw/cargo-add)
    "cb"  `("build" . ,(mw/rust-compile-function "cargo build"))
    "cf" '("find crate" . eeglot-x-find-crate)
    "cF" '("find crate in deps" . eglot-x-find-crate)
    "cg"  '("crate graph" . eglot-x-view-crate-graph)
    "cr"  `("run" . ,(mw/rust-compile-function "cargo run"))
    "ct"  `("test" . ,(mw/rust-compile-function "cargo test"))
    "cR"  `("build (release)" . ,(mw/rust-compile-function "cargo build --release"))
    "d"   '("doc in browser" . eglot-x-open-external-documentation)
    "R"   '("runnables" . eglot-x-ask-runnables)
    "g"   `(make-sparse-keymap "goto")
    "gp"  '("goto parameters" . mw/rust-goto-parameters)
    "gr"  '("goto return type" . mw/rust-goto-return-type)
    "m"   `(make-sparse-keymap "+macro")
    "me"   '("expand macro" . eglot-x-expand-macro)
    "mr"   '("rebuild macro" . eglot-x-expand-macro)
    "t"   '("rerun last test" . mw/rust-retest)
    ))

(use-package flymake-clippy
  :disabled t
  :demand t
  :hook (rust-mode . flymake-clippy-setup-backend)
  )


(provide 'mw-dev-rust)
;;; mw-dev-rust.el ends here
