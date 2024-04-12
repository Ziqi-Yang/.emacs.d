;;; l-languages.el --- languages configurations  -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Meow King <mr.meowking@anche.no>

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; languages configurations

;;; Code:

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))

;;; CC =========================================================================

(with-eval-after-load 'c-ts-mode
  (keymap-unset c-ts-base-mode-map "C-c C-c")
  (keymap-set c-ts-base-mode-map "C-c C-c m" #'mk/better-consult-man)
  (keymap-set c-ts-base-mode-map "C-c C-c f" #'ff-find-other-file)
  (keymap-set c-ts-base-mode-map "C-c C-c p" #'c-ts-prototype-copy-proto))

(use-package c-ts-prototype
  :ensure (:type git :host sourcehut :repo "meow_king/c-ts-protoype"))

(setq c-ts-mode-indent-style 'linux)

;; Nix =========================================================================
(use-package nix-mode
  :mode "\\.nix\\'")

;;; Draw =======================================================================
(use-package plantuml-mode
  :ensure (:type git :host github :repo "xshyamx/simple-plantuml-mode"))

(use-package mermaid-ts-mode
  :ensure (:type git :host github :repo "JonathanHope/mermaid-ts-mode"))
(add-to-list 'auto-mode-alist '("\\.\\(mermaid\\|mmd\\)\\'" . mermaid-ts-mode))

;; (use-package d2-mode)
;; (add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode))

(defun mk/draw-local-keybinding-setup()
  (keymap-local-set "C-c C-c c" #'mermaid-compile)
  (keymap-local-set "C-c C-c b" #'mermaid-open-browser)
  (keymap-local-set "C-c C-c d" #'mermaid-open-doc))

(add-hook 'mermaid-mode-hook 'mk/draw-local-keybinding-setup)

;;; Java =======================================================================
(defun mk/java-open-doc()
  "Open java17 documentation."
  (interactive)
  (browse-url "https://docs.oracle.com/en/java/javase/17/docs/api/index.html"))

(defun mk/java-generate-tags ()
  "Generate tags file for current java project."
  (interactive)
  (let ((compile-command "make gen_tags"))
    (project-compile)))

;; ;; enable cape-dabbrev and cape-keyword for java-mode and java-ts-mode
;; (dolist (mode '(java-mode-hook java-ts-mode-hook))
;;   (add-hook mode
;;     '(lambda ()
;;        (setq-local completion-at-point-functions
;;          (append completion-at-point-functions '(cape-dabbrev cape-keyword))))))

(defun mk/java-local-keybinding-setup()
  (keymap-local-set "C-c C-c d" #'mk/java-open-doc)
  (keymap-local-set "C-c C-c t" #'mk/java-generate-tags))

(use-package java-ts-mode
  :ensure nil
  :custom (java-ts-mode-indent-offset 2))

(add-hook 'java-mode-hook 'mk/java-local-keybinding-setup)
(add-hook 'java-ts-mode-hook 'mk/java-local-keybinding-setup)


;;; Kotlin =====================================================================
(use-package kotlin-ts-mode
  :ensure (:host gitlab :repo "bricka/emacs-kotlin-ts-mode")
  :mode "\\.kt\\'")

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
    `(kotlin-ts-mode . ("kotlin-language-server"))))

;;; Lisp =======================================================================
(defun others/eval-buffer ()
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

(defun mk/emacs-lisp-local-keybindings-setup()
  "Set up local keybindings for scratch buffer(lisp interaction mode)"
  (keymap-set emacs-lisp-mode-map "TAB" #'indent-for-tab-command)
  (keymap-local-set "TAB" #'completion-at-point)
  (keymap-local-set "C-j" #'completion-at-point)
  (keymap-local-set "C-i" #'eval-print-last-sexp)
  (keymap-local-set "C-x C-S-e" #'others/eval-buffer))

(add-hook 'lisp-interaction-mode-hook 'mk/emacs-lisp-local-keybindings-setup)
(add-hook 'emacs-lisp-mode-hook 'mk/emacs-lisp-local-keybindings-setup)

;;; Makefile ===================================================================
(defun mk/makefile-local-keybinding-setup()
  (keymap-local-set "<tab>" #'(lambda () (insert (kbd "TAB")))))

(add-hook 'make-mode 'mk/makefile-local-keybinding-setup)


;;; Makefile ===================================================================
;; C-c ' to edit code block like in org
(use-package edit-indirect)

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init
	;; export function(need to install external program)
	(setq markdown-command "multimarkdown"))

;;; Python =====================================================================
(with-eval-after-load
  (setq python-shell-interpreter "python"
    python-shell-interpreter-args "-i"))

(with-eval-after-load 'python
  (keymap-unset python-mode-map "C-c C-c")
  (keymap-set python-mode-map "C-c C-c o" #'python-fix-imports)

  (keymap-unset python-ts-mode-map "C-c C-c")
  (keymap-set python-ts-mode-map "C-c C-c o" #'python-fix-imports))

;;; Rust =======================================================================
(use-package cargo
  :ensure (:host github :repo "kwrooijen/cargo.el")
  :custom
  (cargo-process--command-search "search --registry crates-io")
  :config
  (add-hook 'cargo-process-mode-hook #'(lambda () (setq-local truncate-lines nil))))

;; (defun mk/cargo-crate-info (crate-name)
;;   "Get the basic information of a crate. The underlying command is =cargo add --dry-run crate="
;;   (interactive (list (read-string "crate name: " (thing-at-point 'symbol))))
;;   (async-shell-command (concat "cargo add --dry-run " crate-name)))

(with-eval-after-load 'rust-ts-mode
  
  (keymap-set rust-ts-mode-map "C-c C-c" #'mk/trans-map/cargo))

(with-eval-after-load 'toml-ts-mode
  (keymap-set toml-ts-mode-map "C-c C-c" #'mk/trans-map/cargo))

(defun mk/rust/setup ()
  (setq-local electric-pair-pairs
    (add-to-list 'electric-pair-pairs '(?| . ?|))))

(add-hook 'rust-ts-mode-hook #'mk/rust/setup)

(defun mk/add-rust-search-engine()
  "Add search engine in addition to mk/search-engines when in rust."
  (push '("crate.io" . "https://crates.io/crates/%s") mk/search-engines)
  (push '("docs.rs" . "https://docs.rs/%s/") mk/search-engines)
  (push '("rust-std" . "file:///home/zarkli/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/share/doc/rust/html/std/index.html?search=%s") mk/search-engines))

(add-hook 'toml-ts-mode-hook 'mk/add-rust-search-engine)
(add-hook 'rust-ts-mode-hook 'mk/add-rust-search-engine)
;; (add-hook 'rust-mode-hook 'mk/add-rust-search-engine)

;;; Typst ======================================================================
(use-package outline-indent-mode
  :ensure (:type git :host sourcehut :repo "meow_king/outline-indent-mode"))
(use-package typst-ts-mode
  :ensure (:type git :host sourcehut :repo "meow_king/typst-ts-mode" :branch "develop" :files (:defaults "*.el"))
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(typst-ts-mode . ("typst-lsp"))))


;;; Zig ========================================================================
(use-package zig-mode
  :ensure (:type git :host github :repo "ziglang/zig-mode")
  :config
  (setq zig-format-on-save nil)
  
  (keymap-set zig-mode-map "C-c C-c c" #'zig-compile)
  (keymap-set zig-mode-map "C-c C-c C-b" #'zig-build-exe)
  (keymap-set zig-mode-map "C-c C-c C-l" #'zig-build-lib)
  (keymap-set zig-mode-map "C-c C-c C-o" #'zig-build-obj)
  (keymap-set zig-mode-map "C-c C-c t" #'zig-test-buffer)
  (keymap-set zig-mode-map "C-c C-c r" #'zig-run)
  (keymap-set zig-mode-map "C-c C-c f" #'zig-format-buffer))


;;; Go =========================================================================
(with-eval-after-load 'go-ts-mode
  (setq-default go-ts-mode-indent-offset 4))


;;; micc =======================================================================
(use-package crystal-mode)
(use-package lua-mode)
(use-package fish-mode)
(use-package just-mode)

(provide 'l-languages)

;;; l-languages.el ends here
