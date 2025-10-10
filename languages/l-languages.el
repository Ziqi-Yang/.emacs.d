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

;; (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))

(use-package llvm-mode
  :ensure (:host github :repo "nverno/llvm-mode"))

;;; Lisp Family ================================================================
;; (use-package geiser-guile)

;; gerbil
;; (use-package gambit
;;   :ensure (:host github :repo "gambit/gambit" :files ("misc/gambit.el")))

;; (use-package gerbil-mode
;;   :ensure (:host github :repo "mighty-gerbils/gerbil" :files ("etc/gerbil-mode.el")))

(use-package janet-ts-mode
  :ensure (:host github :repo "sogaiu/janet-ts-mode"))

;;; CC =========================================================================

(with-eval-after-load 'c-ts-mode
  (setq c-ts-mode-indent-style #'gnu)
  (keymap-unset c-ts-base-mode-map "C-c C-c")
  (keymap-set c-ts-base-mode-map "C-c C-c m" #'mk/better-consult-man)
  (keymap-set c-ts-base-mode-map "C-c C-c f" #'ff-find-other-file)
  (keymap-set c-ts-base-mode-map "C-c C-c p" #'c-ts-prototype-copy-proto))

(use-package c-ts-prototype
  :ensure (:type git :host sourcehut :repo "meow_king/c-ts-protoype"))

(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-ts-mode))

;; Nix =========================================================================
(use-package nix-ts-mode
  :after eglot
  :ensure (:type git :host github :repo "nix-community/nix-ts-mode")
  :mode ("\\.nix\\'" . nix-ts-mode))

;;; Draw =======================================================================
;; (use-package plantuml-mode
;;   :ensure (:type git :host github :repo "xshyamx/simple-plantuml-mode"))

(use-package mermaid-ts-mode
  :ensure (:type git :host github :repo "JonathanHope/mermaid-ts-mode")
  :config
  (keymap-set mermaid-ts-mode-map "C-c C-c c" #'mermaid-compile)
  (keymap-set mermaid-ts-mode-map "C-c C-c b" #'mermaid-open-browser)
  (keymap-set mermaid-ts-mode-map "C-c C-c d" #'mermaid-open-doc)
  )
(add-to-list 'auto-mode-alist '("\\.\\(mermaid\\|mmd\\)\\'" . mermaid-ts-mode))

;; (use-package d2-mode)
;; (add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode))

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

(use-package java-ts-mode
  :ensure nil
  :custom (java-ts-mode-indent-offset 2))

(with-eval-after-load 'java-mode
  (keymap-set java-mode-map "C-c C-c d" #'mk/java-open-doc)
  (keymap-set java-mode-map "C-c C-c t" #'mk/java-generate-tags))

(with-eval-after-load 'java-ts-mode
  (keymap-set java-ts-mode-map "C-c C-c d" #'mk/java-open-doc)
  (keymap-set java-ts-mode-map "C-c C-c t" #'mk/java-generate-tags))


;;; Kotlin =====================================================================
(use-package kotlin-ts-mode
  :ensure (:host gitlab :repo "bricka/emacs-kotlin-ts-mode")
  :mode "\\.kt\\'")

;;; Lisp =======================================================================

(with-eval-after-load 'elisp-mode
  (keymap-set emacs-lisp-mode-map "TAB" #'completion-at-point)
  (keymap-set emacs-lisp-mode-map "C-j" #'completion-at-point)
  (keymap-set emacs-lisp-mode-map "C-i" #'eval-print-last-sexp)
  (keymap-set emacs-lisp-mode-map "C-x C-S-e" #'others/eval-buffer)
  
  (keymap-set lisp-interaction-mode-map "TAB" #'completion-at-point)
  (keymap-set lisp-interaction-mode-map "C-j" #'completion-at-point)
  (keymap-set lisp-interaction-mode-map "C-i" #'eval-print-last-sexp)
  (keymap-set lisp-interaction-mode-map "C-x C-S-e" #'others/eval-buffer))


;;; Makefile ===================================================================
(defun mk/makefile-local-keybinding-setup()
  (keymap-local-set "<tab>" #'(lambda () (insert (kbd "TAB")))))

(add-hook 'make-mode 'mk/makefile-local-keybinding-setup)


;;; Makefile ===================================================================
;; C-c ' to edit code block like in org
(use-package edit-indirect
  :ensure (:host github :repo "Fanael/edit-indirect"))

;; (use-package markdown-ts-mode
;;   :mode ("\\.md\\'" . markdown-ts-mode))

(use-package md-ts-mode
  :ensure (:host github :repo "eki3z/md")
  :mode ("\\.md\\'" . md-ts-mode)
  :hook ((md-ts-mode . md-toc-mode)))

;;; Python =====================================================================
(with-eval-after-load 'python
  (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i"))

;;; Rust =======================================================================
(defun mk/rust/setup ()
  (setq-local electric-pair-pairs
              (add-to-list 'electric-pair-pairs '(?| . ?|))))

(use-package rust-ts-mode
  :ensure nil
  :hook (rust-ts-mode . mk/rust/setup)
  :config
  (defun mk/treesit/rust/simple-indent-rules/field_expression/offset (node parent bol)
    (save-excursion
      (goto-char bol)
      (beginning-of-line-text 0)
      (while (and (looking-at-p "\\.") (not (bobp)))
        (beginning-of-line-text 0))
      (if (string-match-p "\\`[\]\)\}]+\\'" (buffer-substring (point) (line-end-position)))
          (- (mk/lib/column-at-pos (point))
             (save-excursion
               (goto-char (treesit-node-start parent))
               (beginning-of-line-text)
               (current-column)))
        rust-ts-mode-indent-offset)))

  (setq rust-ts-mode--indent-rules
        (treesit-simple-indent-modify-rules
         'rust
         '((rust ((parent-is "field_expression") parent-bol mk/treesit/rust/simple-indent-rules/field_expression/offset)))
         rust-ts-mode--indent-rules
         :replace)))

;;; Typst ======================================================================
;; (use-package outline-indent-mode
;;   :ensure (:type git :host sourcehut :repo "meow_king/outline-indent-mode")
;;   :hook (typst-ts-mode))

(use-package typst-ts-mode
  :after eglot
  :ensure (:type git :host codeberg :repo "meow_king/typst-ts-mode" :branch "develop")
  :custom
  ;; (typst-ts-markup-header-same-height nil)  ; it will leads to height change
  ;; when error occurs (so that the header faces changes into error faces)
  (typst-ts-watch-options '("--open"))
  (typst-ts-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  (typst-ts-enable-raw-blocks-highlight t)
  (typst-ts-preview-function #'browse-url-xdg-open)
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu))

;;; Dockerfile =================================================================
(use-package dockerfile-mode
  :ensure (:host github :repo "spotify/dockerfile-mode"))

;;; Zig ========================================================================
;; (use-package zig-mode
;;   :ensure (:type git :host github :repo "ziglang/zig-mode")
;;   :disabled
;;   :config
;;   (setq zig-format-on-save nil)

;;   (keymap-set zig-mode-map "C-c C-c c" #'zig-compile)
;;   (keymap-set zig-mode-map "C-c C-c C-b" #'zig-build-exe)
;;   (keymap-set zig-mode-map "C-c C-c C-l" #'zig-build-lib)
;;   (keymap-set zig-mode-map "C-c C-c C-o" #'zig-build-obj)
;;   (keymap-set zig-mode-map "C-c C-c t" #'zig-test-buffer)
;;   (keymap-set zig-mode-map "C-c C-c r" #'zig-run)
;;   (keymap-set zig-mode-map "C-c C-c f" #'zig-format-buffer))

(use-package zig-ts-mode
  :after eglot
  :ensure (:host codeberg :branch "develop" :repo "meow_king/zig-ts-mode")
  ;; `(,(rx "." (or "zig" "zon") eos))
  :mode  ("\\.\\(?:z\\(?:ig\\|on\\)\\)\\'"))

;;; Go =========================================================================
(with-eval-after-load 'go-ts-mode
  (setq-default go-ts-mode-indent-offset 4))

;;; OCaml ======================================================================
(use-package neocaml
  :ensure (:host github :repo "bbatsov/neocaml")
  :custom
  (neocaml-repl-program-name "utop")
  (neocaml-repl-program-args '("-emacs")))


;;; micc =======================================================================
;; (use-package crystal-mode)
;; (use-package lua-mode)
;; (use-package fish-mode)

(use-package just-ts-mode
  :ensure (:type git :host github :repo "leon-barrett/just-ts-mode.el"))

;; (use-package graphql-mode)

(provide 'l-languages)

;;; l-languages.el ends here
