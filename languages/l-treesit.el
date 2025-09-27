;;; l-treesit.el --- treesit configuration  -*- lexical-binding: t; -*-
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

;; `nf/treesit-install-all-languages' to install all languages
;; `treesit-install-language-grammar' to install a specific language

;;; Code:

(setq treesit-language-source-alist
      '(  ; use `sort-lines' to sort
        (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.0"))
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil
                       "typescript/src"))
        (mermaid "https://github.com/monaqa/tree-sitter-mermaid")
        ;; remember to update my forked repo:
        ;; https://github.com/Ziqi-Yang/tree-sitter-typst, which is used for
        ;; typst-ts-mode
        (typst "https://github.com/uben0/tree-sitter-typst")
        (llvm "https://github.com/benwilliamgraham/tree-sitter-llvm")
        (vue "https://github.com/ikatyang/tree-sitter-vue")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (toml "https://github.com/ikatyang/tree-sitter-toml")
        (svelte "https://github.com/Himujjal/tree-sitter-svelte.git")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                     nil "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                            nil "tree-sitter-markdown-inline/src"))
        (nix "https://github.com/nix-community/tree-sitter-nix")
        (just "https://github.com/IndianBoy42/tree-sitter-just")
        (zig "https://github.com/maxxnino/tree-sitter-zig")
        (htmldjango . ("https://github.com/interdependence/tree-sitter-htmldjango.git"
                       "v1.0.0"))
        (htmljinja2 . ("https://codeberg.org/meow_king/tree-sitter-htmljinja2"))
        (svelte . ("https://github.com/Himujjal/tree-sitter-svelte"))
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml"
                  nil "grammars/ocaml/src"))
        (ocaml-interface  . ("https://github.com/tree-sitter/tree-sitter-ocaml"
                             nil "grammars/interface/src"))))

(defun nf/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
	    (treesit-install-language-grammar lang)
	    (message "`%s' parser was installed." lang)
	    (sit-for 0.75))))

(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.m?js\\'" . js-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))  ; `rust-ts-mode' has't autoload this line ...
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))   ; so does typescript-ts-mode ...
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'"
;;                                 . cmake-ts-mode))

(provide 'l-treesit)

;;; l-treesit.el ends here
