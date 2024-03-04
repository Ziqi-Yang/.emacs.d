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
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (typst "https://github.com/uben0/tree-sitter-typst")
        (vue "https://github.com/ikatyang/tree-sitter-vue")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (toml "https://github.com/ikatyang/tree-sitter-toml")))

(defun nf/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
	    (treesit-install-language-grammar lang)
	    (message "`%s' parser was installed." lang)
	    (sit-for 0.75))))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))  ; `rust-ts-mode' haven't autoload this line ...
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))   ; so do typescript-ts-mode ...
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))

;; (add-to-list 'major-mode-remap-alist '(mhtml-mode . html-ts-mode))

(provide 'l-treesit)

;;; l-treesit.el ends here
