;;; my-spellcheck.el --- spell check  -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Meow King <mr.meowking@anche.no>

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

;;; Code:

;; @ ispell
(setq ispell-program-name "hunspell"
      ispell-dictionary "en_US" ;; M-: (message "%s" (ispell-valid-dictionary-list))
      ispell-alternate-dictionary (expand-file-name  "dicts/en_US-large.dic" user-emacs-directory))

;; @ dictionary
(setq dictionary-server "localhost")
(setq switch-to-buffer-obey-display-actions t)

;; @ jinx
(use-package jinx
  :disabled ; TODO
  :ensure (:host github :repo "minad/jinx" :files ("*.el" "*.h" "*.c"))
  :delight
  :init
  (add-hook 'emacs-startup-hook #'global-jinx-mode)
  (add-hook 'markdown-ts-mode-hook 'jinx-mode)
  :config
  ;; To excludes CJK characters
  ;; See issue https://github.com/minad/jinx/issues/4
  ;; This is the syntax table approach. It changes CJK characters from "w" (
  ;; word constituent) to "_" (symbol constituent). You can use `describe-char'
  ;; to view a characters' specific syntax category (from major mode syntax table).
  ;; Emacs 29 supports Unicode 15, the code charts of which can be found at
  ;; http://www.unicode.org/charts/ (use mouse hover to show the specific range)
  (let ((st jinx--base-syntax-table))  ; this code block actually adds 0.14s to my Emacs startup time...
    (modify-syntax-entry '(#x4E00 . #x9FFF) "_" st)   ; CJK Unified Ideographs
    (modify-syntax-entry '(#x3400 . #x4DBF) "_" st)   ; CJK Unified Ideographs Extension A
    (modify-syntax-entry '(#x20000 . #x2A6DF) "_" st) ; CJK Unified Ideographs Extension B
    (modify-syntax-entry '(#x2A700 . #x2B73F) "_" st) ; CJK Unified Ideographs Extension C
    (modify-syntax-entry '(#x2B740 . #x2B81F) "_" st) ; CJK Unified Ideographs Extension D
    (modify-syntax-entry '(#x2B820 . #x2CEAF) "_" st) ; CJK Unified Ideographs Extension E
    (modify-syntax-entry '(#x2CEB0 . #x2EBEF) "_" st) ; CJK Unified Ideographs Extension F
    (modify-syntax-entry '(#x30000 . #x3134F) "_" st) ; CJK Unified Ideographs Extension G
    (modify-syntax-entry '(#x31350 . #x323AF) "_" st) ; CJK Unified Ideographs Extension H
    (modify-syntax-entry '(#x2EBF0 . #x2EE5F) "_" st) ; CJK Unified Ideographs Extension I
    )
  (add-to-list
   'jinx-exclude-faces
   '(typst-ts-mode
     ;; not included font lock faces
     ;; `font-lock-comment-face', `font-lock-string-face', `font-lock-doc-face'
     ;; `font-lock-doc-markup-face'
     font-lock-warning-face font-lock-function-name-face font-lock-function-call-face
     font-lock-variable-name-face font-lock-variable-use-face font-lock-keyword-face
     font-lock-comment-delimiter-face font-lock-type-face font-lock-constant-face
     font-lock-builtin-face font-lock-preprocessor-face
     font-lock-negation-char-face font-lock-escape-face font-lock-number-face
     font-lock-operator-face font-lock-property-use-face font-lock-punctuation-face
     font-lock-bracket-face font-lock-delimiter-face font-lock-misc-punctuation-face
     ;; typst-ts-mode created faces
     typst-ts-markup-item-indicator-face typst-ts-markup-term-indicator-face
     typst-ts-markup-rawspan-indicator-face typst-ts-markup-rawspan-blob-face
     typst-ts-markup-rawblock-indicator-face typst-ts-markup-rawblock-lang-face
     typst-ts-markup-rawblock-blob-face
     typst-ts-error-face typst-ts-shorthand-face typst-ts-markup-linebreak-face
     typst-ts-markup-quote-face typst-ts-markup-url-face typst-ts-math-indicator-face)))


(provide 'my-spellcheck)

;;; my-spellcheck.el ends here
