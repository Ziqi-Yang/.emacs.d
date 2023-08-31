;;; l-kotlin.el --- Kotlin -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:


(use-package kotlin-ts-mode
  :straight (:host gitlab :repo "bricka/emacs-kotlin-ts-mode")
  :mode "\\.kt\\'")

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
    `(kotlin-ts-mode . ("kotlin-language-server"))))

(provide 'l-kotlin)
