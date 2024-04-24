;;; my-emacs-pkg-dev.el --- Dev Libraries -*- lexical-binding: t -*-
;;; Commentary:

;; other use libraries for use in ur packages
;; (use-package compat)

;;; Code:

(use-package package-lint)

;;; tree-sitter
(use-package ts-query-highlight
  :ensure (:type git :host sourcehut :repo "meow_king/ts-query-highlight")
  :config
  (setq ts-query-highlight-dabbrev-expand-function 'cape-dabbrev))

(provide 'my-emacs-pkg-dev)

;;; my-emacs-pkg-dev.el ends here
