;;; emacs-developer.el --- Third Pary Libraries to develop your own emacs libraries -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package compat)
(use-package package-lint)
;; (use-package polymode)
;; (use-package posframe)
;; (use-package quick-peek)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; M-x package-refresh-contents to refresh package-archive-contents

;; https://elpa.gnu.org/packages/persist.html

;;; tree-sitter
(use-package ts-query-highlight
  :elpaca (:type git :host sourcehut :repo "meow_king/ts-query-highlight")
  :config
  (setq ts-query-highlight-dabbrev-expand-function 'cape-dabbrev))

(provide 'emacs-developer)

;;; emacs-developer.el ends here
