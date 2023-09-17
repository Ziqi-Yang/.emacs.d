;;; emacs-developer.el --- Third Pary Libraries to develop your own emacs libraries -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package polymode :defer 1)
(use-package package-lint :defer 1)
(use-package posframe :defer 1)
;; (use-package quick-peek :defer 1)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; M-x package-refresh-contents to refresh package-archive-contents

;; https://elpa.gnu.org/packages/persist.html

;;; tree-sitter
(use-package ts-query-highlight
  :straight (:type git :host sourcehut :repo "meow_king/ts-query-highlight")
  :config
  (setq ts-query-highlight-dabbrev-expand-function 'cape-dabbrev))

(provide 'emacs-developer)
