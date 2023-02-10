;;; init-key.el --- Key Mappings Here -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;;; html 
;; in insert mode: C-j or C-<return> to expand
(use-package emmet-mode
	:hook ((html-mode . emmet-mode)))

;;; Css =====================================================
(use-package rainbow-mode
	:hook ( ((mhtml-mode html-mode css-mode) . rainbow-mode) )) ;; TODO more specific mode

;;; Vue =====================================================
;; @ Custom vue mode based on web-mode
(use-package web-mode)
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;; eglot for vue-mode
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(vue-mode . ("vls" "--stdio"))))

(provide 'l-web)
