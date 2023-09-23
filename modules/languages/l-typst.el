;;; l-typst.el --- Typst -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;; (use-package typst-mode
;;   :straight (:type git :host github :repo "Ziqi-Yang/typst-mode.el"))

;; use local version 
;; (push (expand-file-name "modules/languages/typst-mode" user-emacs-directory) load-path)
;; (with-temp-message ""
;;   (require 'typst-mode))

;; (use-package typst-mode
;;   :straight (:type git :host github :repo "Ziqi-Yang/typst-mode.el"))

(use-package typst-ts-mode
  :straight (:type git :host sourcehut :repo "meow_king/typst-ts-mode")
  :custom
  (typst-ts-mode-watch-options "--open"))

(provide 'l-typst)

;;; l-typst.el ends here
