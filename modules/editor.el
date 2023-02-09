;;; editor.el --- Editor Settings -*- lexical-binding: t -*-
;;; Commentary:
;; Not Incluing KeyBindings
;;; Code:

(setq-default tab-width 2
							evil-shift-width tab-width
							scroll-margin 14
							select-enable-clipboard nil ;; make register indepentent from clipboard
							)

;; @ remember cursor position
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))



;;; Paren ===================================================

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paren
	:config
	(setq show-paren-when-point-inside-paren t))

;; @ use smartparens instead
;; (use-package elec-pair
;;   :hook ((prog-mode org-mode) . electric-pair-mode)
;;   :config
;;   (setq electric-pair-pairs '( ; make electric-pair-mode work on more brackets.
;;                               (?\{ . ?\})
;;                               (?\[ . ?\])
;;                               (?\< . ?\>)
;;                               )))

(use-package smartparens
  (:hook (prog-mode org-mode) . smartparens-mode)
	:config
	(sp-pair "<" ">")
	(sp-pair "$" "$"))

;;; Focus ===================================================
;; focus mode, dim other text color
;; (use-package focus)

(provide 'editor)
