;;; editor.el --- Editor Settings -*- lexical-binding: t -*-
;;; Commentary:
;; Not Incluing KeyBindings
;;; Code:

(setq-default tab-width 2
  indent-tabs-mode nil  ; use white spaces instead of tabs
	evil-shift-width tab-width
	scroll-margin 15
  scroll-step 1
	select-enable-clipboard nil)  ; make register indepentent from clipboard

;; @ remember cursor position
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;;; Paren ===================================================
;; @ color for all 
;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; @ according to point position
(use-package highlight-parentheses
	;; :hook ((prog-mode . highlight-parentheses-mode))
	:config
	(global-highlight-parentheses-mode)
	(setq highlight-parentheses-colors nil
		highlight-parentheses-highlight-adjacent t
		highlight-parentheses-attributes '((:weight ultra-bold :background "#808080"
																				 :box
																				 ( :line-width (1 . -1)
																					 :color ,(face-attribute 'shadow :foreground))))))

(use-package paren 
	:custom
	(show-paren-when-point-inside-paren t)
	:init
	(setq show-paren-mode nil)) ;; use highlight-parentheses instead

;; @ use electronic pair
(use-package elec-pair
  :hook ((prog-mode) . electric-pair-mode)
  :config
  (setq electric-pair-pairs '( ; make electric-pair-mode work on more brackets.
                               (?\{ . ?\})
                               (?\[ . ?\])
                               ;; (?\< . ?\>)
                               )))

;; (use-package smartparens
;;   :hook ((prog-mode org-mode lisp-interaction-mode-hook) . smartparens-mode)
;; 	:config
;; 	;; (sp-pair "<#" "#>") ;; example, support multiple characters
;; 	)

;;; Indentation & format ====================================
(use-package aggressive-indent 
	;; :hook ((prog-mode . aggressive-indent-mode))
	:config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
	;; (global-aggressive-indent-mode 1)
  ;; (dolist (mode '(html-mode mhtml-mode python-mode python-ts-mode mermaid-mode java-ts-mode java-mode))
  ;;   (add-to-list 'aggressive-indent-excluded-modes mode))
  )

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package apheleia)

;; Other ====================================================

(use-package combobulate
	:straight (:host github :repo "mickeynp/combobulate")
  :hook ((python-ts-mode . combobulate-mode)
          (js-ts-mode . combobulate-mode)
          (css-ts-mode . combobulate-mode)
          (yaml-ts-mode . combobulate-mode)
          (json-ts-mode . combobulate-mode)
          (typescript-ts-mode . combobulate-mode)
          (tsx-ts-mode . combobulate-mode)))

;;; Focus ===================================================
;; focus mode, dim other text color
;; (use-package focus)

;;; 80 column ruler =========================================
;; has some problems with olivetti
;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;; Trivial
;; @ show frequentcy of command
;; (use-package keyfreq
;; 	:config
;; 	(keyfreq-mode 1)
;; 	(keyfreq-autosave-mode 1))

(provide 'editor)
