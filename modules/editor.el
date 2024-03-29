;;; editor.el --- Editor Settings -*- lexical-binding: t -*-
;;; Commentary:
;; Not Incluing KeyBindings
;;; Code:

(setq-default
 tab-width 2
 indent-tabs-mode nil  ; use white spaces instead of tabs
 evil-shift-width tab-width
 ;; scroll-margin 15
 scroll-step 1
 select-enable-clipboard nil)  ; make register indepentent from clipboard

;; (with-eval-after-load 'visual-wrap
;;   (add-hook 'prog-mode-hook #'visual-wrap-prefix-mode))

;; @ remember cursor position
(use-package saveplace
  :ensure nil
  :config
  (save-place-mode))


;; change name case at ease (i.e. camel case, snake case)
(use-package string-inflection
  :ensure (:type git :host github :repo "akicho8/string-inflection"))


;; Surround =========================
(use-package surround
  :ensure (:type git :host github :repo "mkleehammer/surround"))

;;; Indent Bar ==================================================================
;; (use-package indent-bars
;;   :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
;;   :custom
;;   (indent-bars-treesit-support t)
;;   (indent-bars-no-descend-string t)
;;   (indent-bars-treesit-ignore-blank-lines-types '("module"))
;;   (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
;; 				                        list list_comprehension
;; 				                        dictionary dictionary_comprehension
;; 				                        parenthesized_expression subscript)))
;;   :hook ((python-base-mode yaml-mode web-mode) . indent-bars-mode))

;;; Paren =======================================================================
;; @ color for all 
;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; @ according to point position
;; (use-package highlight-parentheses
;;   ;; :hook ((prog-mode . highlight-parentheses-mode))
;;   :config
;;   (global-highlight-parentheses-mode)
;;   (setq highlight-parentheses-colors nil
;; 	  highlight-parentheses-highlight-adjacent t
;; 	  highlight-parentheses-attributes '((:weight ultra-bold :background "lightGray"
;; 						                             :box
;; 						                             ( :line-width (1 . -1)
;; 						                               :color ,(face-attribute 'shadow :foreground))))))

;; `show-parent-mode' when point is near a paren, highlight the matching paren
(use-package paren
  :ensure nil
  :custom
  ;; directly near the inner of paren
  (show-paren-when-point-inside-paren t)
  (show-paren-context-when-offscreen 'child-frame)
  :init
  ;; close this mode (default is open)
  (show-paren-mode t))

;; @ use electronic pair
(use-package elec-pair
  :ensure nil
  :hook ((prog-mode) . electric-pair-mode)
  :config
  (add-hook 'web-mode-hook #'(lambda () (electric-pair-local-mode -1)))
  (setq electric-pair-pairs '( ; make electric-pair-mode work on more brackets.
                              (?( . ?))
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
  :delight
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  ;; (global-aggressive-indent-mode 1)
  ;; (dolist (mode '(html-mode mhtml-mode python-mode python-ts-mode mermaid-mode java-ts-mode java-mode))
  ;;   (add-to-list 'aggressive-indent-excluded-modes mode))
  )

;; note: .editorconfig configuration can lead to delete trailing characters on save
(use-package editorconfig
  :delight
  :config
  (editorconfig-mode 1))

(use-package apheleia)

;;; Focus ===================================================
;; focus mode, dim other text color
;; (use-package focus)

;;; Trivial =================================================
;; @ show frequentcy of command
;; (use-package keyfreq
;; 	:config
;; 	(keyfreq-mode 1)
;; 	(keyfreq-autosave-mode 1))


;;; Replace =================================================
(use-package substitute
  :ensure (:type git :host sourcehut :repo "protesilaos/substitute")
  :config
  (add-hook 'substitute-post-replace-functions #'substitute-report-operation))

(provide 'editor)

;;; editor.el ends here
