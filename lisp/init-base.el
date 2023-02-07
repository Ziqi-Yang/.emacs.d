;;; init-key.el --- Basics -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; clean directory ==========================================
(use-package no-littering
	:init
	(setq
		no-littering-etc-directory (expand-file-name ".local/config/" user-emacs-directory)
		no-littering-var-directory (expand-file-name ".local/data/" user-emacs-directory))
	:config
	(require 'recentf)
	(add-to-list 'recentf-exclude no-littering-var-directory)
	(add-to-list 'recentf-exclude no-littering-etc-directory)
	(setq
	auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
	custom-file (no-littering-expand-etc-file-name "custom.el")))

;;; workspace ===============================================
(use-package persp-mode
  :config
  (persp-mode))

;;; text scale change on the fly ============================
(use-package default-text-scale 
	:bind (("C--" . default-text-scale-decrease)
				 ("C-=" . default-text-scale-increase))
  :defer 1
	:hook (after-init . default-text-scale-mode))

;;; Project Utilities =======================================
;; use buildin prokect.el for project ability
;; @ enable consult to find file in project
(use-package consult-project-extra)

;;; Window ==================================================
;; @ jump
(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?c ?n ?j ?k ?l ?i))
  (aw-minibuffer-flag t))

;; @ remember window layout for different scino
(use-package winner
	:hook (after-init . winner-mode))

;;; Recent file =============================================
(use-package recentf
  :defer t
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup 'never) ; "05:00am"
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 50)
  :config
  (add-to-list 'recentf-exclude "/elpa/.*\\'")
  (add-to-list 'recentf-exclude "/tramp.*\\'")
  (add-to-list 'recentf-exclude "/\\.git/.*\\'")
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))


;;; fold ====================================================
;; @ vimmish-fold
;; (use-package vimish-fold
;;   :after evil)

;; (use-package evil-vimish-fold
;;   :after vimish-fold
;;   :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

;; @ tree-sitter powered fold capability
;; TODO this pakcage(folk) is temporary, wait for news
;; (use-package ts-fold
;;   :straight (:type git :host github :repo "AndrewSwerlick/ts-fold" :branch "andrew-sw/treesit-el-support"))

;;; save file utility =======================================
;; when change window, lose focus & idle ...
(use-package super-save
  :defer t
	:hook (after-init . super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t))

;;; Treesitter ==============================================
;; treesitter lang lib load path: /usr/local/lib and ~/.emacs.d/tree-sitter 
;; use treesit-install-language-grammar to install lang by looking into
;; treesit-language-source-alist variable
;; for manual build: https://github.com/casouri/tree-sitter-module
;; additional resources:
;; starter-guide: https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
;; https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.html

;; @ Automatically install and use tree-sitter major modes in Emacs 29+.
(use-package treesit-auto
  :demand t
  :config
  (global-treesit-auto-mode))

;;; Lsp =====================================================
;; check eglot-server-programs to know the language programs that corresponding
;; to a certain language.
;; If you still cannot know it since the corresponding function is byte-compiled,
;; go to https://github.com/emacs-mirror/emacs/blob/emacs-29/lisp/progmodes/eglot.el
;; to check the value the eglot-server-programs.
(progn
	(customize-set-variable 'eglot-autoshutdown t) ;; automatically shutdown
	;; see outer files(like header files) as in project temporarily
	(customize-set-variable 'eglot-extend-to-xref t) 

	(add-hook 'c-mode-hook #'eglot-ensure)
	(add-hook 'c-ts-mode-hook #'eglot-ensure)
	(add-hook 'c++-mode-hook #'eglot-ensure)
	(add-hook 'c++-ts-mode-hook #'eglot-ensure)

	(with-eval-after-load 'eglot
		(add-hook 'eglot-managed-mode-hook
		(lambda () ;; show diagnostics in the echo area
				;; Show flymake diagnostics first.
				(setq eldoc-documentation-functions
						(cons #'flymake-eldoc-function
										(remove #'flymake-eldoc-function eldoc-documentation-functions)))
		;; Show all eldoc feedback.
		(setq eldoc-documentation-strategy #'eldoc-documentation-compose))))

	;; corfu/orderless integration
	(setq completion-category-overrides '((eglot (styles orderless))))

	;; NOTE
	;; install markdown-mode to rich the doc
	)

(provide 'init-base)
