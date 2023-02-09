;;; init-key.el --- Basics -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Trivil ==================================================
;; @ save minibuffer history
(use-package savehist
  :init
	;; Allow commands in minibuffers, will affect `dired-do-dired-do-find-regexp-and-replace' command:
  (setq enable-recursive-minibuffers t)
  (savehist-mode 1))

;;; clean directory =========================================
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
;; @ persp-mode
;; poor document, conflict with vertico-posframe when manually recover, too hard to use
;; (use-package persp-mode
;;   :hook (after-init . persp-mode)
;;   :config
;;   (setq persp-autokill-buffer-on-remove 'kill-weak
;;         persp-auto-resume-time -1 ; Don't auto-load on startup
;;         persp-auto-save-opt 1 ;; save on the emacs shutdown and only if the persp-mode active
;;         persp-reset-windows-on-nil-window-conf nil
;;         persp-nil-hidden t
;;         persp-set-last-persp-for-new-frames t
;;         ;; persp-switch-to-added-buffer nil
;;         persp-kill-foreign-buffer-behaviour 'kill
;;         persp-remove-buffers-from-nil-persp-behaviour nil)) 

(use-package eyebrowse
  :config 
	(define-key eyebrowse-mode-map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
	(define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
	(define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
	(define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
	(define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
	(define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
	(eyebrowse-mode t)
	(setq eyebrowse-wrap-around t) ;; makes workspaces a loop
	(setq eyebrowse-new-workspace "*dashboard*")) ;; use *scratch* buffer (use string to provide it with custom buffer name)

;; save session
(use-package desktop
  :custom
  (desktop-restore-eager 4)
	(desktop-save t)
	:init
	(if (display-graphic-p)
		;; non-daemon emacs 
		(progn
			(add-hook 'after-init-hook '(lambda () (desktop-save-mode t)))
			;; Manually read by clicking on dashboard icon instead
			;; (add-hook 'after-init-hook #'desktop-read)
			)
		;; emacs server
		(progn
			(add-hook 'server-after-make-frame-hook '(lambda () (desktop-save-mode t)))
			;; we need the first emacsclient to read the session, the later opened emacsclient(the
			;; first one is still alive) will not read the session since the server arleady owns the
			;; session
			;; Manually read by clicking on dashboard icon instead
			;; (add-hook 'server-after-make-frame-hook #'desktop-read)
			)
		)
	:config
	;; Config Block makes sure this lambda function load later than desktop in kill-emacs-query-functions hook , so this lambda function is executed earlier

	;; remove desktop-kill hook. Leave out the check procedure.
	(remove-hook 'kill-emacs-query-functions #'desktop-kill)

	(let ((save-path (expand-file-name ".local/data/desktop" user-emacs-directory)))
		;; when explictly quit emacs with kill-emacs command
		(add-hook 'kill-emacs-hook
							`(lambda ()
								 (desktop-remove)
								 (desktop-save ,save-path t)))
		;; when implictly quit emacs like close window
		(add-hook 'kill-emacs-query-functions
							`(lambda ()
										 (desktop-remove) ;; make sure there is no desktop file or desktop.el will prompt you Whether override it or not
										 (desktop-save ,save-path t))))) ;; save session without lock


;;; text scale change on the fly ============================
(use-package default-text-scale 
	:bind (("C--" . default-text-scale-decrease)
				 ("C-=" . default-text-scale-increase))
  :defer 1
	:hook (after-init . default-text-scale-mode))

;;; Project Utilities =======================================
;; use buildin prokect.el for project ability
;; @ enable consult to find file in project
;; (use-package consult-project-extra)

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

;;; Enhance Help ============================================
;; Some symbol cannot be found(like eglot-server-programs, also the emacs
;; buildin helper, but the latter in wider cases can find, maybe same buffer
;; can find, different buffer cannot find). In this case, you should first search
;; for eglot symbol, then all the symbols related to eglot can be found at the next
;; time
(use-package helpful
	:bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

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
	;; currently, emacs lacks normal rust mode, we directly enebl rust-ts-mode
	(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

  (global-treesit-auto-mode))

;;; Lsp =====================================================
;; check eglot-server-programs to know the language programs that corresponding
;; to a certain language.
;; 
;; Notice that a language server can also execute other *optional package command*
;; like bash-language-server can execute shellcheck, you should check the optional
;; package required by the main pakcage is also installed(pacman -Qi to view)
;;
;; If you still cannot know it since the corresponding function is byte-compiled,
;; go to https://github.com/emacs-mirror/emacs/blob/emacs-29/lisp/progmodes/eglot.el
;; to check the value the eglot-server-programs.
(progn
	(customize-set-variable 'eglot-autoshutdown t) ;; automatically shutdown
	;; see outer files(like header files) as in project temporarily
	(customize-set-variable 'eglot-extend-to-xref t) 

	(add-hook 'c-mode-hook #'eglot-ensure) ;; c
	(add-hook 'c-ts-mode-hook #'eglot-ensure)
	(add-hook 'python-mode-hook #'eglot-ensure) ;; python
	(add-hook 'python-ts-mode-hook #'eglot-ensure)
	(add-hook 'rust-ts-mode-hook #'eglot-ensure) ;; rust
	(add-hook 'sh-mode-hook #'eglot-ensure)

	(with-eval-after-load 'eglot
		(add-hook 'eglot-managed-mode-hook
		(lambda () ;; show diagnostics in the echo area
				;; Show flymake diagnostics first.
				(setq eldoc-documentation-functions
						(cons #'flymake-eldoc-function
										(remove #'flymake-eldoc-function eldoc-documentation-functions)))
		;; Show all eldoc feedback.
				(setq eldoc-documentation-strategy #'eldoc-documentation-compose)))

		;; to custom language server (like flags), add-to-list 'eglot-server-programs
		)

	;; corfu/orderless integration
	(setq completion-category-overrides '((eglot (styles orderless))))

	;; NOTE
	;; install markdown-mode to rich the doc
	)

;;; Waketime ================================================
(use-package wakatime-mode
	:config
	(global-wakatime-mode))

;;; Zoxide ==================================================
(use-package zoxide)

(provide 'init-base)
