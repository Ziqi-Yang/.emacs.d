;;; init-key.el --- Key Mappings Here -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;;; Treesitter ==============================================
;; treesitter lang lib load path: /usr/local/lib and ~/.emacs.d/tree-sitter 
;; use treesit-install-language-grammar to install lang by looking into
;; treesit-language-source-alist variable
;; for manual build: https://github.com/casouri/tree-sitter-module
;; additional resources:
;; starter-guide: https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
;; https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.html

;; currently, emacs lacks buildin rust mode, we directly enable rust-ts-mode
;; but(guess) I think though with ts support, it still lack some feature. For more feature, you
;; should use third-party rust-mode instead.
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(yaml\\|yml\\)\\'" . yaml-ts-mode))

;; manual(use script) build(recommend, since more language are included, but you need need to manualy hook the extra langauge. For build script, see above information), or use nf/treesit-install-all-languages for those languages defined in treesit-auto
(defun nf/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
	    (treesit-install-language-grammar lang)
	    (message "`%s' parser was installed." lang)
	    (sit-for 0.75))))

;; @ Automatically install and use tree-sitter major modes in Emacs 29+.
(use-package treesit-auto
	:hook (after-init . global-treesit-auto-mode)
  :config
  (setq treesit-auto-install t))

;;; Lsp =====================================================
;; check eglot-server-programs to know the language programs that corresponding
;; to a certain language.
;; 
;; Notice that a language server can also execute other *optional package command*
;; like bash-language-server can execute shellcheck, you should check the optional
;; package required by the main pakcage is also installed(pacman -Qi to view)
;;
;; If you still cannot know it since the corresponding function is byte-compiled,
;; go to source code. 
;; to check the value the eglot-server-programs.

(defun mk/add-eglot-ensure (hook-list)
	(dolist (mode hook-list)
		(add-hook mode #'eglot-ensure)))

(progn
	;; (setq-default eglot-events-buffer-size 0)  ;; NOTE disable log, improve performance
	(customize-set-variable 'eglot-autoshutdown t) ;; automatically shutdown
	;; see outer files(like header files) as in project temporarily
	(customize-set-variable 'eglot-extend-to-xref t) 

	(mk/add-eglot-ensure '(c-mode-hook c-ts-mode-hook)) ;; c
	(mk/add-eglot-ensure '(python-mode-hook python-ts-mode-hook)) ;; python
	(mk/add-eglot-ensure '(rust-mode-hook rust-ts-mode-hook)) ;; rust
	(mk/add-eglot-ensure '(go-ts-mode-hook go-mod-ts-mode-hook)) ;; go
	(mk/add-eglot-ensure '(js-mode-hook js-ts-mode-hook tsx-ts-mode-hook typescript-ts-mode-hook typescript-mode-hook)) ;; js/ts
	(mk/add-eglot-ensure '(html-mode-hook mhtml-mode-hook vue-mode-hook css-mode-hook css-ts-mode)) ;; web, vue(defined in l-web.el) and css
  ;; (mk/add-eglot-ensure '(java-mode-hook java-ts-mode-hook)) ;; java (terrible)

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

;; (use-package citre
;;   :defer t
;;   :init
;;   (require 'citre-config)
;;   :config
;;   (setq
;;     citre-default-create-tags-file-location 'global-cache
;;     citre-use-project-root-when-creating-tags t
;;     citre-prompt-language-for-ctags-command t))

;; @ eldoc
(setq eldoc-echo-area-use-multiline-p nil)

;;; Compile command for each mode ===========================
;; since configuration files for some mode doesn't exist, so I put it all here
(add-hook 'prog-mode-hook #'mk/compile-command)

;;; ispell
(setq ispell-program-name "hunspell"
  ispell-dictionary "en_US") ;; M-: (ispell-valid-dictionary-list)

;; @ Just-in-time spell checking
(use-package jit-spell
  :straight (:type git :host github :repo "astoff/jit-spell")
  :hook (( text-mode org-mode) . jit-spell-mode))

(defun mk/compile-command()
  "Define compile command for every mode."
  (if (or (file-exists-p "makefile")
        (file-exists-p "Makefile"))
    (message "hihihi")
    ;; (setq-local compile-command "make run")
    (setq-local compile-command
      (cond
        ((or (eq major-mode 'rust-mode) (eq major-mode 'rust-ts-mode))
          "cargo run")
        ((or (eq major-mode 'c-mode) (eq major-mode 'c-ts-mode))
          "make run")
        (t "make run")))))

(provide 'l-general)
