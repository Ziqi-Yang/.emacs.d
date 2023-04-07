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
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . mhtml-mode))
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(yaml\\|yml\\)\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(mermaid\\|mmd\\)\\'" . mermaid-mode))

;; manual(use script) build(recommend, since more language are included, but you need need to manualy hook the extra langauge. For build script, see above information), or use nf/treesit-install-all-languages for those languages defined in treesit-auto
(defun nf/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
	    (treesit-install-language-grammar lang)
	    (message "`%s' parser was installed." lang)
	    (sit-for 0.75))))

(setq treesit-font-lock-level 4)

;; @ Automatically install and use tree-sitter major modes in Emacs 29+.
(use-package treesit-auto
	:hook (after-init . global-treesit-auto-mode)
  :config
  (setq treesit-auto-install nil))

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
  ;; in l-java, I use cape to provide very basic completion abilities
  ;; (mk/add-eglot-ensure '(java-mode-hook java-ts-mode-hook)) ;; java (terrible)
  (mk/add-eglot-ensure '(zig-mode-hook)) ;; zig

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

;;; @ lsp-bridge ============================================
;; (use-package yasnippet
;;   :config
;;   (yas-global-mode 1))
;; (use-package posframe)
;; (push (expand-file-name "modules/lsp-bridge" user-emacs-directory) load-path)

;; (require 'lsp-bridge)
;; (require 'lsp-bridge-jdtls) 
;; (dolist (mode-hook '(java-ts-mode-hook rust-ts-mode-hook))
;; 	(add-to-list 'lsp-bridge-default-mode-hooks mode-hook))
;; (global-lsp-bridge-mode)
;; ;; (add-hook 'after-init-hook '(lambda () ((global-lsp-bridge-mode))))
;; (setq acm-candidate-match-function 'orderless-flex
;;   lsp-bridge-enable-auto-import t
;;   lsp-bridge-complete-manually t)
;; (evil-define-key 'normal lsp-bridge-mode-map (kbd "M-k") #'lsp-bridge-popup-documentation-scroll-up)
;; (evil-define-key 'normal lsp-bridge-mode-map (kbd "M-j") #'lsp-bridge-popup-documentation-scroll-down)

;; (setq evil-lookup-func #'lsp-bridge-popup-documentation)
;; (evil-define-key 'insert acm-mode-map (kbd "C-n") #'acm-select-next)
;; (evil-define-key 'insert acm-mode-map (kbd "C-p") #'acm-select-prev)
;; (evil-define-key 'insert acm-mode-map (kbd "C-j") #'acm-select-next)
;; (evil-define-key 'insert acm-mode-map (kbd "C-k") #'acm-select-prev)

;; (evil-define-key 'insert acm-mode-map (kbd "M-n") #'acm-select-next-page)
;; (evil-define-key 'insert acm-mode-map (kbd "M-p") #'acm-select-prev-page)
;; ;; (evil-define-key 'insert acm-mode-map (kbd "M-j") #'acm-select-next-page)
;; ;; (evil-define-key 'insert acm-mode-map (kbd "M-k") #'acm-select-prev-page)
;; (add-hook 'acm-mode-hook #'evil-normalize-keymaps)

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

;;; Spell Checker
;; @ ispell
(setq ispell-program-name "hunspell"
  ispell-dictionary "en_US" ;; M-: (message "%s" (ispell-valid-dictionary-list))
  ispell-alternate-dictionary (expand-file-name  "dicts/en_US-large.dic" user-emacs-directory))

;; @ dictionary
(setq dictionary-server "localhost")
(setq switch-to-buffer-obey-display-actions t)
(add-to-list 'display-buffer-alist
  '("^\\*Dictionary\\*" display-buffer-in-side-window
     (side . left)
     (window-width . 100)))

;; @ Just-in-time spell checking
;; (use-package jit-spell
;;   :straight (:type git :host github :repo "astoff/jit-spell")
;;   :hook (( text-mode org-mode) . jit-spell-mode))


;; @ jinx
(use-package jinx
  :straight (:host github :repo "minad/jinx" :files ("*.el" "*.h" "*.c"))
  :init
  (add-hook 'emacs-startup-hook #'global-jinx-mode)
  :config
  ;; (setq jinx-languages '("en_US.UTF-8" "zh_CN.UTF-8"))
  )

;;; Compile command for each mode ===========================
;; since configuration files for some mode doesn't exist, so I put it all here
(defun mk/set-compile-command ()
  "Define compile command for every mode."
  (setq-local compile-command
    (let* ((base-path
             (if (project-current)
               (project-root (project-current)) ;; have problem with git submodule
               (file-name-directory buffer-file-name)))
            (makefile-exist-p
              (file-exists-p (expand-file-name "Makefile" base-path))))
      (if makefile-exist-p
        "make run"
        (cond
          ;; rust
          ((or (eq major-mode 'rust-mode) (eq major-mode 'rust-ts-mode))
            "cargo run")
          ;; emacs lisp 
          ((eq major-mode 'emacs-lisp-mode)
            (concat "emacs -Q -l " (buffer-file-name) " <open-file>"))
          ;; c
          ((or (eq major-mode 'c-mode) (eq major-mode 'c-ts-mode))
            "make run")
          ;; python
          ((or (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode))
            (concat "python " (buffer-file-name)))
          ;; other
          (t "make run"))))
    ))

(add-hook 'prog-mode-hook #'mk/set-compile-command)

(provide 'l-general)
