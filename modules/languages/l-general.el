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
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . mhtml-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode))
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

;; @ Automatically install and use tree-sitter major modes in Emacs 29+.
(use-package treesit-auto
  :config
  (global-treesit-auto-mode)
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

;; @ elgot ==================================================
(defun mk/add-eglot-ensure (hook-list)
	(dolist (mode hook-list)
		(add-hook mode #'eglot-ensure)))

(progn
  ;; performance improvemence: https://www.reddit.com/r/emacs/comments/16vixg6/how_to_make_lsp_and_eglot_way_faster_like_neovim/
  (fset #'jsonrpc--log-event #'ignore) ;; remove laggy typing it probably reduces chatty json from lsp to eglot i guess
	(setq-default eglot-events-buffer-size 0) ;; disable log, improve performance
  ;; list of things that eglot won't change
	(customize-set-variable 'eglot-stay-out-of '(imenu))
	(customize-set-variable 'eglot-autoshutdown t) ;; automatically shutdown
	;; see outer files(like header files) as in project temporarily
	(customize-set-variable 'eglot-extend-to-xref t) 

	(mk/add-eglot-ensure '(c-mode-hook c-ts-mode-hook)) ;; c
	(mk/add-eglot-ensure '(python-mode-hook python-ts-mode-hook)) ;; python
	(mk/add-eglot-ensure '(rust-mode-hook rust-ts-mode-hook)) ;; rust
	(mk/add-eglot-ensure '(go-ts-mode-hook go-mod-ts-mode-hook)) ;; go
	(mk/add-eglot-ensure '(js-mode-hook js-ts-mode-hook tsx-ts-mode-hook typescript-ts-mode-hook typescript-mode-hook)) ;; js/ts
	(mk/add-eglot-ensure '(html-mode-hook mhtml-mode-hook vue-mode-hook css-mode-hook css-ts-mode)) ;; web, vue(defined in l-web.el) and css
  (mk/add-eglot-ensure '(java-mode-hook java-ts-mode-hook)) ;; java (terrible)
  (mk/add-eglot-ensure '(kotlin-ts-mode-hook))
  ;; (mk/add-eglot-ensure '(typst-ts-mode-hook))
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

(use-package eglot-hierarchy
  :elpaca (:host github :repo "dolmens/eglot-hierarchy"))

;; @ eldoc
(setq eldoc-echo-area-use-multiline-p nil)

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

;;; citre ===================================================
(use-package citre
  :init
  (require 'citre-config)
  :config
  (setq-default citre-enable-imenu-integration nil) ;; disable imenu integration
  (setq
    citre-default-create-tags-file-location 'global-cache
    citre-use-project-root-when-creating-tags t
    citre-prompt-language-for-ctags-command t
    citre-capf-substr-completion t
    citre-auto-enable-citre-mode-modes '(prog-mode))
  (add-to-list 'completion-category-overrides
    '(citre (substring basic))) ;; it seems that citre only support substring
  ;; (setq evil-lookup-func #'citre-peek) ;; mapping key "K"
  )

;;; dumb-jump ===============================================
;; (use-package dumb-jump
;;   :config
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;   (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
;;   ;; (setq evil-lookup-func #'dumb-jump-quick-look)
;;   )

;;; Breadcrumb ==============================================
(use-package breadcrumb
  :elpaca (:type git :host github :repo "joaotavora/breadcrumb")
  :config
  (breadcrumb-mode))

;;; Syntax Checker ==========================================
;; flymake is integrated with eglot, so we only need to enable it for emacs lisp mode
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

;;; Spell Checker ===========================================
;; @ ispell
(setq ispell-program-name "hunspell"
  ispell-dictionary "en_US" ;; M-: (message "%s" (ispell-valid-dictionary-list))
  ispell-alternate-dictionary (expand-file-name  "dicts/en_US-large.dic" user-emacs-directory))

;; @ dictionary
(setq dictionary-server "localhost")
(setq switch-to-buffer-obey-display-actions t)


;; @ Just-in-time spell checking
;; (use-package jit-spell
;;   :straight (:type git :host github :repo "astoff/jit-spell")
;;   :hook (( text-mode org-mode) . jit-spell-mode))


;; @ jinx
(use-package jinx
  :elpaca (:host github :repo "minad/jinx" :files ("*.el" "*.h" "*.c"))
  :init
  (add-hook 'emacs-startup-hook #'global-jinx-mode)
  :config
  ;; (setq jinx-languages '("en_US.UTF-8" "zh_CN.UTF-8"))
  )

;;; Debug =======================================================================
(use-package dape
  ;; Currently only on github
  :elpaca (dape :type git :host github :repo "svaante/dape"))

;;; Compile command for each mode ===========================
;; since configuration files for some mode doesn't exist, so I put it all here
(defun mk/set-compile-command ()
  "Define compile command for every mode."
  ;; to make sure buffer has corresponding file, and prevent
  ;; error when loading lisp-interaction-mode at emacs startup
  (when buffer-file-name
    (setq-local compile-command
      (let* ((base-path ;; project root when in a project; current directory when not
               (if (project-current)
                 (project-root (project-current)) ;; have problem with git submodule
                 (file-name-directory buffer-file-name)))
              (file-extension (file-name-extension buffer-file-name))
              (relative-file-name (file-relative-name buffer-file-name base-path))
              (relative-bare-file-name (file-name-sans-extension relative-file-name))
              (makefile-exist (file-exists-p (expand-file-name "Makefile" base-path))))
        (cond
          (makefile-exist
            "make ")
          ;; rust
          ((or (eq major-mode 'rust-mode) (eq major-mode 'rustic-mode) (eq major-mode 'rust-ts-mode)) 
            "cargo run")
          ;; emacs lisp 
          ((eq major-mode 'emacs-lisp-mode)
            (concat "emacs --debug-init --init-directory=~/.emacs.d_test/ -l "
              (project-root (project-current)) "test/init.el" " test/0.el"))
          ;; cpp
          ((or (eq major-mode 'c++-mode) (eq major-mode 'c++-ts-mode))
            (concat "make " relative-bare-file-name " && ./" relative-bare-file-name))
          ;; c
          ((or (eq major-mode 'c-mode) (eq major-mode 'c-ts-mode))
            (concat "make " relative-bare-file-name " && ./" relative-bare-file-name))
          ;; java
          ((or (eq major-mode 'java-mode) (eq major-mode 'java-ts-mode))
            (concat "./gradlew run"))
          ;; kotlin
          ((eq major-mode 'kotlin-ts-mode)
            (concat "kotlinc " relative-file-name " -include-runtime -d app.jar && kotlin ./app.jar"))
          ;; zig
          ((eq major-mode 'zig-mode)
            "zig build")
          ;; python
          ((or (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode))
            (concat "python " relative-file-name))
          ;; web
          ((eq major-mode 'web-mode)
            ;; format file
            (pcase file-extension
              ("j2" (concat "djlint " relative-file-name " --extension=html.j2 --reformat"))))
          ;; other
          (t "make "))))))

(add-hook 'prog-mode-hook #'mk/set-compile-command)

;;; Extra Project Root Markers ==============================
(setq project-vc-extra-root-markers '("Cargo.toml" ".project-root"))

(provide 'l-general)
