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
(add-to-list 'auto-mode-alist '("\\.\\(mermaid\\|mmd\\)\\'" . mermaid-ts-mode))
(add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode))

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
;; run command `treesit-auto-install-all' manually to install grammers
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
(use-package eglot-booster
  :elpaca (:host github :repo "jdtsmith/eglot-booster")
	:after eglot
	:config	(eglot-booster-mode))

(defun mk/add-eglot-ensure (hook-list)
	(dolist (mode hook-list)
		(add-hook mode #'eglot-ensure)))

(with-eval-after-load 'eglot
	;; NOTE
	;; install markdown-mode to rich the doc
  ;; performance improvemence: https://www.reddit.com/r/emacs/comments/16vixg6/how_to_make_lsp_and_eglot_way_faster_like_neovim/
  (fset #'jsonrpc--log-event #'ignore) ;; remove laggy typing it probably reduces chatty json from lsp to eglot i guess
	(setq-default eglot-events-buffer-size 0) ;; disable log, improve performance
  ;; list of things that eglot won't change
	(customize-set-variable 'eglot-stay-out-of '(imenu))
  (customize-set-variable 'eglot-extend-to-xref t)
	(customize-set-variable 'eglot-autoshutdown t) ;; automatically shutdown
  (add-hook 'eglot-managed-mode-hook
    (lambda () (eglot-inlay-hints-mode -1)))
  (setq-default eglot-send-changes-idle-time 0.25)
  ;; see outer files(like header files) as in project temporarily

  ;; manually do `eglot' for every workspace is easy, see `eglot-ensure'
  ;; (mk/add-eglot-ensure
  ;;   '(c-mode-hook c-ts-mode-hook ;; c
  ;;      python-mode-hook python-ts-mode-hook ;; python
  ;;      rust-mode-hook rust-ts-mode-hook ;; rust
  ;;      go-ts-mode-hook go-mod-ts-mode-hook ;; go
  ;;      js-mode-hook js-ts-mode-hook tsx-ts-mode-hook typescript-ts-mode-hook typescript-mode-hook ;;  js/ts
  ;;      html-mode-hook mhtml-mode-hook vue-mode-hook css-mode-hook css-ts-mode
  ;;      java-mode-hook java-ts-mode-hook ;; java
  ;;      kotlin-ts-mode-hook ;; kotlin
  ;;      zig-mode-hook ;; zig
  ;;      ))

  ;; how to configure eglot-workspace-configuration:
  ;; https://paste.sr.ht/~meow_king/df83c4dd8541e54befe511ddaf0eeee7cb59eaba
  (setq-default eglot-workspace-configuration
    ;; install python-lsp-server and python-lsp-ruff
    ;; see https://github.com/python-lsp/python-lsp-server
    ;; and https://github.com/charliermarsh/ruff
    '((:pylsp . (:plugins (:ruff (:enabled t)
                            ;; :rope_autoimport doens't work ...
                            ))))))

(use-package eglot-hierarchy
  :elpaca (:host github :repo "dolmens/eglot-hierarchy"))

;; @ eldoc
(use-package eldoc
  :elpaca nil
  :config
  (setq eldoc-minor-mode-string nil)
  (setq eldoc-echo-area-use-multiline-p nil))

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

;; use ggtags instead? https://github.com/yoshizow/global-pygments-plugin.git
;; hacks for ggtags: https://github.com/lynnux/.emacs.d/blob/a4fb0a6cf6abe9f62f3cbadf4d77a11d9ff09a13/settings/package_extra.el#L5801

(use-package citre
  :init
  (require 'citre-config)
  :config
  ;; use `citre-mode' to manually enable citre capf-integration
  ;; citre can do jump without enabling `citre-mode'
  (remove-hook 'find-file-hook #'citre-auto-enable-citre-mode)
  (setq-default
    citre-enable-capf-integration t  ; completion-at-point integration
    citre-enable-imenu-integration nil
    citre-enable-xref-integration nil)
  (setq
    citre-default-create-tags-file-location 'global-cache
    citre-use-project-root-when-creating-tags t
    citre-prompt-language-for-ctags-command t
    ;; citre-capf-substr-completion t
    ;; for my custom MarkdownTAG
    ;; citre-auto-enable-citre-mode-modes '(prog-mode markdown-mode))
    ;; (setq evil-lookup-func #'citre-peek) ;; mapping key "K"
    ))

;;; dumb-jump ===============================================
;; (use-package dumb-jump
;;   :config
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;   (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
;;   ;; (setq evil-lookup-func #'dumb-jump-quick-look)
;;   )

;;; Breadcrumb ==============================================
;; (use-package breadcrumb
;;   :elpaca (:type git :host github :repo "joaotavora/breadcrumb")
;;   :config
;;   (breadcrumb-mode))

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
  :delight
  :init
  (add-hook 'emacs-startup-hook #'global-jinx-mode)
  :config
  ;; (setq jinx-languages '("en_US.UTF-8" "zh_CN.UTF-8"))
  )


(defun mk/setup-flymake-eldoc ()
  "Better setting for displaying flymake diagnostics in eldoc documentation."
  (setq eldoc-documentation-functions
	  (cons #'flymake-eldoc-function
	    (remove #'flymake-eldoc-function eldoc-documentation-functions)))
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose))

(with-eval-after-load 'eglot
  ;; `eglot-hover-eldoc-function' doesn't always display flymake errors
  (add-hook 'eglot-managed-mode-hook #'mk/setup-flymake-eldoc))

(add-hook 'flymake-mode-hook #'mk/setup-flymake-eldoc) ;; works in emacs lisp buffers

;;; Debug =======================================================================
(use-package dape
  ;; Currently only on github
  :elpaca (dape :type git :host github :repo "svaante/dape"))

;; Completion-Preview-Mode (emacs30 ============================================
(use-package completion-preview
  :elpaca nil
  :delight completion-preview-mode
  :hook
  ;; text-mode comint-mode
  ((prog-mode) . completion-preview-mode)
  :custom
  (completion-preview-minimum-symbol-length 2)
  :bind
  (:map completion-preview-active-mode-map
    ("M-n" . completion-preview-next-candidate)
    ("M-p" . completion-preview-prev-candidate)))

;;; Compile command for each mode ===========================
;; since configuration files for some mode doesn't exist, so I put it all here
(defun mk/set-compile-command (&optional major-mode-first)
  "Define compile command for every mode.
MAJOR-MODE-FIRST: respect more about major mode configuration than project
configuration (like Makefile)."
  (interactive (list t))
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
              (makefile-exist (file-exists-p (expand-file-name "Makefile" base-path)))
              (justfile-exist (file-exists-p (expand-file-name "justfile" base-path)))
              (gradlew-project (file-exists-p (expand-file-name "gradlew" base-path))))
        (cond
          ((and (not major-mode-first) makefile-exist)
            "make run")
          ((and (not major-mode-first) justfile-exist)
            "just --list")
          (gradlew-project
            "./gradlew run")
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
          ((derived-mode-p '(zig-mode))
            (concat "zig run " relative-file-name))
          ;; js
          ((derived-mode-p '(js-base-mode typescript-ts-base-mode))
            (concat "tsc " relative-file-name))
          ;; d2
          ((eq major-mode 'd2-mode)
            (concat "d2 -p 8888 -l elk -w " relative-file-name))
          ;; python
          ((or (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode))
            (concat "python " relative-file-name))
          ;; web
          ((eq major-mode 'web-mode)
            ;; format file
            (pcase file-extension
              ("j2" (concat "djlint " relative-file-name " --extension=html.j2 --reformat"))))
          ;; ((eq major-mode 'mermaid-ts-mode)
          ;;   ;; see https://github.com/mermaid-js/mermaid-cli/issues/112#issuecomment-869401507
          ;;   (concat "mmdc -c ~/.config/mermaid/config.json -i " relative-file-name " -o " relative-bare-file-name ".svg && swayimg " relative-bare-file-name ".svg"))
          ((eq major-mode 'mermaid-ts-mode)
            ;; https://sr.ht/~meow_king/mermaid-open/
            (concat "mermaid-open -v " relative-file-name " --no-open | xargs firefox-developer-edition "))
          ((eq major-mode 'plantuml-mode) ;; it seems like that we need to manually run it
            (concat "env PLANTUML_LIMIT_SIZE=327680 plantuml " relative-file-name " && imv " relative-bare-file-name ".png"))
          ((and major-mode-first makefile-exist)
            "make run")
          ;; other
          (t "make "))))))

(add-hook 'prog-mode-hook #'mk/set-compile-command)

;;; Extra Project Root Markers ==============================
(setq project-vc-extra-root-markers '(".project-root"))

(provide 'l-general)

;;; l-general.el ends here
