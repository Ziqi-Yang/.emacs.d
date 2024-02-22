;;; init-key.el --- Key Mappings Here -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . mhtml-mode))
;; (add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode))

;; @ eldoc
(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-minor-mode-string nil)
  (setq eldoc-echo-area-use-multiline-p nil))


;;; @ lsp-bridge ============================================
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package lsp-bridge
  :ensure '(lsp-bridge
            :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not elpaca--byte-compile))
  :custom
  (lsp-bridge-code-action-enable-popup-menu nil)  ; FIXME quit popup menu will cause weird problem
  (lsp-bridge-enable-hover-diagnostic t)
  (lsp-bridge-complete-manually t)
  (acm-candidate-match-function 'orderless-regexp)
  (lsp-bridge-python-lsp-server "ruff")
  (lsp-bridge-python-multi-lsp-server "pylsp_ruff")
  (lsp-bridge-multi-lang-server-mode-list
   '(((python-mode python-ts-mode) . lsp-bridge-python-multi-lsp-server)
     ((web-mode) . "html_emmet")
     ((vue-mode) . "volar_emmet")
     ((qml-mode qml-ts-mode) . "qmlls_javascript")))
  ;; use my `eldoc-headline' to display signature information
  (lsp-bridge-signature-show-function '(lambda (str) (setq-local eldoc-headline-string str)))
  :config
  ;; lsp-bridge doesn't work well on *scratch* buffer
  (dolist (hook '(lisp-interaction-mode-hook emacs-lisp-mode-hook))
    (setq lsp-bridge-default-mode-hooks (remove hook lsp-bridge-default-mode-hooks)))

  (add-hook 'web-mode-hook (lambda () (setq-local lsp-bridge-enable-completion-in-string t)))
  (add-hook 'vue-mode-hook (lambda () (setq-local lsp-bridge-enable-completion-in-string t)))
  
  (global-lsp-bridge-mode))

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
  :ensure (:host github :repo "minad/jinx" :files ("*.el" "*.h" "*.c"))
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

(defun mk/setup-eglot-eldoc ()
  "Set the eldoc documentation functions to be the following.
1. flymake-eldoc-function (ensure we can see error in echo line when hover)
2. eglot-signature-eldoc-function
3. eglot-hover-eldoc-function"
  (setq eldoc-documentation-functions
    (cons #'eglot-signature-eldoc-function
	    (remove #'eglot-signature-eldoc-function eldoc-documentation-functions))))

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook #'mk/setup-eglot-eldoc))

(add-hook 'flymake-mode-hook #'mk/setup-flymake-eldoc) ;; works in emacs lisp buffers

;;; Debug =======================================================================
(use-package dape
  ;; Currently only on github
  :ensure (dape :type git :host github :repo "svaante/dape"))

;; Completion-Preview-Mode (emacs30 ============================================
;; (use-package completion-preview
;;   :ensure nil
;;   :delight completion-preview-mode
;;   :hook
;;   ;; text-mode comint-mode
;;   ((prog-mode) . completion-preview-mode)
;;   :custom
;;   (completion-preview-minimum-symbol-length 2)
;;   :bind
;;   (:map completion-preview-active-mode-map
;;     ("M-n" . completion-preview-next-candidate)
;;     ("M-p" . completion-preview-prev-candidate)))

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
    (setq-local
     compile-command
     (let* ((base-path ;; project root when in a project; current directory when not
             (if (project-current)
                 (project-root (project-current)) ;; have problem with git submodule
               (file-name-directory buffer-file-name)))
            (file-extension (file-name-extension buffer-file-name))
            (file-name (file-name-nondirectory buffer-file-name))
            (relative-file-name (file-relative-name buffer-file-name base-path))
            (relative-bare-file-name (file-name-sans-extension relative-file-name))
            (makefile-exist (file-exists-p (expand-file-name "Makefile" base-path)))
            (justfile-exist (file-exists-p (expand-file-name "justfile" base-path)))
            (gradlew-project (file-exists-p (expand-file-name "gradlew" base-path))))
       (cond
        ((and (not major-mode-first) makefile-exist)
         "make run")
        ((and (not major-mode-first) justfile-exist)
         "just run")  ; just --list
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
         (concat "g++ -Wall -std=c++17 " relative-file-name " -o " relative-bare-file-name " && ./" relative-bare-file-name))
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
         (concat "zig build run"))
        ;; typescript
        ((derived-mode-p '(typescript-ts-base-mode))
         (concat "bun run " relative-file-name))
        ;; d2
        ((eq major-mode 'd2-mode)
         (concat "d2 -p 8888 -l elk -w " relative-file-name))
        ;; python
        ((or (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode))
         (concat "python " relative-file-name))
        ;; README.typ -> README.md
        ((and (eq major-mode 'typst-ts-mode) (equal file-name "README.typ"))
         (concat "pandoc -o README.md README.typ"))
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
(add-hook 'typst-ts-mode-hook #'mk/set-compile-command)

;;; Extra Project Root Markers ==============================
(setq project-vc-extra-root-markers
      '(".project-root"  ; my custom
        ".jj"))  ; jujutsu

(provide 'l-general)

;;; l-general.el ends here
