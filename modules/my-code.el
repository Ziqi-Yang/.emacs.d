;;; my-code.el --- eglot configuration -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Meow King <mr.meowking@anche.no>

;;; Commentary:

;; start lsp: mk/code/lsp/start
;; stop lsp: mk/code/lsp/stop
;; stop all lsp: mk/code/lsp/stop-all

;;; @ Lsp Mode =================================================================

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))


(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :custom ((lsp-headerline-breadcrumb-enable nil))
  :config
  ;; Lsp Booster: https://github.com/blahgeek/emacs-lsp-booster
  ;; (advice-add (if (progn (require 'json)
  ;;                        (fboundp 'json-parse-buffer))
  ;;                 'json-parse-buffer
  ;;               'json-read)
  ;;             :around
  ;;             #'lsp-booster--advice-json-parse)
  ;; (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  )

;;; Eglot ======================================================================

;; need https://aur.archlinux.org/packages/emacs-lsp-booster-git

;; (use-package eglot-booster
;;   :ensure (:host github :repo "jdtsmith/eglot-booster")
;; 	:after eglot
;; 	:config	(eglot-booster-mode))


;; https://github.com/jdtsmith/eglot-booster/blob/cab7803c4f0adc7fff9da6680f90110674bb7a22/eglot-booster.el#L57
(defun mk/eglot-plain-command-p (com)
  "Test if command COM is a plain eglot server command."
  (and (consp com)
       (not (integerp (cadr com)))
       (not (keywordp (car com)))
       (not (memq :autoport com))))


;; reference: https://github.com/jdtsmith/eglot-booster/blob/cab7803c4f0adc7fff9da6680f90110674bb7a22/eglot-booster.el#L95
(defun mk/advice/eglot/wrap-contact (args)
  "Wrap eglot server commands with `with-project-env' command.  ARGS."
  (let ((contact (nth 3 args))
        (def-args '("with-project-env")))
    (cond
     ((functionp contact)
      (setf (nth 3 args)
            (lambda (&optional interactive)
	            (let ((res (funcall contact interactive)))
		            (if (mk/eglot-plain-command-p res)
		                (append def-args res)
		              res)))))
     ((mk/eglot-plain-command-p contact)
      (setf (nth 3 args) (append def-args contact))))
    args))


(use-package eglot
  :ensure nil
  :config
  ;; NOTE
	;; install markdown-mode to rich the doc
  ;; performance improvemence:
  ;; https://www.reddit.com/r/emacs/comments/16vixg6/how_to_make_lsp_and_eglot_way_faster_like_neovim/

  
  (fset #'jsonrpc--log-event #'ignore) ;; remove laggy typing it probably reduces chatty json from lsp to eglot i guess
  (setq-default eglot-events-buffer-config '(:size 0 :format full))
  
  ;; list of things that eglot won't change
	(customize-set-variable 'eglot-stay-out-of '(imenu))
  (customize-set-variable 'eglot-extend-to-xref t)
	(customize-set-variable 'eglot-autoshutdown t) ;; automatically shutdown
  (add-hook 'eglot-managed-mode-hook
            (lambda () (eglot-inlay-hints-mode -1)))
  (setq-default eglot-send-changes-idle-time 0.25)


  (advice-add 'eglot--connect :filter-args #'mk/advice/eglot/wrap-contact)
  
  (setq-default eglot-workspace-configuration
                ;; use different target directory for rust-analyzer so it won't block other cargo instance
                '(:rust-analyzer (:cargo (:targetDir t))))

  ;; For Vue, use lsp-bridge is the best choice. Vue language server is shit
  (setq eglot-server-programs
        `(((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio"))
          ((markdown-mode markdown-ts-mode md-ts-mode text-mode) . ("harper-ls" "--stdio"))
          (rust-ts-mode . ("rust-analyzer"))
          ((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode) . ,(eglot-alternatives
                                                                 '("clangd")))
          ((nix-mode nix-ts-mode) . ,(eglot-alternatives '("nil" "rnix-lsp" "nixd")))
          (svelte-ts-mode . ("svelteserver" "--stdio"))))

  (with-eval-after-load 'typst-ts-mode
    (add-to-list 'eglot-server-programs
                 `(typst-ts-mode .
                                 ,(eglot-alternatives `(,typst-ts-lsp-download-path
                                                        "tinymist"
                                                        "typst-lsp"))))))

;;; @ lsp-bridge ============================================
(use-package yasnippet
  :delight yas-minor-mode
  :config
  (yas-global-mode 1))

;; activate: mk/lsp-bridge
;; de-activate: mk/lsp-bridge
;; kill the whole lsp-bridge process: mk/lsp-bridge-shutdown-all
;; single buffer
;; lsp-bridge-mode
(use-package lsp-bridge
  :ensure '(lsp-bridge
            :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not elpaca--byte-compile))
  :custom
  (lsp-bridge-code-action-enable-popup-menu nil)  ; FIXME quit popup menu will cause weird problem
  (lsp-bridge-enable-hover-diagnostic t)
  ;; (lsp-bridge-signature-show-function #'lsp-bridge-signature-show-with-frame)
  (lsp-bridge-complete-manually t)
  (acm-candidate-match-function 'orderless-regexp)
  (lsp-bridge-python-lsp-server "basedpyright")
  ;; pyright is good at handling virtual environment with a configuration file
  ;; see https://microsoft.github.io/pyright/#/configuration
  ;; pyrightconfig.json
  ;; {
  ;; "venvPath": ".",
  ;; "venv": ".venv",
  ;; "typeCheckingMode": "standard",
  ;; }
  (lsp-bridge-python-multi-lsp-server "my_basedpyright_ruff")
  (lsp-bridge-single-lang-server-extension-list
   '((("svelte") . "svelteserver")
     (("vue") . "volar")))
  (lsp-bridge-multi-lang-server-extension-list
   '((("html") . "html_emmet_tailwindcss")))
  (lsp-bridge-multi-lang-server-mode-list
   '(((python-mode python-ts-mode) . lsp-bridge-python-multi-lsp-server)))
  ;; use my `eldoc-headline' to display signature information
  ;; (lsp-bridge-signature-show-function '(lambda (str) (setq-local eldoc-headline-string str)))
  (lsp-bridge-user-langserver-dir (expand-file-name "lsp-bridge-config/langserver" user-emacs-directory))
  (lsp-bridge-user-multiserver-dir (expand-file-name "lsp-bridge-config/multiserver"
                                                     user-emacs-directory))
  
  :config
  ;; lsp-bridge doesn't work well on *scratch* buffer
  ;; typst-ts-mode: editing in docs will leads to error messaging/buffer constantly popping up
  (dolist (hook '(lisp-interaction-mode-hook emacs-lisp-mode-hook typst-ts-mode-hook))
    (setq lsp-bridge-default-mode-hooks (remove hook lsp-bridge-default-mode-hooks)))
  
  (setq lsp-bridge-enable-with-tramp nil)  ; goto local sudo bookmark will cause error
  (add-hook 'html-ts-mode-hook (lambda () (setq-local lsp-bridge-enable-completion-in-string t)))
  
  ;; NOTE Don't use advice-add override here, it doesn't work. Simply re-evaluate it
  (defun lsp-bridge--user-tsdk-path-func ()
    "Get tsserver lib*.d.ts directory path."
    (when-let* (((null lsp-bridge-tsdk-path))
                (bin (executable-find "tsc"))
                ;; NOTE I only use NixOS, so hard-code NixOS path here
                (tsdk (expand-file-name "../../lib/node_modules/typescript/lib/" (file-truename bin)))
                ((file-exists-p tsdk)))
      (setq lsp-bridge-tsdk-path tsdk))
    (or lsp-bridge-tsdk-path "")))

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
   citre-enable-xref-integration t)
  (setq
   citre-default-create-tags-file-location 'global-cache
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ;; citre-capf-substr-completion t
   ;; for my custom MarkdownTAG
   ;; citre-auto-enable-citre-mode-modes '(prog-mode markdown-mode))
   ;; (setq evil-lookup-func #'citre-peek) ;; mapping key "K"
   ))

(defun mk/update-all-tags()
  "Update both ctags and gtags file (for citre)."
  (interactive)
  (citre-update-this-tags-file)
  (citre-global-update-database))

(defun mk/toggle-auto-citre()
  "Toggle citre's find-file-hook"
  (interactive)
  (if (member 'citre-auto-enable-citre-mode (default-value 'find-file-hook))
      (remove-hook 'find-file-hook #'citre-auto-enable-citre-mode nil)
    (add-hook 'find-file-hook #'citre-auto-enable-citre-mode nil)))

;;; Misc ======================================================================


(use-package lspx
  :ensure (:host codeberg :repo "meow_king/lspx")
  :config
  (lspx-setup-lspx))


;; @ eldoc
(use-package eldoc
  :ensure nil
  :hook
  ;; global-eldoc-mode is t by default, which enables eldoc when
  ;; `eldoc-documentation-strategy' is not nil
  ;; The following line makes sure that the content in eldoc buffer won't be erased when
  ;; cursor is in eldoc buffer.
  (special-mode . (lambda () (when (equal (current-buffer) (ignore-errors (eldoc-doc-buffer)))
                               (setq-local eldoc-documentation-strategy nil))))
  :custom
  (eldoc-minor-mode-string nil)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package eldoc-box
  :custom
  (eldoc-box-lighter nil)
  (eldoc-box-only-multi-line t)
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-box-hover-mode)
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-managed-mode-hook #'eldoc-box-hover-mode t)))

(use-package flymake
  :ensure nil
  :hook ((emacs-lisp-mode) . flymake-mode)
  :custom
  (flymake-show-diagnostics-at-end-of-line nil))


(use-package dape
  ;; Currently only on github
  :ensure (:type git :host github :repo "svaante/dape"))

;; format file
(use-package apheleia)

(provide 'my-code)

;;; my-code.el ends here
