;;; my-eglot.el --- eglot configuration -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Meow King <mr.meowking@anche.no>

;;; Commentary:

;; start lsp: mk/code/lsp/start
;; stop lsp: mk/code/lsp/stop
;; stop all lsp: mk/code/lsp/stop-all

;; Eglot  ===================

;; check eglot-server-programs to know the language programs that corresponding
;; to a certain language.
;; 
;; Note that a language server can also execute other *optional package command*
;; like bash-language-server can execute shellcheck, you should check the optional
;; package required by the main pakcage is also installed(pacman -Qi to view)

;; manually do `eglot' for every workspace is easy, so we dont' use `eglot-ensure'

;;; Code:

;; lsp-copilot: need refinement

;; (use-package flycheck)
;; (use-package lsp-copilot
;;   :ensure (:host github :repo "Ziqi-Yang/lsp-copilot"
;;                  :files (:defaults "lsp-copilot")
;;                  :pre-build (("cargo" "build" "--release") ("cp" "./target/release/lsp-copilot" "./"))))

;;; Eglot ======================================================================

;; need https://aur.archlinux.org/packages/emacs-lsp-booster-git
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
	:after eglot
	:config	(eglot-booster-mode))

(defun mk/add-eglot-ensure (hook-list)
	(dolist (mode hook-list)
		(add-hook mode #'eglot-ensure)))

(with-eval-after-load 'eglot
	;; NOTE
	;; install markdown-mode to rich the doc
  ;; performance improvemence:
  ;; https://www.reddit.com/r/emacs/comments/16vixg6/how_to_make_lsp_and_eglot_way_faster_like_neovim/
  
  (add-to-list 'eglot-server-programs '((markdown-mode markdown-ts-mode md-ts-mode) . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs '(text-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs `((python-mode python-ts-mode)
                                        . ,(eglot-alternatives
                                            '(("basedpyright-langserver" "--stdio")
                                              ("pyrefly" "lsp")
                                              ("ruff" "server")))))
  
  (fset #'jsonrpc--log-event #'ignore) ;; remove laggy typing it probably reduces chatty json from lsp to eglot i guess
  (setq-default eglot-events-buffer-config '(:size 0 :format full))
  
  ;; list of things that eglot won't change
	(customize-set-variable 'eglot-stay-out-of '(imenu))
  (customize-set-variable 'eglot-extend-to-xref t)
	(customize-set-variable 'eglot-autoshutdown t) ;; automatically shutdown
  (add-hook 'eglot-managed-mode-hook
            (lambda () (eglot-inlay-hints-mode -1)))
  (setq-default eglot-send-changes-idle-time 0.25)
  ;; see outer files(like header files) as in project temporarily

  ;; NOTE: pyright is better for handling virtual environment with a configuration flie per project
  ;; how to configure eglot-workspace-configuration:
  ;; https://paste.sr.ht/~meow_king/df83c4dd8541e54befe511ddaf0eeee7cb59eaba
  ;; Examples:
  ;; (setq-default eglot-workspace-configuration
  ;;               '(:pylsp (:plugins (:ruff (:enabled t)))  ; pylsp
  ;;                        :exportPdf "onSave"))  ; tinymist
  ;; The whole value of eglot-workspace-configuration is directly passed into
  ;; the langauge server
  ;; pylsp's configuration docs: `pylsp.configurationSources'
  ;; tinymist's configuration dosc: `:exportPdf'

  )

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
   '((("svelte") . "svelteserver")))
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
  (add-hook 'web-mode-hook (lambda () (setq-local lsp-bridge-enable-completion-in-string t)))
  (add-hook 'vue-mode-hook (lambda () (setq-local lsp-bridge-enable-completion-in-string t)))
  
  (mk/global-lsp-bridge))


(defvar mk/lsp-bridge--managed-projects (make-hash-table :test #'equal))

(defun mk/lsp-bridge--in-managed-projects-p ()
  (gethash (project-current) mk/lsp-bridge--managed-projects))

(defun mk/lsp-bridge--may-activate ()
  (when (mk/lsp-bridge--in-managed-projects-p)
    (lsp-bridge-mode 1)))

(defun mk/lsp-bridge ()
  "Enable lsp-bridge per project width.
Needs to run `mk/global-lsp-bridge' first."
  (interactive)
  (let ((p (project-current)))
    (if (mk/lsp-bridge--in-managed-projects-p)
        (progn
          (remhash p mk/lsp-bridge--managed-projects)
          ;; deactivate lsp-bridge modes on all buffers in this project
          (dolist (buf (project-buffers p))
            (with-current-buffer buf
              (when lsp-bridge-mode
                (lsp-bridge-mode -1)))))
      (puthash p t mk/lsp-bridge--managed-projects)
      ;; for current buffer
      (lsp-bridge-mode 1))))

(defun mk/lsp-bridge-shutdown-all()
  (interactive)
  (lsp-bridge-kill-process)
  (clrhash mk/lsp-bridge--managed-projects))

(defun mk/global-lsp-bridge ()
  "Slightly modified from `global-lsp-bridge-mode'."
  (interactive)
  (dolist (hook (append
                 lsp-bridge-default-mode-hooks
                 acm-backend-capf-mode-hooks))
    (add-hook hook (lambda ()
                     (when (cl-every (lambda (pred)
                                       (lsp-bridge-check-predicate pred "global-lsp-bridge-mode"))
                                     lsp-bridge-enable-predicates)
                       (mk/lsp-bridge--may-activate))
                     ))))

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

(defun mk/update-all-tags()
  "Update both ctags and gtags file (for citre)."
  (interactive)
  (citre-update-this-tags-file)
  (citre-global-update-database))

;;; Other ======================================================================

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
    (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)))

(with-eval-after-load 'flymake
  (add-hook 'emacs-lisp-mode-hook #'flymake-mode))

(use-package dape
  ;; Currently only on github
  :ensure (:type git :host github :repo "svaante/dape"))

;; format file
(use-package apheleia)

;;; Custom glue functions ======================================================

(defcustom mk/code/current-lsp-backend 'eglot
  "Current lsp backend.
Though citre(ctag) is not a lsp client implementation XD."
  :type '(choice (const :tag "Eglot" eglot)
                 (const :tag "Lsp Bridge" lsp-bridge)
                 (const :tag "citre" citre)))

(defconst mk/code/error/todo-or-not-implemented
  "Todo or not implemented!")

(defun mk/code/lsp/set-backend ()
  (interactive)
  (setq mk/code/current-lsp-backend
        (intern (completing-read
                 (format "Choose an backend(current: %s): " mk/code/current-lsp-backend)
                 '(eglot lsp-bridge citre)))))

(defun mk/code/lsp/start ()
  (interactive)
  (unless mk/code/current-lsp-backend (call-interactively #'mk/code/set-current-lsp-backend))
  (pcase mk/code/current-lsp-backend
    ('lsp-bridge (call-interactively #'mk/lsp-bridge))
    ('eglot (call-interactively #'eglot))
    (_ (user-error mk/code/error/todo-or-not-implemented))))

(defun mk/code/lsp/stop ()
  "NOTE only stop language servers for a single project (or even some language)."
  (interactive)
  (pcase mk/code/current-lsp-backend
    ('lsp-bridge (call-interactively #'mk/lsp-bridge))
    ('eglot (call-interactively #'eglot-shutdown))
    (_ (user-error mk/code/error/todo-or-not-implemented))))

(defun mk/code/lsp/stop-all ()
  (interactive)
  (pcase mk/code/current-lsp-backend
    ('lsp-bridge (call-interactively #'mk/lsp-bridge-shutdown-all))
    ('eglot (call-interactively #'eglot-shutdown-all))
    (_ (user-error mk/code/error/todo-or-not-implemented))))

(with-eval-after-load 'lsp-bridge
  (add-hook 'lsp-bridge-mode-hook (lambda () (setq mk/code/current-lsp-backend 'lsp-bridge))))

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook (lambda () (setq mk/code/current-lsp-backend 'eglot))))


(defun mk/code//is-lsp-bridge-active ()
  (and (equal mk/code/current-lsp-backend 'lsp-bridge) lsp-bridge-mode))

(defun mk/code//is-eglot-active ()
  (and (equal mk/code/current-lsp-backend 'eglot) eglot--managed-mode))

(defun mk/code/rename ()
  (interactive)
  (cond
   ((mk/code//is-lsp-bridge-active)
    (call-interactively #'lsp-bridge-rename))
   ((mk/code//is-eglot-active)
    (call-interactively #'eglot-rename))
   (t (user-error mk/code/error/todo-or-not-implemented))))

(defun mk/code/find-definition ()
  (interactive)
  (cond
   ((mk/code//is-lsp-bridge-active)
    (call-interactively #'lsp-bridge-find-def))
   ((equal mk/code/current-lsp-backend 'citre)
    (call-interactively #'citre-jump))
   (t (let ((this-command 'xref-find-definitions))
        (call-interactively #'xref-find-definitions)))))

(defun mk/code/find-type-definition ()
  (interactive)
  (cond
   ((mk/code//is-eglot-active)
    (eglot-find-typeDefinition))
   (t (user-error (format "Not implemented for back end %s" mk/code/current-lsp-backend)))))

(defun mk/code/query-find-definition ()
  (interactive)
  (pcase mk/code/current-lsp-backend
    ('citre (call-interactively #'citre-query-jump))
    (_ (user-error (format "Not implemented for back end %s" mk/code/current-lsp-backend)))))

(defun mk/code/find-definition-other-window ()
  (interactive)
  (cond
   ((mk/code//is-lsp-bridge-active)
    (call-interactively #'lsp-bridge-find-def-other-window))
   (t (let ((this-command 'xref-find-definitions-other-window))
        (call-interactively #'xref-find-definitions-other-window)))))



(defun mk/code/find-references ()
  (interactive)
  (cond
   ((mk/code//is-lsp-bridge-active)
    (call-interactively #'lsp-bridge-find-references))
   ((equal mk/code/current-lsp-backend 'citre)
    (call-interactively #'citre-jump-to-reference))
   (t (let ((this-command 'xref-find-references))
        (call-interactively #'xref-find-references)))))

(defun mk/code/query-find-references ()
  (interactive)
  (pcase mk/code/current-lsp-backend
    ('citre (call-interactively #'citre-query-jump-to-reference))
    (_ (user-error mk/code/error/todo-or-not-implemented))))




(defun mk/code/find-implementation ()
  (interactive)
  (cond
   ((mk/code//is-lsp-bridge-active)
    (call-interactively #'lsp-bridge-find-impl))
   ((equal mk/code/current-lsp-backend 'citre)
    (call-interactively #'citre-jump-to-reference))
   ((mk/code//is-eglot-active)
    (call-interactively #'eglot-find-implementation))
   (t
    (user-error mk/code/error/todo-or-not-implemented))))

(defun mk/code/toggle-inlay-hint ()
  (interactive)
  (pcase mk/code/current-lsp-backend
    ('lsp-bridge
     (setq-local lsp-bridge-enable-inlay-hint (not lsp-bridge-enable-inlay-hint))
     (if lsp-bridge-enable-inlay-hint
         (lsp-bridge-inlay-hint)
       (lsp-bridge-inlay-hint-hide-overlays)))
    ('eglot (call-interactively #'eglot-inlay-hints-mode))
    ('citre (user-error "Not implemented for back end 'citre"))
    (_ (user-error mk/code/error/todo-or-not-implemented))))

(defun mk/code/error-list (&optional arg)
  (interactive "P")
  (pcase mk/code/current-lsp-backend
    ('lsp-bridge (lsp-bridge-diagnostic-list))
    (_ (call-interactively #'flymake-show-buffer-diagnostics))))

(defun mk/code/action ()
  (interactive)
  (pcase mk/code/current-lsp-backend
    ('lsp-bridge (call-interactively #'lsp-bridge-code-action))
    ('eglot (call-interactively #'eglot-code-actions))
    ('citre (user-error "Not implemented for back end 'citre"))
    (_ (user-error mk/code/error/todo-or-not-implemented))))

(defun mk/code/documentation()
  (interactive)
  (pcase mk/code/current-lsp-backend
    ('lsp-bridge (call-interactively #'lsp-bridge-popup-documentation))
    (_ (call-interactively #'eldoc))))

(defun mk/code/peek (&optional arg)
  (interactive "P")
  (pcase mk/code/current-lsp-backend
    ('lsp-bridge (call-interactively #'lsp-bridge-peek))
    ('eglot (user-error "Not implemented for back end 'eglot"))
    ('citre (call-interactively #'citre-peek))
    (_ (user-error mk/code/error/todo-or-not-implemented))))

(defun mk/code/query-peek-definition ()
  (interactive)
  (pcase mk/code/current-lsp-backend
    ('citre (call-interactively #'citre-query-peek))
    (_ (user-error (format "Not implemented for back end %s" mk/code/current-lsp-backend)))))

(defun mk/code/peek-reference (&optional arg)
  (interactive "P")
  (pcase mk/code/current-lsp-backend
    ('lsp-bridge (user-error "Not implemented for back end 'lsp-bridge"))
    ('eglot (user-error "Not implemented for back end 'eglot"))
    ('citre (call-interactively #'citre-peek-reference))
    (_ (user-error mk/code/error/todo-or-not-implemented))))

(defun mk/code/query-peek-reference ()
  (interactive)
  (pcase mk/code/current-lsp-backend
    ('citre (call-interactively #'citre-query-peek-reference))
    (_ (user-error (format "Not implemented for back end %s" mk/code/current-lsp-backend)))))

(provide 'my-lsp)

;;; my-lsp.el ends here
