;;; init-base.el --- Basics -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Trivil ==================================================
;; @ delete file by moving to trash
;; change the behavior of delete-file and delete-directory function
(setq delete-by-moving-to-trash t)
(setq make-backup-files nil) ;; dont' automatically backup files in <fileName>~ format

;; @ save minibuffer history
;;; save minibuffer history
;; Persist history over Emacs restarts.
(use-package savehist
  :elpaca nil
  :init
  ;; Allow commands in minibuffers, will affect `dired-do-dired-do-find-regexp-and-replace' command:
  (setq enable-recursive-minibuffers t)
  (savehist-mode 1))

;;; auto revert buffer ======================================
(global-auto-revert-mode)

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
(use-package tab-bar
  :elpaca nil
  :custom
  (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-show 1)  ; hide bar if <= 1 tabs open
  (tab-bar-close-button-show nil)  ; hide tab close / X button
  (tab-bar-new-tab-choice "*dashboard*")  ; buffer to show in new tabs
  (tab-bar-tab-hints t)  ; show tab numbers
  (tab-bar-format '(tab-bar-format-menu-bar tab-bar-format-tabs tab-bar-separator))
  ;; don't use :custom-face
  :config
  (tab-bar-mode 1)                           ;; enable tab bar
  (custom-set-faces
   '(tab-bar ((t (:inherit mode-line :box nil))))
   '(tab-bar-tab ((t (:inherit mode-line :foreground "black" :box nil))))
   '(tab-bar-tab-inactive ((t (:inherit mode-line-inactive :foreground "dimGray" :box nil))))))


;;; text scale change on the fly ============================
(use-package default-text-scale
  :bind (("C--" . default-text-scale-decrease)
	       ("C-=" . default-text-scale-increase))
  :config
  (default-text-scale-mode))

;;; Project Utilities =======================================
;; use buildin prokect.el for project ability
;; @ enable consult to find file in project
;; (use-package consult-project-extra)

(use-package project
  :elpaca nil
  :config
  (setq project-switch-commands
    (remove (assoc 'project-find-regexp project-switch-commands) project-switch-commands))
  (add-to-list 'project-switch-commands '(mk/consult-ripgrep-file-type "Consult rg" "r")))

;; (add-hook 'after-init-hook #'mk/setup-project.el)
;; (elpaca nil (mk/setup-project.el))

;;; Window ==================================================
;; @ jump
(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?c ?n ?j ?k ?l ?i))
  (aw-minibuffer-flag t))

(use-package popper
  :init
  (setq popper-reference-buffers
    '("\\*Messages\\*"
       "note.txt"))
  (popper-mode +1)
  (popper-echo-mode +1))

;; @ remember window layout for different scino
;; (use-package winner
;; 	:hook (after-init . winner-mode))

;;; Recent file =============================================
(use-package recentf
  :elpaca nil
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
(use-package hideshow
  :elpaca nil
  :delight hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :config
  ;; also works for outline (minor) mode
  (set-display-table-slot standard-display-table
		'selective-display
		(string-to-vector " ❡❡❡")))

;;; save file utility =======================================
(defun mk/auto-save-visited-predicate ()
  (and
    (not (eq major-mode 'mu4e-compose-mode))
    (not (and buffer-file-name ;; save-visited-file-mode only 
           (member (file-name-base buffer-file-name)
             '("git-rebase-todo"))))))

(custom-set-variables
  '(auto-save-no-message t)
  '(auto-save-visited-mode t)
  '(auto-save-visited-interval 1)
  '(auto-save-visited-predicate #'mk/auto-save-visited-predicate))

;;; Waketime ================================================
(use-package wakatime-mode
  :delight
  :config
  (global-wakatime-mode t))

;;; Zoxide ==================================================
(use-package zoxide)

;;; Search & Replce =========================================
;; @ make search and replace very easy (even for project)
;; notice: in replace: !, y, n is the keybindings to replace all, replace current and not replace current
(use-package color-rg
  :elpaca (:host github :repo "manateelazycat/color-rg"))

;; @ fuzzy finder ;; use consult-ripgrep instead
;; (use-package affe
;;   :elpaca (:host github :repo "minad/affe" :files ("*.el"))
;;   :config
;;   ;; Manual preview key for `affe-grep'
;;   (consult-customize affe-grep :preview-key '(:debounce 0.5 any))
;; 	;; use orderless
;; 	(defun affe-orderless-regexp-compiler (input _type _ignorecase)
;; 		(setq input (orderless-pattern-compiler input))
;; 		(cons input (lambda (str) (orderless--highlight input str))))
;; 	(setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

;;; Todo highlight ==========================================
(use-package hl-todo
  :init
  (setq hl-todo-keyword-faces
	      '(("TODO"   . "#2ecc71")
          ;; ("DONE" . "#b3b3b3")
	        ("FIXME"  . "#e74c3c")
	        ("DEBUG"  . "#9b59b6")
	        ("NOTE" . "#3498db")
	        ("STUB"   . "#f39c12")))
  :config
  (global-hl-todo-mode))

(use-package consult-todo
  :elpaca (:type git :host github :repo "liuyinz/consult-todo"))

;;; Persistent Scrctch Buffer ===============================
;; (use-package persistent-scratch
;; 	:config
;; 	(persistent-scratch-setup-default))

;;; Show Key ================================================
;; for presentation usage
;; (use-package keycast
;;   :after (doom-modeline dashboard))

;;; Buffer Move (swap window) ===============================
(use-package buffer-move)

;;; outline minor mode ======================================
;; use TAB, ze, zE to toggle outline (evil-collection binding)
;; (defun mk/set-outline-minor-mode ()
;;   "Define compile command for every mode."
;;   (outline-minor-mode)
;;   ;; this line also set hs-hide symbol
;;   (let (comment-symbol)
;;     (cond
;;       ((or (eq major-mode 'rust-mode) (eq major-mode 'rust-ts-mode))
;;         (setq comment-symbol "//"))
;;       (t
;;         (setq comment-symbol '(syntax comment-start))))
;;     (set (make-local-variable 'outline-regexp)
;;       (eval `(rx bol ,comment-symbol (*? not-newline) (>= 10 "=") (* blank) eol)))))

;; (add-hook 'prog-mode-hook #'mk/set-outline-minor-mode)

;;; Environment Variables ===================================
(defun mk/set-env()
  "Set environment variables for Emacs"
  (interactive)
  (setenv "GTAGSOBJDIRPREFIX" "/home/zarkli/.cache/gtags/"))
(add-hook 'emacs-startup-hook #'mk/set-env)

;;; Terminal ====================================================================
;; (use-package vterm)

;;; My custom functions ===================================
(defun mk/base/copy-string-to-clipboard (str)
  ;; note this function only works in GUI version emacs
  (with-temp-buffer
    (insert str)
    (clipboard-kill-region (point-min) (point-max))))

(defun mk/vundo-hook ()
  (meow-mode -1))

;;; undo
(use-package vundo
  :config
  (add-hook 'vundo-mode-hook #'mk/vundo-hook)
  ;; Take less on-screen space.
  (setq vundo-compact-display t)

  ;; Better contrasting highlight.
  (custom-set-faces
    '(vundo-node ((t (:foreground "#808080"))))
    '(vundo-stem ((t (:foreground "#808080"))))
    '(vundo-highlight ((t (:foreground "#FFFF00")))))

  ;; Use `HJKL` VIM-like motion, also Home/End to jump around.
  (define-key vundo-mode-map (kbd "l") #'vundo-forward)
  (define-key vundo-mode-map (kbd "<right>") #'vundo-forward)
  (define-key vundo-mode-map (kbd "h") #'vundo-backward)
  (define-key vundo-mode-map (kbd "<left>") #'vundo-backward)
  (define-key vundo-mode-map (kbd "j") #'vundo-next)
  (define-key vundo-mode-map (kbd "<down>") #'vundo-next)
  (define-key vundo-mode-map (kbd "k") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<up>") #'vundo-previous)
  (define-key vundo-mode-map (kbd "a") #'vundo-stem-root)
  (define-key vundo-mode-map (kbd "e") #'vundo-stem-end)
  (define-key vundo-mode-map (kbd "q") #'vundo-quit)
  (define-key vundo-mode-map (kbd "C-g") #'vundo-quit)
  (define-key vundo-mode-map (kbd "RET") #'vundo-confirm))

;;; Navigation ==================================================================
(with-eval-after-load 'xref
  (customize-set-variable
    'xref-history-storage 'xref-window-local-history))

(defun mk/push-point-to-xref-marker-stack (&rest r)
  (xref-push-marker-stack (point-marker)))

(defun mk/xref-stack-current-position ()
  (interactive)
  (mk/push-point-to-xref-marker-stack))

(defun mk/funcs-go-back-setup ()
  (dolist (func '(find-function
                  query-replace-regexp
                  mk/better-query-replace
                  meow-search
                  meow-line
                  ;; meow-beginning-of-thing
                  ;; meow-end-of-thing
                  lsp-bridge-find-def
                  lsp-bridge-find-references
                  lsp-bridge-find-impl
                  lsp-bridge-find-type-def
                  isearch-forward
                  isearch-backward
                  consult-line
                  consult-imenu
                  consult-ripgrep
                  consult-git-grep))
    (advice-add func :before 'mk/push-point-to-xref-marker-stack)))

(add-hook 'after-init-hook 'mk/funcs-go-back-setup)

;;; Ediff =======================================================================
(use-package ediff
  :elpaca nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

;;; Hack Garbage Collector ======================================================
;; emacs2023 conf discuss the emacs gc, and some user pointed out the way that gcmh
;; done is not effective.
;; https://emacsconf.org/2023/talks/gc/
;;  -> Download --pad.md, which is https://media.emacsconf.org/2023/emacsconf-2023-gc--emacsgcstats-does-garbage-collection-actually-slow-down-emacs--ihor-radchenko--pad.md
;; (use-package gcmh
;;   :config
;;   (gcmh-mode))

;;; License =====================================================================
(use-package lice
  :elpaca (:type git :host github :repo "buzztaiki/lice-el"))

;; collect gc-statistics to help developers improve gc performance
;; https://www.reddit.com/r/emacs/comments/14dej62/please_help_collecting_statistics_to_optimize/
;; (use-package emacs-gc-stats
;;   :config
;;   (setq emacs-gc-stats-remind t) ; can also be a number of days
;;   (setq emacs-gc-stats-gc-defaults 'emacs-defaults) ;; use default gc settings
;;   (emacs-gc-stats-mode +1))


;;; Headline ====================================================================
;;; eldoc headline (my package)
;; regarding the configuration of the header-line, see `init-ui'
(use-package eldoc-headline
  :elpaca (:type git :host sourcehut :repo "meow_king/eldoc-headline")
  :delight eldoc-headline-local-mode
  :custom (eldoc-headline-disable-echo-area t)
  :config
  (eldoc-headline-mode 1))

(use-package breadcrumb
  :elpaca (:host github :repo "joaotavora/breadcrumb"))

;;; Misc ==================================================================
(use-package so-long
  :elpaca nil
  :config
  (global-so-long-mode 1))

(use-package pixel-scroll
  :elpaca nil
  :bind
  ([remap scroll-up-command]   . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up)
  :custom
  (pixel-scroll-precision-interpolate-page t)
  :init
  ;; scrolling with an ordinary mouse to be almost as smooth as scrolling with a touchpad, on systems other than X:
  ;; (setq pixel-scroll-precision-large-scroll-height 40.0)
  (pixel-scroll-precision-mode 1))

;; expand region ===========
(use-package expreg
  :elpaca (:type git :host github :repo "casouri/expreg"))

(provide 'init-base)

;;; init-base.el ends here
