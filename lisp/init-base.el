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
  (tab-bar-show 1) ;; hide bar if <= 1 tabs open
  (tab-bar-close-button-show nil) ;; hide tab close / X button
  (tab-bar-new-tab-choice "*dashboard*") ;; buffer to show in new tabs
  (tab-bar-tab-hints t) ;; show tab numbers
  (tab-bar-format '(tab-bar-format-menu-bar tab-bar-format-tabs tab-bar-separator))
  ;; don't use :custom-face
  :config
  (tab-bar-mode 1)                           ;; enable tab bar
  (custom-set-faces
    '(tab-bar ((t (:inherit mode-line :box nil))))
    '(tab-bar-tab ((t (:inherit mode-line :foreground "black" :box nil))))
    '(tab-bar-tab-inactive ((t (:inherit mode-line-inactive :foreground "dimGray" :box nil))))))


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
;; TODO try tabspace

;; @ switch workspace
;; (use-package eyebrowse
;;   :config
;; 	(define-key eyebrowse-mode-map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
;; 	(define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
;; 	(define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
;; 	(define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
;; 	(define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
;; 	(define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
;; 	(eyebrowse-mode t)
;; 	(setq eyebrowse-wrap-around t) ;; makes workspaces a loop
;; 	;; (setq eyebrowse-new-workspace "*dashboard*")
;;   ) ;; use *scratch* buffer (use string to provide it with custom buffer name)

;; (use-package tabspaces
;;   ;; use this next line only if you also use straight, otherwise ignore it.
;;   :elpaca (:type git :host github :repo "mclear-tools/tabspaces")
;;   :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
;;   :commands (tabspaces-switch-or-create-workspace
;;               tabspaces-open-or-create-project-and-workspace)
;;   :custom
;;   (tabspaces-use-filtered-buffers-as-default t)
;;   (tabspaces-default-tab "Default")
;;   (tabspaces-remove-to-default t)
;;   (tabspaces-include-buffers '("*scratch*"))
;;   ;; sessions
;;   (tabspaces-session t)
;;   (tabspaces-session-auto-restore t))

;; @ save session(buildin)
;; (use-package desktop
;;   :custom
;;   (desktop-restore-eager 4)
;; 	(desktop-save t)
;; 	:init
;; 	(if (display-graphic-p)
;; 		;; non-daemon emacs
;; 		(progn
;; 			(add-hook 'after-init-hook '(lambda () (desktop-save-mode t)))
;; 			;; Manually read by clicking on dashboard icon instead
;; 			;; (add-hook 'after-init-hook #'desktop-read)
;; 			)
;; 		;; emacs server
;; 		(progn
;; 			(add-hook 'server-after-make-frame-hook '(lambda () (desktop-save-mode t)))
;; 			;; we need the first emacsclient to read the session, the later opened emacsclient(the
;; 			;; first one is still alive) will not read the session since the server arleady owns the
;; 			;; session
;; 			;; Manually read by clicking on dashboard icon instead
;; 			;; (add-hook 'server-after-make-frame-hook #'desktop-read)
;; 			)
;; 		)
;; 	:config
;; 	;; Config Block makes sure this lambda function load later than desktop in kill-emacs-query-functions hook , so this lambda function is executed earlier

;; 	;; remove desktop-kill hook. Leave out the check procedure.
;; 	(remove-hook 'kill-emacs-query-functions #'desktop-kill)

;; 	(let ((save-path (expand-file-name ".local/data/desktop" user-emacs-directory)))
;; 		;; when explictly quit emacs with kill-emacs command
;; 		(add-hook 'kill-emacs-hook
;; 			`(lambda ()
;; 				 (desktop-remove)
;; 				 (desktop-save ,save-path t)))
;; 		;; when implictly quit emacs like close window
;; 		(add-hook 'kill-emacs-query-functions
;; 			`(lambda ()
;; 				 (desktop-remove) ;; make sure there is no desktop file or desktop.el will prompt you Whether override it or not
;; 				 (desktop-save ,save-path t))))) ;; save session without lock

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
  (add-to-list 'project-switch-commands '(project-find-regexp "find regexp" "G"))
  (add-to-list 'project-switch-commands '(consult-ripgrep "Consult rg" "r"))
  (add-to-list 'project-switch-commands '(consult-git-grep "Consult git grep" "g")))

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
  :config
  (global-wakatime-mode))

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
	  '(("DONE" . "#b3b3b3")
	     ("TODO"   . "#2ecc71")
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

;;; Sideline ================================================
;; (use-package sideline
;;   :init
;;   (setq sideline-flymake-display-mode 'point)
;;   (setq sideline-backends-right
;; 	'((sideline-flymake . up)
;;           ;; (mk/sideline-eldoc . down)
;;           ))
;;   :config
;;   (global-sideline-mode))

;; (use-package sideline-flymake)

;; This package does badly (2023.08.30
;; (use-package sideline-eldoc
;;   :elpaca (:type git :host github :repo "ginqi7/sideline-eldoc"))

;; if run the following code on non-emacs-lisp mode using eldoc, then text-read-only error occurs
;; (defvar mk/sideline-eldoc--message "")

;; (defun mk/sideline-eldoc--set-message (str &rest args)
;;   "Extract eldoc message format STR with ARGS."
;;   (when str
;;     (message str)
;;     (setq mk/sideline-eldoc--message (apply #'format str args))))

;; (defun mk/sideline-eldoc (command)
;;   "Eldoc backend for sideline."
;;   (cl-case command
;;     (`candidates
;;       (cons :async
;;         (lambda (callback &rest _)
;;           (progn
;;             (remove-text-properties 0 (length mk/sideline-eldoc--message)
;;               '(read-only t) mk/sideline-eldoc--message)
;;             (funcall callback (split-string mk/sideline-eldoc--message "\n"))))))))

;; (setq eldoc-message-function #'mk/sideline-eldoc--set-message)

;;; Peek ====================================================
;; (use-package peek
;;   :elpaca (:type git :host sourcehut :repo "meow_king/peek")

;;   :custom
;;   ;; only list some mostly-want-changed settings
;;   (peek-overlay-window-size 11) ; lines
;;   ;; one line before the place found by `peek-definition' will also appear
;;   ;; in peek window. Note `peek-definition' is the underlying function of
;;   ;; `peek-xref-definition'
;;   (peek-definition-surrounding-above-lines 1)
;;   (peek-overlay-position 'above) ;; or below

;;   (peek-live-update t) ;; live update peek view of a marked region

;;   (peek-eldoc-message-overlay-position 2) ;; eldoc message overlay at two lines below the point

;;   (peek-enable-eldoc-message-integration nil) ;; disable (defaut) `eldoc-message-function' integration
;;   (peek-enable-eldoc-display-integration nil) ;; enable `eldoc-display-functons'  integration

;;   :config
;;   (global-peek-mode 1)

;;   ;; Keybindings
;;   ;; default keybindings in peek-mode-keymap
;;   (define-key peek-mode-keymap (kbd "M-n") 'peek-next-line)
;;   (define-key peek-mode-keymap (kbd "M-p") 'peek-prev-line)

;;   ;; or you can use `keymap-global-set', which is introduced in emacs 29
;;   ;; (global-set-key (kbd "C-x P p") #'peek-overlay-dwim)
;;   ;; (global-set-key (kbd "C-x P d") #'peek-xref-definition)
;;   ;; (global-set-key (kbd "C-x P m") #'peek-overlay-eldoc-message-toggle-stauts)
;;   ;; (global-set-key (kbd "C-c c d") #'eldoc)

;;   ;; Eldoc display setting
;;   ;; Besides making `peek-enable-eldoc-display-integration' to t, you may want to remove
;;   ;;   other eldoc display functions.
;;   ;; (remove-hook 'eldoc-display-functions 'eldoc-display-in-buffer)

;;   ;; (add-hook 'meow-insert-enter-hook 'peek-overlay-eldoc-message-enable)
;;   ;; (add-hook 'meow-insert-exit-hook 'peek-overlay-eldoc-message-disable)
;;   )

;; (use-package peek-collection
;;   :elpaca (:type git :host sourcehut :repo "meow_king/peek-collection"))


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
(defun mk/push-point-to-xref-marker-stack (&rest r)
  (xref-push-marker-stack (point-marker)))

(defun mk/funcs-go-back-setup()
  (dolist (func '(find-function
                   query-replace-regexp
                   mk/better-query-replace
                   mk/mark-line-smart
                   meow-line
                   meow-beginning-of-thing
                   meow-end-of-thing
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

;;; Bookmark Manage
(use-package ebm
  :elpaca (:type git :host sourcehut :repo "meow_king/ebm"))


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

;;; eldoc headline (my package)
(use-package eldoc-headline
  :elpaca (:type git :host sourcehut :repo "meow_king/eldoc-headline")
  :custom (eldoc-headline-disable-echo-area t)
  :config
  (eldoc-headline-mode 1))


;; expand region ===========
(use-package expreg
  :elpaca (:type git :host github :repo "casouri/expreg"))

(provide 'init-base)

;;; init-base.el ends here
