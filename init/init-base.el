;;; init-base.el --- Basics -*- lexical-binding: t -*-
;;; Commentary:

;; change the behavior of built-in & basic stuffs 

;;; Code:

;;; Trivil ==================================================
(setq delete-by-moving-to-trash t
      make-backup-files nil)

(use-package savehist
  :ensure nil
  :init
  ;; Allow commands in minibuffers, will affect `dired-do-dired-do-find-regexp-and-replace' command:
  (setq enable-recursive-minibuffers t)
  (savehist-mode 1))

;;; auto revert buffer ======================================
(with-eval-after-load 'emacs
  (global-auto-revert-mode))

;;; clean directory =========================================
;; Don't use it now since saved transient value (via `C-x C-s') cannot be reloaded
;; (use-package no-littering
;;   :init
;;   (setq
;;    no-littering-etc-directory (expand-file-name ".local/config/" user-emacs-directory)
;;    no-littering-var-directory (expand-file-name ".local/data/" user-emacs-directory))
;;   :config
;;   (require 'recentf)
;;   (add-to-list 'recentf-exclude no-littering-var-directory)
;;   (add-to-list 'recentf-exclude no-littering-etc-directory)
;;   (setq
;;    auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
;;    custom-file (no-littering-expand-etc-file-name "custom.el")))

;;; workspace ===============================================
(use-package tab-bar
  :ensure nil
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
(use-package project
  :ensure nil
  :config
  (setq project-switch-commands
        (remove (assoc 'project-find-regexp project-switch-commands) project-switch-commands))
  (add-to-list 'project-switch-commands '(mk/consult-ripgrep-file-type "Consult rg" "r")))

;;; Window ==================================================
;; (use-package moeti-window
;;   :ensure (:host sourcehut :repo "meow_king/moeti-window"))

;; @ jump
(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?c ?n ?j ?k ?l ?i))
  (aw-minibuffer-flag t))

;; @ remember window layout for different scino
;; (use-package winner
;; 	:hook (after-init . winner-mode))

;;; Recent file =============================================
(use-package recentf
  :ensure nil
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
(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;;; fold ====================================================
(use-package hideshow
  :ensure nil
  :delight hs-minor-mode
  ;; :hook (prog-mode . hs-minor-mode)
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

(with-eval-after-load 'emacs
  (custom-set-variables
   '(auto-save-no-message t)
   '(auto-save-visited-mode t)
   '(auto-save-visited-interval 1)
   '(auto-save-visited-predicate #'mk/auto-save-visited-predicate)))

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

;;; undo
(use-package vundo
  :config
  (add-hook 'vundo-mode-hook #'(lambda () (meow-mode -1)))
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
   'xref-history-storage 'xref-window-local-history)
  (mk/funcs-go-back-setup))

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
                  meow-beginning-of-thing
                  meow-end-of-thing
                  lsp-bridge-find-def
                  lsp-bridge-find-references
                  lsp-bridge-find-impl
                  lsp-bridge-find-type-def
                  isearch-forward isearch-forward-regexp
                  isearch-backward isearch-backward-regexp
                  consult-line
                  consult-goto-line
                  consult-imenu
                  consult-ripgrep
                  consult-git-grep
                  citre-jump))
    (advice-add func :before 'mk/push-point-to-xref-marker-stack)))

;;; Shell ======================================================================
(use-package eat
  :ensure
  (:url "https://codeberg.org/akib/emacs-eat"
        :files ("*.el" ("term" "term/*.el") "*.texi"
                "*.ti" ("terminfo/e" "terminfo/e/*")
                ("terminfo/65" "terminfo/65/*")
                ("integration" "integration/*")
                (:exclude ".dir-locals.el" "*-tests.el")))
  :custom
  (eat-shell
   (if mk/vars/in-nixos
       "/run/current-system/sw/bin/fish"
     "/bin/fish")))

;;; Ediff =======================================================================
(use-package ediff
  :ensure nil
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


;; Performance =================================================================
(use-package so-long
  :ensure nil
  :config
  (global-so-long-mode 1))

;; https://emacs-china.org/t/topic/25811/7
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)



;;; Misc =======================================================================
(with-eval-after-load 'indent-aux
  (kill-ring-deindent-mode t))

(use-package pixel-scroll
  :ensure nil
  :bind
  ([remap scroll-up-command]   . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up)
  :custom
  (pixel-scroll-precision-interpolate-page t)
  :init
  ;; scrolling with an ordinary mouse to be almost as smooth as scrolling with a touchpad, on systems other than X:
  ;; (setq pixel-scroll-precision-large-scroll-height 40.0)
  (pixel-scroll-precision-mode 1))

(use-package eee
  :ensure (:type git :host github :repo "eval-exec/eee.el"
                 :files (:defaults "*.el" "*.sh"))
  :custom
  (ee-terminal-command "footclient -a floating_noanim"))

(provide 'init-base)

;;; init-base.el ends here
