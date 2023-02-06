;;; init-key.el --- Basics -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; clean directory ==========================================
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
(use-package persp-mode
  :config
  (persp-mode))

;;; text scale change on the fly ============================
(use-package default-text-scale 
	:bind (("C--" . default-text-scale-decrease)
				 ("C-=" . default-text-scale-increase))
  :defer 1
	:hook (after-init . default-text-scale-mode))

;;; Project Utilities =======================================
;; use buildin prokect.el for project ability
;; @ enable consult to find file in project
(use-package consult-project-extra)


;;; Window ==================================================
;; @ jump
(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?c ?n ?j ?k ?l ?i))
  (aw-minibuffer-flag t))

;; @ remember window layout for different scino
(use-package winner
	:hook (after-init . winner-mode))

;;; Recent file =============================================
(use-package recentf
  :defer t
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup 'never) ; "05:00am"
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 50)
  :config
  (add-to-list 'recentf-exclude "/elpa/.*\\'")
  (add-to-list 'recentf-exclude "/tramp.*\\'")
  (add-to-list 'recentf-exclude "/\\.git/.*\\'")
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))


;;; fold ====================================================
(use-package vimish-fold
  :after evil)

(use-package evil-vimish-fold
  :after vimish-fold
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

;;; save file utility =======================================
;; when change window, lose focus & idle ...
(use-package super-save
  :defer t
	:hook (after-init . super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t))

(provide 'init-base)
