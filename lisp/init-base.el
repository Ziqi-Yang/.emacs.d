;;; init-key.el --- Basics -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; clean directory ======================
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

;;; workspace ===========================
(use-package persp-mode
  :config
  (persp-mode))

;;; fold ================================
(use-package vimish-fold
  :after evil)

(use-package evil-vimish-fold
  :after vimish-fold
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

;;; save file utility ==========
;; when change window, lose focus & idle ...
(use-package super-save
  :defer t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(provide 'init-base)
