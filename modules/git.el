;;; git.el --- git -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Magit ===================================================
;; use C-j and C-k to navigate instead
;; when meet key conflicts, please refer to evil-collection
;; https://github.com/emacs-evil/evil-collection
;; FIXME it seems that magit has performance issue
;; (use-package magit
;; 	:config
;; 	(setq magit-status-buffer-switch-function #'switch-to-buffer)
;; 	:custom
;;   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; @ Forge
;; Work with Git forges, such as Github and Gitlab, from the comfort of Magit and the rest of Emacs.
;; (use-package forge 
;;   :disabled)

;; (use-package magit-todos
;;   :defer t)

;;; Diff-hl =================================================
(use-package diff-hl
	:hook ((after-init . global-diff-hl-mode))
  :config
  ;; When Emacs runs in terminal, show the indicators in margin instead.

	;; make sure it works in daemon mode
	(add-hook 'server-after-make-frame-hook
		'(lambda () (unless (display-graphic-p)
									(diff-hl-margin-mode))))
	(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
	(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'git)
