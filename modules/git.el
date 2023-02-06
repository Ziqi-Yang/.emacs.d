;;; git.el --- git -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Magit ===================================================
;; when meet key conflicts, please refer to https://github.com/emacs-evil/evil-collection

;; (use-package magit
;; 	:config
;; 	(setq magit-status-buffer-switch-function 'switch-to-buffer)
;; 	:custom
;;   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(provide 'git)
