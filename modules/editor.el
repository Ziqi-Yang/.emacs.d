;;; editor.el --- Editor Settings -*- lexical-binding: t -*-
;;; Commentary:
;; Not Incluing KeyBindings
;;; Code:

(setq-default tab-width 2
							evil-shift-width tab-width
							scroll-margin 14)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


(provide 'editor)
