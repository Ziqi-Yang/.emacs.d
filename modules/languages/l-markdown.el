;;; l-markdown.el --- markdown -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
	;; export function(need to install external program)
	(setq markdown-command "multimarkdown"))

(provide 'l-markdown)
