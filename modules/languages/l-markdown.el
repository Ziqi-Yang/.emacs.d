;;; l-markdown.el --- markdown -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; C-c ' to edit code block like in org
(use-package edit-indirect)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
	;; export function(need to install external program)
	(setq markdown-command "multimarkdown"))

(provide 'l-markdown)
