;;; l-org.el --- org -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Common ==================================================

(use-package org
	:ensure nil
	:config
	(setq
	 org-directory "~/notes/"
	 org-hide-emphasis-markers t
	 org-edit-src-content-indentation 2
	 org-ellipsis " ▾ "
	 org-pretty-entities t
	 org-imenu-depth 4
	 org-fold-catch-invisible-edits 'smart
	 org-yank-adjusted-subtrees t 
	 org-image-actual-width nil ;; don't use actual image size
	 org-log-done 'time
	 ;; org-priority-faces ;; tweak org-modern buildin symbol instead
	 ;;   '((?A :foreground "#ff6c6b" :weight bold)
	 ;;     (?B :foreground "#98be65" :weight bold)
	 ;;     (?C :foreground "#c678dd" :weight bold))
	 org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
	 '(("google" . "http://www.google.com/search?q=")
		 ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
		 ("wiki" . "https://en.wikipedia.org/wiki/"))
	 org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
	 '((sequence
			"TODO(t)"           ; A task that is ready to be tackled
			"PROJ(p)"           ; A project that contains other tasks
			"PROG(g)"           ; programming
			"BLOG(b)"           ; Blog writing assignments
			"WAIT(w)"           ; Something is holding up this task
			"|"                 ; The pipe necessary to separate "active" states and "inactive" states
			"DONE(d)"           ; Task has been completed
			"CANCELLED(c)" ))))
(add-hook 'org-mode-hook #'org-indent-mode)

;;; Export
(add-to-list 'org-latex-packages-alist '("AUTO" "babel" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
(setq org-export-with-toc t
			org-export-with-footnotes t
			org-export-coding-system 'utf-8
			org-export-headline-levels 4
			org-export-with-smart-quotes t
			org-latex-compiler "xelatex"
			)


;;; Fancy face ==============================================
;; @ most of the stuffs
(use-package org-modern
	:config
	(setq
	 org-modern-star ["✿" "❀" "✜" "◉" "○" "✸" "✳" "◈" "◇"]
	 org-modern-priority
	 `((?A . ,(propertize "❗" 'face 'error))
		 (?B . ,(propertize "⚡" 'face 'warning))
		 (?C . ,(propertize "☕" 'face 'sucess)))
	 org-modern-todo-faces
	 '(("TODO" :background "#00b894" ;; green
			:foreground "white")
		 ("PROG" :background "#e17055" ;; orange
			:foreground "white")
		 ("PROJ" :background "#6c5ce7" ;; purple
			:foreground "white")
		 ("BLOG" :background "#fdcb6e" ;; yellow
			:foreground "black")
		 ("WAIT" :background "#ff7675" ;; grey
			:foreground "white")
		 ("DONE" :background "#b2bec3" ;; grey
			:foreground "white")
		 ("CANCELLED"  :foreground "#b2bec3")))
	(global-org-modern-mode))


;;; Facility ===============================================
;; @ tangle
;; #+auto_tangle: t
;; #+PROPERTY: header-args :tangle install.sh ;; apply to every header
(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

;; @ visibility
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t)
  (setq org-appear-inside-latex t))

;; @ presentation
(use-package org-tree-slide
	:config
	(setq org-tree-slide-author "MeowKing"
				org-tree-slide-email "mr.ziqiyang@gmail.com"))

;;; Local KeyBindings =======================================
(mapBegin!
 (mk/local-leader-def
	 :states 'normal
	 :keymaps 'org-mode-map
	 "b" #'(org-babel-tangle :which-key "tangle")
	 "e" #'(org-export-dispatch :which-key "dispatch")
	 ))

(provide 'l-org)
