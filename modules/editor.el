;;; editor.el --- Editor Settings -*- lexical-binding: t -*-
;;; Commentary:
;; Not Incluing KeyBindings
;;; Code:

(setq-default tab-width 2
  indent-tabs-mode nil  ; use white spaces instead of tabs
	evil-shift-width tab-width
	scroll-margin 15
  scroll-step 1
	select-enable-clipboard nil)  ; make register indepentent from clipboard

;; @ remember cursor position
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;;; Indent Bar ==================================================================
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
				                        list list_comprehension
				                        dictionary dictionary_comprehension
				                        parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode web-mode) . indent-bars-mode))

;;; Paren =======================================================================
;; @ color for all 
;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; @ according to point position
(use-package highlight-parentheses
	;; :hook ((prog-mode . highlight-parentheses-mode))
	:config
	(global-highlight-parentheses-mode)
	(setq highlight-parentheses-colors nil
		highlight-parentheses-highlight-adjacent t
		highlight-parentheses-attributes '((:weight ultra-bold :background "#808080"
																				 :box
																				 ( :line-width (1 . -1)
																					 :color ,(face-attribute 'shadow :foreground))))))

(use-package paren 
	:custom
	(show-paren-when-point-inside-paren t)
	:init
	(setq show-paren-mode nil)) ;; use highlight-parentheses instead

;; @ use electronic pair
(use-package elec-pair
  :hook ((prog-mode) . electric-pair-mode)
  :config
  (setq electric-pair-pairs '( ; make electric-pair-mode work on more brackets.
                               (?\{ . ?\})
                               (?\[ . ?\])
                               ;; (?\< . ?\>)
                               )))

;; (use-package smartparens
;;   :hook ((prog-mode org-mode lisp-interaction-mode-hook) . smartparens-mode)
;; 	:config
;; 	;; (sp-pair "<#" "#>") ;; example, support multiple characters
;; 	)

;;; Indentation & format ====================================
(use-package aggressive-indent 
	;; :hook ((prog-mode . aggressive-indent-mode))
	:config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
	;; (global-aggressive-indent-mode 1)
  ;; (dolist (mode '(html-mode mhtml-mode python-mode python-ts-mode mermaid-mode java-ts-mode java-mode))
  ;;   (add-to-list 'aggressive-indent-excluded-modes mode))
  )

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package apheleia)

;; Other ====================================================

(use-package combobulate
	:straight (:host github :repo "mickeynp/combobulate")
  :hook ((python-ts-mode . combobulate-mode)
          (js-ts-mode . combobulate-mode)
          (css-ts-mode . combobulate-mode)
          (yaml-ts-mode . combobulate-mode)
          (json-ts-mode . combobulate-mode)
          (typescript-ts-mode . combobulate-mode)
          (tsx-ts-mode . combobulate-mode)))

;;; Focus ===================================================
;; focus mode, dim other text color
;; (use-package focus)

;;; 80 column ruler =========================================
;; has some problems with olivetti
;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;; Trivial
;; @ show frequentcy of command
;; (use-package keyfreq
;; 	:config
;; 	(keyfreq-mode 1)
;; 	(keyfreq-autosave-mode 1))

;;; Auto Insertion ============================================================
(with-eval-after-load 'autoinsert
  ;; Markdown
  (define-auto-insert
    'markdown-mode
    '(lambda ()
       (let ((p (project-current)))
         (setq pname (project-name p))
         (if pname
           (insert
             (concat
               "# " pname "\n\n"
               "![Static Badge](https://img.shields.io/badge/Made_with-Emacs-purple)" "  \n\n"
               
               "[Project](https://git.sr.ht/~meow_king/" pname "/) FIXME  \n"
               "[Public Inbox](https://lists.sr.ht/~meow_king/public-inbox): General Consults  \n"
               "[Sending a Patch](https://lists.sr.ht/~meow_king/dev)  \n"
               "[Discussion](https://lists.sr.ht/~meow_king/discussion): Questions and Feedback  \n"
               "[Tickets](https://todo.sr.ht/~meow_king/" pname "/trackers) FIXME  \n"))
           (insert
             "# " (file-name-base buffer-file-name))))))

  ;; Emacs Lisp Mode
  (define-auto-insert
    'emacs-lisp-mode
    '(lambda ()
       (let ((fname (file-name-nondirectory buffer-file-name))
              (fbname (file-name-base buffer-file-name)))
         (insert
           (format
             ";;; %s --- FIXME description  -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Ziqi Yang <mr.meowking@anche.no>

;; Version: 0.1.0
;; Author: Ziqi Yang <mr.meowking@anche.no>
;; Keywords: `finder-by-keywords' FIXME
;; URL: FIXME
;; License: GNU General Public License >= 3
;; Package-Requires: ()  ;FIXME: `package-lint-current-buffer'

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(provide '%s)

;;; %s ends here
" fname fbname fname))))))

(provide 'editor)

;;; editor.el ends here
