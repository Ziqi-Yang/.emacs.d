;;; my-search-replace.el --- search replace  -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Meow King <mr.meowking@anche.no>

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

;; search replace utilities

;;; Code:

(use-package substitute
  :ensure (:type git :host sourcehut :repo "protesilaos/substitute")
  :config
  (add-hook 'substitute-post-replace-functions #'substitute-report-operation))

(use-package zoxide)

;; notice: in replace: !, y, n is the keybindings to replace all, replace current and not replace current
(use-package color-rg
  :ensure (:host github :repo "manateelazycat/color-rg"))

;; before goto certain position, you can press `?' key to view the action panel
(use-package avy
  :config
  (setf (alist-get ?c avy-dispatch-alist) 'mk/avy-action/copy-word
        (alist-get ?C avy-dispatch-alist) 'mk/avy-action/copy-symbol
        (alist-get ?y avy-dispatch-alist) 'mk/avy-action/yank-word
        (alist-get ?Y avy-dispatch-alist) 'mk/avy-action/yank-symbol))

(use-package consult
  :custom
  (consult-imenu-config
   '((java-ts-mode :toplevel "Method" :types
					         ((?m "Method" font-lock-function-name-face)
					          (?c "Class" font-lock-type-face)
					          (?i "Interface" font-lock-type-face)))
     (emacs-lisp-mode :toplevel "Functions" :types
					            ((?f "Functions" font-lock-function-name-face)
					             (?m "Macros" font-lock-function-name-face)
					             (?p "Packages" font-lock-constant-face)
					             (?t "Types" font-lock-type-face)
					             (?v "Variables" font-lock-variable-name-face)))
     (typst-ts-mode :topLevel "Headings" :types
					          ((?h "Headings" typst-ts-markup-header-face)
					           (?f "Functions" font-lock-function-name-face)))
     (rust-ts-mode :topLevel "Fn" :types
                   ((?f "Fn" font-lock-function-name-face)
                    (?m "Module" font-lock-variable-name-face)
                    (?t "Type" font-lock-type-face)
                    (?i "Impl" font-lock-operator-face)
                    (?e "Enum" font-lock-variable-name-face)
                    (?s "Struct" font-lock-variable-name-face)))))
  :config
  ;; integrated with xref
  (setq xref-show-xrefs-function #'consult-xref
	      xref-show-definitions-function #'consult-xref)
  ;; disable preview for recent file
  (consult-customize 
   consult-info
   consult-recent-file :preview-key nil))

(use-package consult-todo
  :ensure (:type git :host github :repo "liuyinz/consult-todo"))

;; collaborate with `consult' and `embark'
;; command line tool `repgrep' is also awesome, but not as this good
(use-package wgrep
  :defer 2
  :ensure (:host github :repo "mhayashi1120/Emacs-wgrep"))

;;; Functions ==================================================================

(defvar mk/v/consult-line-persistent-prefix "")

(defun mk/better-consult-line (arg)
  "Use symbol at point as the default input of `consult-line'.
ARG: prefix argument.  Use prefix argument when you want no default input."
  (interactive "P")
  (if arg
      (setq mk/v/consult-line-persistent-prefix (read-string "Set prefix: ")))
  (consult-line (concat mk/v/consult-line-persistent-prefix (thing-at-point 'symbol)) nil))


(defun mk/consult-line-other-window-no-jump ()
  (interactive)
  (let ((symbol (thing-at-point 'symbol))
        (original-window (selected-window)))
    (call-interactively #'ace-window)
    (unwind-protect
        (consult-line (concat mk/v/consult-line-persistent-prefix symbol) nil)
      (select-window original-window t))))

(defun mk/better-consult-line-multi (arg)
  "Buffer filter + symbol at point as the default input of `consult-line-multi'.
ARG: prefix argument.  Use prefix argument when you want no default input.
When ARG is non-nil, then search all buffer."
  (interactive "P")
  (let ((query (list :sort 'alpha-current :directory (and (not arg) 'project)
                     :include (completing-read "buffer include str[empty for all]: "
                                               (when-let (root (consult--project-root))
                                                 (consult--buffer-query :sort 'visibility
                                                                        :directory (and (not arg) 'project)
                                                                        :as #'buffer-name)))))
        (this-command #'consult-line-multi))
    (consult-line-multi query (thing-at-point 'symbol))))

(defun mk/better-consult-man (arg)
  (interactive "P")
  (if arg
      (call-interactively #'consult-man)
    (consult-man (concat (thing-at-point 'symbol) "#3")))
  ;; default: library apis
  ;; this ugly trick here is because I have problem with
  ;; configuring man buffer in `display-buffer-alist'
  (other-window 1)
  (delete-other-windows))

(defun mk/get-rg-types ()
  ;; https://github.com/manateelazycat/color-rg/blob/65818c493f100a78bf55f5a0fe83f29521621b15/color-rg.el#L697
  "Invokes rg --type-list and puts the result in an alist."
  (unless (executable-find "rg")
    (error "'rg' is not in path"))
  (let ((type-list (nbutlast (split-string
                              (shell-command-to-string
                               (concat (executable-find "rg") " --type-list"))
                              "\n") 1)))
    (mapcar
     (lambda (type-alias)
       (setq type-alias (split-string type-alias ":" t))
       (cons (string-trim (car type-alias))
             (string-trim
              (mapconcat 'identity
                         (split-string (cadr type-alias) "," t )
                         " "))))
     type-list)))

(defun mk/completing-read-rg-types()
  "Completing rg types."
  (let* ((types (mk/get-rg-types))
         (completion-extra-properties
          '(:annotation-function
            (lambda (type)
              (let ((desc
                     (alist-get
                      type minibuffer-completion-table
                      nil nil #'string=)))
                (format "\t%s" desc)))))
         (file-extension (downcase (file-name-extension buffer-file-name)))
         type)
    (setq
     type
     (cond
      ((and (derived-mode-p '(html-ts-mode))
            (equal file-extension "vue"))
       "vue")
      ((derived-mode-p '(emacs-lisp-mode)) "elisp")
      (t (car (split-string (symbol-name major-mode) "-")))
      ))
    (completing-read "File type[empty: all]: " types nil nil type)))

(defun mk/consult-ripgrep-file-type (&optional arg)
  "Consult-ripgrep with file type support.
NOTE you can also use prefix argument to specify directory.
ARG: prefix argument.  If ARG is not nil, then prompt for the search directory."
  (interactive "P")
  (let* ((type (mk/completing-read-rg-types))
         (consult-ripgrep-args
          (concat consult-ripgrep-args
                  (when (and type (not (string-empty-p type)))
                    (concat " -t " type))))
         (this-command #'consult-ripgrep))
    (consult-ripgrep
     arg (thing-at-point 'symbol))))

(defun mk/consult-fd-current-directory ()
  "`Consult-fd' at current directory.
Since we already have `project-find-file', we just need `consult-fd' to
search files from current directory."
  (interactive)
  (consult-fd default-directory))


(defun mk/better-consult-imenu (&optional arg)
  (interactive "P")
  (if arg
      (call-interactively #'consult-imenu-multi)
    (call-interactively #'consult-imenu)))

(defun mk/smart-find-file ()
  "Context intelligent Find file."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (let ((default-directory (dired-current-directory)))
        (call-interactively #'find-file))
    (call-interactively #'find-file)))


(defun mk/avy-action/copy-word (pt)
  "Copy sexp starting on PT."
  (save-excursion
    (let (str)
      (goto-char pt)
      (setq str (thing-at-point 'word))
      (kill-new str)
      (message "Copied: %s" str)))
  (select-window
   (cdr
    (ring-ref avy-ring 0))))


(defun mk/avy-action/copy-symbol (pt)
  "Copy sexp starting on PT."
  (save-excursion
    (let (str)
      (goto-char pt)
      (setq str (thing-at-point 'symbol))
      (kill-new str)
      (message "Copied: %s" str)))
  (select-window
   (cdr
    (ring-ref avy-ring 0))))

(defun mk/avy-action/yank-word (pt)
  (mk/avy-action/copy-word pt)
  (yank))

(defun mk/avy-action/yank-symbol (pt)
  (mk/avy-action/copy-symbol pt)
  (yank))

(provide 'my-search-replace)

;;; my-search-replace.el ends here
