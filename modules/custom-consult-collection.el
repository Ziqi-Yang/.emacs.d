;;; custom-consult-collection.el --- Custom Functions for Consult -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Meow King <mr.meowking@anche.no>

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


(defun mk/better-consult-ripgrep (arg)
  "Use symbol at point as the default input of `consult-ripgrep'.
ARG: prefix argument.  Use prefix argument when you want no default input."
  (interactive "P")
  (if arg
    (call-interactively #'consult-ripgrep)
    (consult-ripgrep nil (thing-at-point 'symbol))))

(defun mk/better-consult-line (arg)
  "Use symbol at point as the default input of `consult-line'.
ARG: prefix argument.  Use prefix argument when you want no default input."
  (interactive "P")
  (if arg
    (call-interactively #'consult-line)
    (consult-line (thing-at-point 'symbol) nil)))

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
                                :as #'buffer-name))))))
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

(defun mk/consult-buffer-no-hidden()
  "Consult buffer without displaying hidden buffers."
  (interactive)
  (let* ((filters consult-buffer-filter)
          (consult-buffer-filter (push "\\`\\*.*\\*\\'" filters))) ;; local consult-buffer-filter
    (consult-buffer)))

(defun mk/consult-project-buffer-no-hidden()
  "Consult project buffer without displaying hidden buffers."
  (interactive)
  (let* ((filters consult-buffer-filter)
          (consult-buffer-filter (push "\\`\\*.*\\*\\'" filters))) ;; local consult-buffer-filter
    (consult-project-buffer)))

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

(defun mk/completing-rg-types()
  "Completing rg types."
  (let* ((types (mk/get-rg-types))
          (completion-extra-properties
            '(:annotation-function
               (lambda (type)
                 (let ((desc
                         (alist-get
                           type minibuffer-completion-table
                           nil nil #'string=)))
                   (format "\t%s" desc))))))
    (completing-read "Select file type: " types)))

(defun mk/consult-ripgrep-file-type(ftype)
  "Consult-ripgrep with file type(FTYPE) support."
  (interactive "P")
  (let ((consult-ripgrep-args (concat consult-ripgrep-args " -t " (mk/completing-rg-types))))
    (call-interactively #'mk/better-consult-ripgrep)))

(provide 'custom-consult-collection)

;;; custom-consult-collection.el ends here
