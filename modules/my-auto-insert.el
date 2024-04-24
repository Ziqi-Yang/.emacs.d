;;; my-auto-insert.el --- Editor Settings -*- lexical-binding: t -*-
;;; Commentary:
;; auto insert content to new files
;;; Code:

(auto-insert-mode t)
;; 3 minutes video: https://www.youtube.com/watch?v=wZYiaIvix34

(with-eval-after-load 'autoinsert
  ;; Markdown
  (define-auto-insert
    'markdown-mode
    '(lambda ()
       (let ((p (project-current)))
         (setq pname (project-name p))
         (if pname
           (insert
             (format "# %s

**This project is still in its awkward teenage phase. Usages and Configuration Specifications may be changed after any random update. Use it with your caution.**

![Static Badge](https://img.shields.io/badge/Made_with-Emacs-purple)

[Discussion on Zulip](https://meow-place.zulipchat.com/)  
[Tickets](https://todo.sr.ht/~meow_king/%s): issues, feature requests, etc.  
[Send a Patch](https://lists.sr.ht/~meow_king/%s)
" pname pname pname))
           (insert
             (format "# %s %s" file-name-base buffer-file-name))))))

  ;; Emacs Lisp Mode
  (define-auto-insert
    'emacs-lisp-mode
    '(lambda ()
       (let ((fname (file-name-nondirectory buffer-file-name))
              (fbname (file-name-base buffer-file-name)))
         (insert
           (format
             ";;; %s --- FIXME description  -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Meow King <mr.meowking@anche.no>

;; Version: 0.1.0
;; Author: Meow King <mr.meowking@anche.no>
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
" fname fbname fname)))))

  ;; Java ts Mode
  (define-auto-insert
    'java-ts-mode
    '(lambda ()
       (when-let*
         ((p (project-current))
           (pr (project-root p))
           (d (file-name-directory buffer-file-name))
           (dr (file-relative-name d pr))
           (dr-package (substring dr (+ 5 (string-search "java" dr))))
           (package-name (string-replace "/" "." (substring dr-package 0 -1)))
           (fn (file-name-nondirectory (file-name-sans-extension buffer-file-name))))
         (insert
           (format "package %s;

public class %s {
  public static void main(String[] args) {
    System.out.println(String.format(\"%%s\", \"El Psy Kongaroo\"));
  }
}" package-name fn)))))

  (define-auto-insert
    'editorconfig-conf-mode
    '(insert
       "# https://EditorConfig.org
# https://github.com/editorconfig/editorconfig/wiki/Projects-Using-EditorConfig

# top-most EditorConfig file
root = true

# All Files
[*]
charset = utf-8
indent_style = space
indent_size = 4
# Unix-style newlines with a newline ending every file
end_of_line = lf
insert_final_newline = true

# Tab indentation (no size specified)
[Makefile]
indent_style = tab

[*.el]
indent_size = 2

[*.java]
indent_size = 2
")))

(provide 'my-auto-insert)

;;; my-auto-insert.el ends here
