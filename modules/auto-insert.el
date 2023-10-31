;;; auto-insert.el --- Editor Settings -*- lexical-binding: t -*-
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
             (concat
               "# " pname "\n\n"
               "![Static Badge](https://img.shields.io/badge/Made_with-Emacs-purple)" "  \n\n"
               
               "[Project](https://sr.ht/~meow_king/" pname "/) FIXME  \n"
               "[Public Inbox](https://lists.sr.ht/~meow_king/public-inbox): General Consults  \n"
               "[Sending a Patch](https://lists.sr.ht/~meow_king/dev)  \n"
               "[Discussion](https://lists.sr.ht/~meow_king/discussion): Questions and Feedback  \n"
               "[Tickets](https://todo.sr.ht/~meow_king/" pname ") FIXME  \n"))
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
;; Copyright (C) 2023 Meow King <mr.meowking@anche.no>

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
" fname fbname fname))))))

(provide 'auto-insert)
