;;; my-minibuffer.el --- Minibuffer Helper Functions  -*- lexical-binding: t; -*-

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
;; TODO entering a temporary state that user can use his/her own model
;; editing keybindings / custom keys in this package keybindings, and then collect
;; return values.

;;; Code:

(defun mk/minibuffer-get-associated-buffer ()
  (with-selected-window (minibuffer-selected-window)
    (window-buffer)))

(defun mk/minibuffer-read-project-file-name ()
  "Read file name version of `project-find-file'."
  (let ((find-file-advice (lambda (file-name) file-name)))
    (advice-add 'find-file :override find-file-advice)
    (unwind-protect
      (project-find-file t)
      (advice-remove 'find-file find-file-advice))))

(defun mk/minibuffer-insert-newline ()
  "Insert a newline character in the minibuffer."
  (interactive)
  (insert "\n"))

(defun mk/minibuffer-read-rx-expresion-to-regexp ()
  "Read rx expression and inert the converted regexp into the current minibuffer."
  (interactive)
  (insert (rx-to-string (read--expression "Enter expression: " "(seq )"))))

(defun mk/minibuffer-find-file-zoxide ()
  "Clear minibuffer text, prompt to choose and insert zoxide location.
Require pakcage `zoxide'."
  (interactive)
  (delete-minibuffer-contents)
  (insert (cl-flet ((complete-file-path-func (apply-partially #'read-file-name "zoxide: ")))
            (let ((zoxide-find-file-function #'complete-file-path-func))
              (zoxide-find-file))))
  (exit-minibuffer))

(defun mk/minibuffer-project-find-file()
  "Clear minibuffer and insert `project-find-file'."
  (interactive)
  (delete-minibuffer-contents)
  (insert (mk/minibuffer-read-project-file-name))
  (exit-minibuffer))


(defun mk/minibuffer-insert-thing-at-point ()
  "Prompt and Insert `thing-at-point' content."
  (interactive)
  (let ((thing (with-current-buffer (mk/minibuffer-get-associated-buffer)
                 (thing-at-point
                   (intern
                     (completing-read
                       "Thing: "
                       '(symbol list sexp defun filename existing-filename url email uuid word
                          sentence whitespace line number face page)))))))
    (insert thing)))

(defun mk/minibuffer-insert-selection ()
  "Insert the active-region text into minibuffer."
  (interactive)
  (let ((active-region-text (with-current-buffer (mk/minibuffer-get-associated-buffer)
                              (when (use-region-p)
                                (buffer-substring (use-region-beginning) (use-region-end))))))
    (insert active-region-text)))


(keymap-set minibuffer-local-map "C-<return>" #'mk/minibuffer-insert-newline)
(keymap-set minibuffer-local-map "C-r" #'mk/minibuffer-read-rx-expresion-to-regexp)
(keymap-set minibuffer-local-map "C-u" #'delete-minibuffer-contents)
(keymap-set minibuffer-local-map "C-z" #'mk/minibuffer-find-file-zoxide)
(keymap-set minibuffer-local-map "C-S-i" #'mk/minibuffer-insert-thing-at-point)
(keymap-set minibuffer-local-map "C-S-r" #'mk/minibuffer-insert-selection)
(keymap-set minibuffer-local-map "C-S-p" #'mk/minibuffer-project-find-file)

(provide 'my-minibuffer)

;;; my-minibuffer.el ends here
