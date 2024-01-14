;;; my-advice-adds.el -- My advice adds  -*- lexical-binding: t; -*-
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

(defun mk/advice/yank (&rest _r)
  "Indent region after yank."
  (indent-region (region-beginning) (region-end)))

(defun mk/my-advice-add-initialize()
  "Add all my custom advices.
This function should be called after init, so that other initialization can work properly."
  (advice-add #'yank :after #'mk/advice/yank))

(defun mk/yank-without-indent()
  (interactive)
  (funcall (advice--cdr (symbol-function #'yank))))

;; indent region being yanked
(add-hook 'after-init-hook #'mk/my-advice-add-initialize)

(provide 'my-advice-adds)

;;; my_addvice_adds.el ends here
