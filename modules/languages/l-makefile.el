;;; l-makefile.el --- Makefile  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ziqi Yang <mr.meowking@anche.no>
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

(defun mk/makefile-local-keybinding-setup()
  (keymap-local-set "<tab>" #'(lambda () (insert (kbd "TAB")))))

(add-hook 'make-mode 'mk/makefile-local-keybinding-setup)

(provide 'l-makefile)

;;; l-makefile.el ends here
