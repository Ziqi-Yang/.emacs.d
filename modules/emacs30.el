;;; emacs30.el --- configurations specific to emasc30  -*- lexical-binding: t; -*-
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

;; Current release version of Emacs is 29
;; This file contains configurations specific to Emacs 30

;;; Code:

(with-eval-after-load 'flymake
  (custom-set-variables '(flymake-show-diagnostics-at-end-of-line 'short)))

(add-hook 'prog-mode-hook #'completion-preview-mode)

(provide 'emacs30)

;;; emacs30.el ends here
