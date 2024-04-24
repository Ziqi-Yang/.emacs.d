;;; init-advice.el --- initialize advices  -*- lexical-binding: t; -*-
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

;; This file should be loaded early in init.el since all the advices inside
;; this file will be used in later initialization process.
;; 

;;; Code:

;; modern looking https://emacsconf.org/2023/talks/flat/
(defun flat-style(theme &rest args)
  (custom-set-faces
   '(header-line
     ((t (:inherit mode-line
                   :box (:style flat-button)))) t)
   '(mode-line
     ((t (:inherit mode-line
                   :box (:style flat-button)))) t)
   '(mode-line-inactive
     ((t (:inherit mode-line-inactive
                   :box (:style flat-button)))) t)))
(advice-add 'load-theme :after #'flat-style)

(provide 'init-advice)

;;; init-advice.el ends here
