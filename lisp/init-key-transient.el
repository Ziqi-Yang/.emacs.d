;;; init-key-transient.el --- Transient Key Maps -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Meow King <mr.meowking@anche.no>

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See theblan
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'transient)

(transient-define-prefix mk/trans-map/rectangle()
  "Rectangle Menu."
  [["Rectangle"
     ("r" "mark mode" rectangle-mark-mode :transient nil)
     ("k" "kill" kill-rectangle :transient nil)
     ("o" "add blank" open-rectangle :transient nil)
     ("y" "yank" yank-rectangle :transient nil)
     ("c" "fill blank" clear-rectangle :transient nil)
     ("d" "delete" delete-rectangle :transient nil)
     ("s" "replace" string-rectangle :transient nil)
     ("K" "copy as kill" copy-rectangle-as-kill :transient nil)
     ("N" "number lines" rectangle-number-lines :transient nil)
     ("C" "copy to register" copy-rectangle-to-register :transient nil)]])

(transient-define-prefix mk/trans-map/consult-info ()
  "Consult Info Menu"
  [["Consult Info"
     ("a" "All" consult-info :transient nil)
     ("e" "Emacs Related" mk/consult-info-emacs :transient nil)]])

(defun mk/consult-info-emacs()
  (interactive)
  (let ((this-command 'consult-info))
    (consult-info "emacs" "efaq" "elisp" "cl" "compat")))

(provide 'init-key-transient)

;;; init-key-transient.el ends here
