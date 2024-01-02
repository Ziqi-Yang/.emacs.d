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

(defun mk/rectangle()
  (interactive)
  (rectangle-mark-mode)
  (mk/trans-map/rectangle))

(transient-define-prefix mk/trans-map/rectangle()
  "Rectangle Menu."
  [["Rectangle"
     ;; ("r" "mark mode" rectangle-mark-mode :transient nil)
     ("j" "next line" next-line :transient t)
     ("k" "prev line" previous-line :transient t)
     ("h" "backward char" backward-char :transient t)
     ("l" "forward char" forward-char :transient t)
     
     ("o" "add blank" open-rectangle :transient nil)
     ("y" "yank" yank-rectangle :transient nil)
     ("c" "fill blank" clear-rectangle :transient nil)
     ("d" "delete" delete-rectangle :transient nil)
     ("s" "replace" string-rectangle :transient nil)
     ("x" "kill" kill-rectangle :transient nil)
     ("K" "copy as kill" copy-rectangle-as-kill :transient nil)
     ("N" "number lines" rectangle-number-lines :transient nil)
     ("C" "copy to register" copy-rectangle-to-register :transient nil)]])

(transient-define-prefix mk/trans-map/consult-info ()
  "Consult Info Menu"
  [["Consult Info"
     ("a" "All" consult-info :transient nil)
     ("e" "Emacs Related" mk/consult-info-emacs :transient nil)]])

(transient-define-prefix mk/trans-map/cargo ()
  "Cargo command menu.
Require cargo package."
  [["Run"
     ("b" "benchmark" cargo-process-bench)
     ("B" "build" cargo-process-build)
     ("C" "clean" cargo-process-clean)
     ("r" "run" cargo-process-run)
     ("x" "run example" cargo-process-run-example)]
    ["Doc"
      ("d" "doc" cargo-process-doc)
      ("D" "open doc" cargo-process-doc-open)]
    ["Test"
      ("t" "current" cargo-process-current-test)
      ("T" "file" cargo-process-current-file-tests)
      ("C-t" "all" cargo-process-test)]
    ["Package"
      ("a" "add" cargo-process-add)
      ("s" "search" cargo-process-search)
      ("C-r" "remove" cargo-process-rm)
      ("u" "update" cargo-process-update)
      ("U" "upgrade" cargo-process-upgrade)]
    ["Misc"
      ("A" "audit" cargo-process-audit)
      ("C-c" "check" cargo-process-check)
      ("c" "clippy" cargo-process-clippy)
      ("R" "repest" cargo-process-repeat)
      ("i" "init" cargo-process-init)
      ("n" "new" cargo-process-new)
      ("f" "format" cargo-process-fmt)
      ("s" "script" cargo-process-script)
      ("w" "watch" cargo-process-watch)]
    ])

(defun mk/consult-info-emacs()
  (interactive)
  (let ((this-command 'consult-info))
    (consult-info "emacs" "efaq" "elisp" "cl" "compat")))

(provide 'init-key-transient)

;;; init-key-transient.el ends here
