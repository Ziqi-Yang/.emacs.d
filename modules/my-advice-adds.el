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
(require 'my-utils)

;; note that `kill-ring-deindent-mode' is useful in Python mode. So I enabled it.
;; When debug, don't ignore it.
(defun mk/advice/clipboard-yank (&rest _r)
  "Advice (type: after) for command `yank'."
  (indent-region (region-beginning) (region-end)))

;; not a advice add function
;; (defun mk/yank-without-indent()
;;   (interactive)
;;   (funcall (advice--cdr (symbol-function #'yank))))

(defun mk/advice/compile (oldfun command &rest r)
  "Advice (type: around) for command `compile'.
OLDFUN COMMAND R."
  (let* ((command-tidy (string-clean-whitespace (string-trim command))))
    (when-let* ((p (project-current))
                (pt (project-root p)))
      (unless (string-prefix-p "nix develop" command-tidy)
        (let ((python-venv (concat pt ".venv")))
          (cond
           ((and (file-exists-p python-venv) (not (string-prefix-p "source" command-tidy)))
            (setq command (concat "source " python-venv "/bin/activate; " command)))))
        (when (and mk/vars/in-nixos (file-exists-p (concat pt "/flake.nix")))
          (setq command (format "nix develop -c bash -c \"%s\"" (mk/util/quote-string command))))))
    (funcall (apply oldfun command r))))

(defun mk/my-advice-add-initialize()
  "Add all my custom advices.
This function should be called after init, so that other initialization can work properly."
  (advice-add #'clipboard-yank :after #'mk/advice/clipboard-yank)
  (advice-add #'compile :around #'mk/advice/compile))

;; indent region being yanked
(add-hook 'after-init-hook #'mk/my-advice-add-initialize)

(provide 'my-advice-adds)

;;; my_addvice_adds.el ends here
