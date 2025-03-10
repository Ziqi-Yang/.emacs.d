;;; init-vars.el --- initialize global variables  -*- lexical-binding: t; -*-
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

;; the file should be imported early at init.el
;; Initialize global variables

;;; Code:

(defvar mk/vars/has-nix (getenv "NIX_PATH"))

(defvar mk/vars/in-nixos
  (and (eq system-type 'gnu/linux)
       (string-prefix-p "/run/current-system" (getenv "SHELL"))))

(setq user-full-name "Meow King"
      user-mail-address "mr.meowking@anche.no"
      default-directory (expand-file-name "/tmp")
      shell-file-name (if mk/vars/in-nixos
                          "/run/current-system/sw/bin/bash"
                        "/bin/bash")
      ;; find-function-C-source-directory "~/proj/probe/emacs/src"
      )

(let ((zvm (file-name-concat (getenv "HOME") ".zvm/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":" zvm))
  (setq exec-path (append (list zvm) exec-path)))


(provide 'init-vars)

;;; init-vars.el ends here
