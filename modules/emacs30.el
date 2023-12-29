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

;; Flymake ======================================================================
;; (with-eval-after-load 'flymake
;;   (custom-set-variables '(flymake-show-diagnostics-at-end-of-line 'short)))

;; Completion-Preview-Mode ======================================================
;; Enable Completion Preview mode in code buffers
;; (add-hook 'prog-mode-hook #'completion-preview-mode)
;; ;; also in text buffers
;; (add-hook 'text-mode-hook #'completion-preview-mode)
;; ;; and in \\[shell] and friends
;; (with-eval-after-load 'comint
;;   (add-hook 'comint-mode-hook #'completion-preview-mode))
;; (with-eval-after-load 'completion-preview
;;   (setq completion-preview-minimum-symbol-length 2)

;;   ;; Org mode has a custom `self-insert-command'
;;   (push 'org-self-insert-command completion-preview-commands)

;;   ;; Cycle the completion candidate that the preview shows
;;   (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
;;   (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
;;   ;; Convenient alternative to C-S-<return> after typing one of the above (TAB does the same thing)
;;   (keymap-set completion-preview-active-mode-map "C-S-<return>" #'completion-preview-insert))


(provide 'emacs30)

;;; emacs30.el ends here
