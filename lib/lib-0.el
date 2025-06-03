;;; lib-0.el --- library -*- lexical-binding: t; -*-
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

;;; Code:

(use-package request)

(defun mk/util/quote-string (s)
  (replace-regexp-in-string
   (rx (group (or "'" "\"")))
   (lambda (match) (concat "\\\\" match))
   (string-replace "\\" "\\\\" s)))

(defun mk/lib/display-buffer-in-other-window ()
  "Display current buffer in the other window."
  (let ((buf (current-buffer))
        (window (selected-window)))
    (with-selected-window
        (display-buffer
         buf
         `((xref--display-buffer-in-other-window)
           (window . ,window)))
      (selected-window))))

(defun mk/lib/copy-string-to-clipboard (str)
  ;; note this function only works in GUI version emacs
  (with-temp-buffer
    (insert str)
    (clipboard-kill-region (point-min) (point-max))))

(defun mk/lib/buffer-remove-left-common-paddings ()
  "Remove common left padding from the current buffer."
  (let ((min-indent nil))
    ;; Calculate minimum indentation across lines
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (unless (looking-at "^[[:space:]]*$") ; Skip blank lines
          (let ((current-indent (current-indentation)))
            (if (or (not min-indent) (< current-indent min-indent))
                (setq min-indent current-indent))))
        (forward-line 1)))
    ;; Remove the minimum indentation
    (when min-indent
      (indent-rigidly (point-min) (point-max) (- min-indent)))))

(defun mk/batch-add-hook (hooks fn)
	(dolist (hook hooks)
		(add-hook hook fn)))

(provide 'lib-0)

;;; lib-0.el ends here
