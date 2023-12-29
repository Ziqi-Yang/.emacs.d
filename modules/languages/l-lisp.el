;;; l-lisp.el --- Lisp  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(defun others/eval-buffer ()
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

(defun mk/emacs-lisp-local-keybindings-setup()
  "Set up local keybindings for scratch buffer(lisp interaction mode)"
  (keymap-local-set "TAB" #'completion-at-point)
  (keymap-local-set "C-j" #'completion-at-point)
  (keymap-local-set "C-i" #'eval-print-last-sexp)
  (keymap-local-set "C-x C-S-e" #'others/eval-buffer))

(add-hook 'lisp-interaction-mode-hook 'mk/emacs-lisp-local-keybindings-setup)
(add-hook 'emacs-lisp-mode-hook 'mk/emacs-lisp-local-keybindings-setup)

(provide 'l-lisp)

;;; 'l-lisp.el ends here
