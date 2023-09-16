;;; l-lisp.el --- Lisp  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(defun mk/emacs-lisp-local-keybindings-setup()
  "Set up local keybindings for scratch buffer(lisp interaction mode)"
  (keymap-local-set "C-j" #'complete-symbol)
  (keymap-local-set "C-i" #'eval-print-last-sexp))

(add-hook 'lisp-interaction-mode-hook 'mk/emacs-lisp-local-keybindings-setup)
(add-hook 'emacs-lisp-mode-hook 'mk/emacs-lisp-local-keybindings-setup)

(provide 'l-lisp)

;;; 'l-lisp.el ends here
