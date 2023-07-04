;;; l-lisp.el --- Lisp  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:


(defun scratch-local-keybindings-serup()
  "Set up local keybindings for scratch buffer(lisp interaction mode)"
  (keymap-local-set "C-j" #'complete-symbol))

(add-hook 'lisp-interaction-mode-hook 'scratch-local-keybindings-serup)

(provide 'l-lisp)
