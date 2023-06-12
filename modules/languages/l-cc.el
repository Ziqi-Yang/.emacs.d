;;; l-cc.el --- c/c++ -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(defun mk/cc-local-keybinding-setup()
  (keymap-local-set "M-m" #'woman))

(add-hook 'c-ts-mode-hook 'mk/cc-local-keybinding-setup)
(add-hook 'c++-ts-mode-hook 'mk/cc-local-keybinding-setup)


(provide 'l-cc)
