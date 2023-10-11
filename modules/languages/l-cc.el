;;; l-cc.el --- c/c++ -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(defun mk/cc-local-keybinding-setup()
  (keymap-local-set "RET" #'newline-and-indent)
  (keymap-local-set "C-c C-c m" #'mk/better-consult-man) 
  (keymap-local-set "C-c C-c f" #'ff-find-other-file)
  (keymap-local-set "C-c C-c p" #'c-ts-prototype-copy-proto))

(use-package c-ts-prototype
  :elpaca (:type git :host sourcehut :repo "meow_king/c-ts-protoype"))

(add-hook 'c-ts-mode-hook 'mk/cc-local-keybinding-setup)
(add-hook 'c++-ts-mode-hook 'mk/cc-local-keybinding-setup)

(setq c-ts-mode-indent-style 'linux)

(provide 'l-cc)

;;; l-cc.el ends here
