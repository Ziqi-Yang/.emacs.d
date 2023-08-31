;;; l-cc.el --- c/c++ -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(defun mk/cc-local-keybinding-setup()
  (keymap-local-set "C-c C-c m" #'mk/better-consult-man) 
  (keymap-local-set "C-c C-c f" #'ff-find-other-file))

(add-hook 'c-ts-mode-hook 'mk/cc-local-keybinding-setup)
(add-hook 'c++-ts-mode-hook 'mk/cc-local-keybinding-setup)

(provide 'l-cc)

;;; l-cc.el ends here
