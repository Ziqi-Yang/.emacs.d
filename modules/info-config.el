;;; info.el --- Info-mode keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun mk/info-mode-set-keybinds ()
  (keymap-local-set "h" #'backward-char)
  (keymap-local-set "l" #'forward-char)
  
  (keymap-local-set "H" #'Info-history-back)
  (keymap-local-set "L" #'Info-history-forward))

(add-hook 'Info-mode-hook #'mk/info-mode-set-keybinds)

(provide 'info-config)
