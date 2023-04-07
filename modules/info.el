;;; info.el --- Info-mode keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; FIXME don't know why this file is not loaded
(defun mk/hello()
  (interactive))

(defun mk/info-mode-set-keybinds ()
  (interactive)
  (general-unbind 'normal Info-mode-map
    "SPC")
  (general-nmap
    :keymaps 'Info-mode-map 
    "p" #'(Info-prev :which-key "prev")
    "n" #'(Info-next :which-key "next")
    "u" #'(Info-up :which-key "up")
    "t" #'(Info-toc :which-key "toc")))

(add-hook 'Info-mode-hook #'mk/info-mode-set-keybinds)

(provide 'info)
