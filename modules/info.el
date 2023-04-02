;;; info.el --- Info -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(mapBegin!
  (general-unbind 'normal Info-mode-map
    "SPC")
  (general-define-key
    :states 'normal
    :keymaps 'Info-mode-map
    "<" #'(Info-prev :which-key "prev")
    ">" #'(Info-next :which-key "next")
    "u" #'(Info-up :which-key "up")
    "t" #'(Info-toc :which-key "toc")))

(provide 'info)
