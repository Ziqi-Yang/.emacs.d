;;; l-java.el --- Java -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(defun mk/java-open-doc()
  "Open java17 documentation."
  (interactive)
  (browse-url "https://docs.oracle.com/en/java/javase/17/docs/api/index.html"))

(mapBegin!
  (mk/local-leader-def
	  :states 'normal
    :keymaps '(java-ts-mode-map)
    "d" #'(mk/java-open-doc :which-key "open java doc")))

(provide 'l-java)
