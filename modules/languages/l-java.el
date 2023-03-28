;;; l-java.el --- Java -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(defun mk/java-open-doc()
  "Open java17 documentation."
  (browse-url "https://docs.oracle.com/en/java/javase/17/docs/api/index.html"))

(mapBegin!
  (mk/local-leader-def
	  :states 'normal
    :keymaps '(java-ts-mode-map)
    "d" #'(mk/java-open-doc :which-key "open java doc")))

(add-hook 'java-ts-mode-hook
  '(lambda ()
     (setq-local completion-at-point-functions
       (append completion-at-point-functions '(cape-dabbrev cape-keyword)))))

(provide 'l-java)
