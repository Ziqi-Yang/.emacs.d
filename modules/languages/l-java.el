;;; l-java.el --- Java -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(defun mk/java-open-doc()
  "Open java17 documentation."
  (interactive)
  (browse-url "https://docs.oracle.com/en/java/javase/17/docs/api/index.html"))

(defun mk/java-generate-tags ()
  "Generate tags file for current java project."
  (interactive)
  (let ((compile-command "make gen_tags"))
    (project-compile)))

(mapBegin!
  (mk/local-leader-def
	  :states 'normal
    :keymaps '(java-mode-map java-ts-mode-map)
    "d" '(mk/java-open-doc :which-key "open java doc")
    "t" '(mk/java-generate-tags :which-key "generate tags")))

(dolist (mode '(java-mode-hook java-ts-mode-hook))
  (add-hook mode
    '(lambda ()
       (setq-local completion-at-point-functions
         (append completion-at-point-functions '(cape-dabbrev cape-keyword))))))


(provide 'l-java)
