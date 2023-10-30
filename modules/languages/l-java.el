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

;; ;; enable cape-dabbrev and cape-keyword for java-mode and java-ts-mode
;; (dolist (mode '(java-mode-hook java-ts-mode-hook))
;;   (add-hook mode
;;     '(lambda ()
;;        (setq-local completion-at-point-functions
;;          (append completion-at-point-functions '(cape-dabbrev cape-keyword))))))

(defun mk/java-local-keybinding-setup()
  (keymap-local-set "C-c C-c d" #'mk/java-open-doc)
  (keymap-local-set "C-c C-c t" #'mk/java-generate-tags))

(use-package java-ts-mode
  :elpaca nil
  :custom (java-ts-mode-indent-offset 2))

(add-hook 'java-mode-hook 'mk/java-local-keybinding-setup)
(add-hook 'java-ts-mode-hook 'mk/java-local-keybinding-setup)


(provide 'l-java)
