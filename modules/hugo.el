;;; hugo.el --- hugo -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defvar mk/hugo-root "~/Documents/blog-meow/"
  "Hugo blog root.")

(defvar mk/hugo-preview-command "open http://127.0.0.1:1313 && hugo server -D"
  "Hugo preview command & process-name")

(defun mk/hugo/execute-command (command &optional wait)
  "Execute a command at hugo root directory."
  (interactive "sCommand:")
  (if (and (project-current) (string= (project-root (project-current)) mk/hugo-root))
    (let ((default-directory mk/hugo-root))
      (if wait
        (unless (eq (shell-command command "*hugo*" "*hugo-error*") 0)
          (user-error "execute command failed"))
        (start-process-shell-command command "*hugo*" command))
      (message command))
    (user-error "Please make sure that you are in the blog (sub)directory.")))

(defun mk/hugo/cd-project()
  "Use project-switch-project to change into blog path."
  (interactive)
  (project-switch-project mk/hugo-root))

(defun mk/hugo/preview()
  "Start hugo server to preview your site(with draft)."
  (interactive)
  (mk/hugo/execute-command mk/hugo-preview-command))

(defun mk/hugo/stop-preview()
  "Stop hugo server process."
  (interactive)
  (delete-process mk/hugo-preview-command)
  (message "preview disabled"))

(defun mk/hugo/toggle-preview()
  "Toggle perview (with draft)."
  (interactive)
  (if (get-process mk/hugo-preview-command)
    (mk/hugo/stop-preview)
    (mk/hugo/preview)))

(defun mk/hugo/build()
  "Build without draft."
  (interactive)
  (mk/hugo/execute-command "hugo"))

(defun mk/hugo/new-file (filename)
  "Hugo new file"
  (interactive "sSection/FileName.<Ext>:")
  (mk/hugo/execute-command (concat "hugo new " filename) t)
  (find-file (expand-file-name (concat "content/" filename) mk/hugo-root )))

(provide 'l-hugo)
