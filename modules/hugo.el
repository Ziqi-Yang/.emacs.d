;;; hugo.el --- hugo -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defvar mk/hugo-root "~/Documents/blog-meow/" ;; must start with ~
  "Hugo blog root.")

(defvar mk/hugo-content-dir (expand-file-name "content" mk/hugo-root)
  "Hugo blog content directory.")

(defvar mk/hugo-preview-command "firefox --new-window http://127.0.0.1:1313 && hugo server -D"
  "Hugo preview command & process-name")

(defun mk/hugo/execute-command (command &optional wait force-cd get-result)
  "Execute a command at hugo root directory.
Arguments:
command: command executed at hugo blog root.
wait: whether or not should we wait the command execution.
force-cd:
  t: temporarily change into the blog directory and execute the command
  nil: should be in blog directory.
get-result:
  t: ignore wait and force-cd(functions like all they are true).
     Execute command and return the result.
  nil: nil"
  (interactive "sCommand:")
  (let ((default-directory default-directory))
    (if (or force-cd get-result)
      (setq default-directory mk/hugo-root)
      nil)
    (if (and (project-current) (string= (project-root (project-current)) mk/hugo-root))
      (let ((default-directory mk/hugo-root))
        (if get-result
          (shell-command-to-string command)
          (progn
            (if wait
              (unless (eq (shell-command command "*hugo*" "*hugo-error*") 0)
                (user-error "execute command failed"))
              (start-process-shell-command command "*hugo*" command))
            (message command))))
      (user-error "Please make sure that you are in the blog (sub)directory."))))

(defun mk/hugo/cd-project()
  "Use project-switch-project to change into blog path."
  (interactive)
  (project-switch-project mk/hugo-root))

(defun mk/hugo/preview()
  "Start hugo server to preview your site(with draft)."
  (interactive)
  (mk/hugo/execute-command mk/hugo-preview-command nil t))

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
  (mk/hugo/execute-command "hugo --minify --gc --cleanDestinationDir"))

(defun mk/hugo/edit-or-create ()
  "Edit or create a blog."
  (interactive)
  (let* ( (direname (completing-read "Section/FileName.<Ext>:" (directory-files mk/hugo-content-dir nil "\\`[^.]*\\'")))
          (filename (concat direname "/" (completing-read "File:"  (directory-files (expand-file-name direname mk/hugo-content-dir) nil "\\`[^.].*\\'"))))
          (file (expand-file-name filename mk/hugo-content-dir)))
    (unless (file-exists-p file) ;; create file if not exists
      (mk/hugo/execute-command (concat "hugo new " filename) t t))
    (find-file file)))

(defun mk/hugo/goto-draft()
  "Goto draft."
  (interactive)
  (let* ((draft (completing-read "draft:"
                  (split-string (mk/hugo/execute-command "hugo list drafts" nil nil t) "\n" t)))
          (draft-file (expand-file-name draft mk/hugo-root)))
    (find-file draft-file)))

(provide 'hugo)
