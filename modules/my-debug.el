;;; my-debug.el --- Debug Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun mk/project-read-file-name ()
  (let ((find-file-advice (lambda (file-name) file-name)))
    (advice-add 'find-file :override find-file-advice)
    (unwind-protect
      (setq res (project-find-file t))
      (advice-remove 'find-file find-file-advice))))

(defun mk/debug-with-valgrind ()
  (interactive)
  (let* ((binary-path (mk/project-read-file-name))
          (compile-command (concat "valgrind " binary-path)))
    (call-interactively #'project-compile)))

(defun mk/gdb-smart ()
  "Start gdb using project root or not."
  (interactive)
  (if (project-current)
    (let ((default-directory (project-root (project-current))))
      (call-interactively #'gdb))
    (call-interactively #'gdb)))

(defun mk/gf-debug ()
  "Use gf as gdb debugger."
  (interactive)
  (let* ((binary-path (mk/project-read-file-name))
          (command (concat "gf2 " binary-path))
          (sway-fullscreen-command "swaymsg fullscreen enable")) 
    (if (project-current)
      (let ((default-directory (project-root (project-current))))
        (start-process-shell-command "gf2" "*gf2*" command))
      (start-process-shell-command "gf2" "*gf2*" command))
    (sleep-for .3)
    (start-process-shell-command "shell-command" "*shell-command*" sway-fullscreen-command)))

(defun mk/setup-gdb ()
  (setq gdb-many-windows t
    gdb-show-main t))

(add-hook 'emacs-startup-hook #'mk/setup-gdb)

(provide 'my-debug)

;;; my-debug.el ends here
