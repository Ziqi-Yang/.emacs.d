;;; file-browser.el --- File Browser -*- lexical-binding: t -*-
;;; Commentary:
;; customization for dired
;;; Code:

(setq dired-listing-switches "-agho --group-directories-first"
  dired-hide-details-hide-symlink-targets nil
  dired-kill-when-opening-new-dired-buffer t
  delete-by-moving-to-trash t)


;; @ icon
;; (use-package all-the-icons-dired
;;   :after dired
;;   :hook (dired-mode . all-the-icons-dired-mode))
(use-package nerd-icons-dired
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; @ side bar
(use-package dired-sidebar
  :init
  (add-hook 'dired-sidebar-mode-hook
    (lambda ()
      (unless (file-remote-p default-directory)
        (auto-revert-mode)))))

(defun mk/dired-find-file ()
  "Like `find-file' but with `default-directory' set to the
one specified by listing header."
  (interactive)
  (let ((default-directory (dired-current-directory)))
    (call-interactively #'find-file)))

(defun mk/dired-local-keybinding-setup()
  (keymap-local-set "/" #'dired-isearch-filenames)
  (keymap-local-set "M-f" #'mk/dired-find-file)
  (keymap-local-set "h" #'dired-up-directory)
  (keymap-local-set "l" #'dired-find-file))

(add-hook 'dired-mode-hook #'mk/dired-local-keybinding-setup)

(provide 'file-browser)
