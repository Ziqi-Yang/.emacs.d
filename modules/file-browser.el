;;; file-browser.el --- File Browser -*- lexical-binding: t -*-
;;; Commentary:
;; customization for dired
;;; Code:

(setq dired-listing-switches "-agho --group-directories-first"
  dired-hide-details-hide-symlink-targets nil
  dired-kill-when-opening-new-dired-buffer t
  delete-by-moving-to-trash t)


;; @ icon
(use-package all-the-icons-dired
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; @ side bar
(use-package dired-sidebar
  :init
  (add-hook 'dired-sidebar-mode-hook
    (lambda ()
      (unless (file-remote-p default-directory)
        (auto-revert-mode)))))

(provide 'file-browser)
