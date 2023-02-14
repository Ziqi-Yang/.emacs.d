;;; file-browser.el --- File Browser -*- lexical-binding: t -*-
;;; Commentary:
;; customization for dired
;;; Code:

(setq dired-listing-switches "-agho --group-directories-first"
  dired-hide-details-hide-symlink-targets nil
  delete-by-moving-to-trash t)


;; @ icon
(use-package all-the-icons-dired
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'file-browser)
