;;; file-browser.el --- File Browser -*- lexical-binding: t -*-
;;; Commentary:
;; including custom for dired
;;; Code:

(use-package dirvish
	:hook (after-init . dirvish-override-dired-mode))

(provide 'file-browser)
