;;; emacs-developer.el --- Third Pary Libraries to develop your own emacs libraries -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package polymode)
(use-package package-lint)

;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; M-x package-refresh-contents to refresh package-archive-contents

(provide 'emacs-developer)
