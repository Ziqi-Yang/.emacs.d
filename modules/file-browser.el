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

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "h" #'dired-up-directory)
  (keymap-set dired-mode-map "l" #'dired-find-file)
  
  (keymap-set dired-mode-map "/" #'dired-isearch-filenames)
  (keymap-set dired-mode-map "M-f" #'mk/dired-find-file)

  (keymap-set dired-mode-map "<tab>" #'dired-subtree-toggle))

(provide 'file-browser)
