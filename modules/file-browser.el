;;; file-browser.el --- File Browser -*- lexical-binding: t -*-
;;; Commentary:
;; customization for dired
;;; Code:

(setq dired-listing-switches "-agho --group-directories-first"
  dired-hide-details-hide-symlink-targets nil
  dired-kill-when-opening-new-dired-buffer t
  delete-by-moving-to-trash t)


;; @ icon
(defun mk/dired-subtree-add-nerd-icons ()
  (interactive)
  (revert-buffer))

(defun mk/dired-subtree-toggle-nerd-icons ()
  (when (require 'dired-subtree nil t)
    (if nerd-icons-dired-mode
      (progn
        (advice-add #'dired-subtree-toggle :after #'mk/dired-subtree-add-nerd-icons)
        (advice-add #'dired-subtree-cycle :after #'mk/dired-subtree-add-nerd-icons))
      (advice-remove #'dired-subtree-toggle #'mk/dired-subtree-add-nerd-icons)
      (advice-remove #'dired-subtree-cycle #'mk/dired-subtree-add-nerd-icons))))

(use-package nerd-icons-dired
  :hook ((dired-mode . nerd-icons-dired-mode)
          (nerd-icons-dired-mode . mk/dired-subtree-toggle-nerd-icons)))

(use-package dired-subtree
  :custom
  (dired-subtree-cycle-depth 5)
  :custom-face
  (dired-subtree-depth-1-face ((t (:background "gray25"))))
  (dired-subtree-depth-2-face ((t (:background "gray30"))))
  (dired-subtree-depth-3-face ((t (:background "gray35"))))
  (dired-subtree-depth-4-face ((t (:background "gray40"))))
  (dired-subtree-depth-5-face ((t (:background "gray45"))))
  (dired-subtree-depth-6-face ((t (:background "gray50")))))

;; @ side bar
(use-package dired-sidebar
  :init
  (add-hook 'dired-sidebar-mode-hook
    (lambda ()
      ;; (nerd-icons-dired-mode -1)
      (unless (file-remote-p default-directory)
        (auto-revert-mode 1))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  
  (setq dired-sidebar-theme 'ascii)
  
  (keymap-set dired-sidebar-mode-map "<tab>" #'dired-subtree-cycle)
  (keymap-set dired-sidebar-mode-map "h" #'dired-sidebar-up-directory)
  (keymap-set dired-sidebar-mode-map "l" #'dired-sidebar-find-file))

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

  (keymap-set dired-mode-map "<tab>" #'dired-subtree-cycle))

(provide 'file-browser)
