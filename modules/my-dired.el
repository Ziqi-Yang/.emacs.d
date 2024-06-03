;;; my-dired.el --- File Browser -*- lexical-binding: t -*-
;;; Commentary:
;; customization for dired
;;; Code:

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
  (dired-subtree-depth-1-face ((((background dark)) (:background "gray50"))
                               (t (:background "gray90"))))
  (dired-subtree-depth-2-face ((((background dark)) (:background "gray45"))
                               (t (:background "gray87"))))
  (dired-subtree-depth-3-face ((((background dark)) (:background "gray40"))
                               (t (:background "gray84"))))
  (dired-subtree-depth-4-face ((((background dark)) (:background "gray30"))
                               (t (:background "gray81"))))
  (dired-subtree-depth-5-face ((((background dark)) (:background "gray25"))
                               (t (:background "gray78")))))


;; (defun others/set-dired-font-face ()
;;   (face-remap-add-relative 'default :height 90))

;; ;; @ side bar
;; (use-package dired-sidebar
;;   :custom
;;   (dired-sidebar-use-custom-font t)
;;   :hook
;;   (dired-sidebar-mode . others/set-dired-font-face)
;;   ;; :custom-face
;;   ;; (dired-sidebar-face ((t (:font (font-spec :family default-font :size font-size)))))
;;   :init
;;   (add-hook 'dired-sidebar-mode-hook
;;             (lambda ()
;;               ;; (nerd-icons-dired-mode -1)
;;               (unless (file-remote-p default-directory)
;;                 (auto-revert-mode 1))))
;;   :config
;;   (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
;;   (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

;;   (setq dired-sidebar-theme 'ascii)

;;   (keymap-set dired-sidebar-mode-map "<tab>" #'dired-subtree-cycle)
;;   (keymap-set dired-sidebar-mode-map "h" #'dired-sidebar-up-directory)
;;   (keymap-set dired-sidebar-mode-map "l" #'dired-sidebar-find-file))

(with-eval-after-load 'dired
  (keymap-unset dired-mode-map "C-M-u")
  
  (keymap-set dired-mode-map "<" #'beginning-of-buffer)
  (keymap-set dired-mode-map ">" #'end-of-buffer)
  
  (keymap-set dired-mode-map "h" #'dired-up-directory)
  (keymap-set dired-mode-map "l" #'dired-find-file)
  (keymap-set dired-mode-map "O" #'dired-do-open)
  (keymap-set dired-mode-map "/" #'dired-isearch-filenames)
  (keymap-set dired-mode-map "<tab>" #'dired-subtree-cycle)
  
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-hide-details-hide-symlink-targets nil
        dired-kill-when-opening-new-dired-buffer t
        delete-by-moving-to-trash t
        ;; so dual panel copy works
        dired-dwim-target t)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))


;;; Custom functions ===========================================================

(defun mk/dired-jump (&optional arg)
  "Open smart considering whether it's in a project.
ARG: universal argument."
  (interactive "P")
  (if (and arg (project-current))
      (project-dired)
    (dired-jump)))


(provide 'my-dired)

;;; my-dired.el ends here
