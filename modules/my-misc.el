;;; my-misc.el --- miscellaneous configuration -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Meow King <mr.meowking@anche.no>

;;; Commentary:

;; miscellaneous configuration

;;; Code:

;; show key, for presentation usage
;; (use-package keycast
;;   :after (doom-modeline dashboard))

;; license
(use-package lice
  :ensure (:type git :host github :repo "buzztaiki/lice-el"))

(with-eval-after-load 'eww
  (setq shr-use-fonts nil)
  (define-key eww-mode-map (kbd "h") #'backward-char)
  (define-key eww-mode-map (kbd "l") #'forward-char)
  (define-key eww-mode-map (kbd "H") #'eww-back-url)
  (define-key eww-mode-map (kbd "L") #'eww-next-url)
  (define-key eww-mode-map (kbd "x") #'meow-line)
  (define-key eww-mode-map (kbd "w") #'meow-mark-word)
  (define-key eww-mode-map (kbd "W") #'meow-mark-symbol)
  (define-key eww-mode-map (kbd "y") #'meow-save)
  (define-key eww-mode-map (kbd "/") #'avy-goto-word-1)

  (define-key eww-mode-map (kbd "C-c H") #'eww-list-histories)
  (define-key eww-mode-map (kbd "C-c r") #'eww-reload))

(with-eval-after-load 'info
  (keymap-set Info-mode-map "h" #'backward-char)
  (keymap-set Info-mode-map "l" #'forward-char)
  (keymap-set Info-mode-map "H" #'Info-history-back)
  (keymap-set Info-mode-map "L" #'Info-history-forward))

(with-eval-after-load 'cus-edit
  (keymap-set Custom-mode-map "<return>" #'widget-button-press))

;; @ colorful compilation
(use-package ansi-color  ; Emacs 28 builtin
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

;;; Extra Project Root Markers ==============================
(setq project-vc-extra-root-markers
      '(".project-root"  ; my custom
        ".jj"))  ; jujutsu

(provide 'my-misc)

;;; my-misc.el ends here
