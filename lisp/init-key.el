;;; init-key.el --- Key Mappings Here -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;; use escape key to cancel all(originally c-g)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; universal-argument conflict with evil mode(c-u, scroll up half screen), so we change it
(global-set-key (kbd "C-M-u") 'universal-argument)

;; evil
(use-package evil
  :init
  (setq
   evil-want-keybinding nil ;; evil-collection required
   evil-want-C-u-scroll t
   evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; which-key
(use-package which-key
  :init (which-key-mode)
  ;; :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-side-window-max-height 0.3))

;; general: https://github.com/noctuid/general.el
;; define keybindings with ease
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer mk/leader-def
			  :prefix "SPC")

  (general-create-definer mk/local-leader-def
			  :prefix "SPC m"))

(provide 'init-key)
