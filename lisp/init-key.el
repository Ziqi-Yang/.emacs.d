;;; init-key.el --- Key Mappings Here -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;; universal-argument conflict with evil mode(c-u, scroll up half screen), so we change it
(global-set-key (kbd "C-M-u") 'universal-argument)

;; evil
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit)) ;; <escape> to act like <C-g>
  :preface
  (setq
   evil-normal-state-cursor 'box
   evil-emacs-state-cursor  'box
   evil-insert-state-cursor 'bar
   evil-visual-state-cursor 'hollow)
  :init
  (setq
   evil-want-keybinding nil ;; evil-collection required
   evil-want-C-u-scroll t
   evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom (evil-collection-outline-bind-tab-p t)
  :config
  (evil-collection-init))

(use-package evil-escape
  :after evil
  :config
  (evil-escape-mode t)
  (setq-default evil-escape-key-sequence "kk"))

(use-package evil-surround
  :after evil)

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
  ;; :demand t ;; use mapBegin! instead
  :config
  (general-evil-setup)
  (general-create-definer mk/leader-def
    	:prefix "SPC")

  (general-create-definer mk/local-leader-def
			  :prefix "SPC m"))

(defmacro mapBegin! (&rest expression)
  "Used for defining keys. Keys are defined after package 'general' load up, so we
don't need to add ':demand t' keyword to 'use-package' declearation."
  `(use-package general
    :config
    ,@expression))

(mapBegin!
 ;; evil-surround
 (general-vmap ;; visual
   "S" #'evil-surround-region
   "C-S-c" #'clipboard-kill-ring-save)
 (general-imap ;; insert
   "C-S-v" #'clipboard-yank)
 (general-omap ;; operation
   "s" #'evil-surround-edit
   "S" #'evil-Surround-edit))

 ;; leader
 (mk/leader-def
   :states 'n
   ;; window
   "ww" #'evil-window-next

   ;; buffer
   "bb" #'buffer-menu

   ;; help
   "hf" #'describe-function
   "hk" #'describe-key
   "ho" #'describe-symbol)

(provide 'init-key)
