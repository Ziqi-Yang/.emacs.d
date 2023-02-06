;;; init-key.el --- Key Mappings Here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; universal-argument conflict with evil mode(c-u, scroll up half screen), so we change it
(global-set-key (kbd "C-M-u") 'universal-argument)

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

(use-package evil-nerd-commenter
	:general ([remap comment-line] #'evilnc-comment-or-uncomment-lines))

;; which-key
(use-package which-key
  :init (which-key-mode)
  ;; :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-side-window-max-height 0.3))


(defmacro mapBegin! (&rest expression)
  "Used for defining keys. Keys are defined after package 'general' load up, so we
don't need to add ':demand t' keyword to 'use-package' declearation."
  `(use-package general
    :config
    ,@expression))

(mapBegin!
 ;; evil-surround
 (general-nmap ;; normal
	 "gcc" #'evilnc-comment-or-uncomment-lines

	 ;; @ fold
	 ;; via init-base/evil-vimish-mode
	 ;; zf(create) -> za/zc/zo(toggle/close/open) -> zd(delete)
	 )

 (general-vmap ;; visual
   "S" #'evil-surround-region
   "C-S-c" #'clipboard-kill-ring-save
	 "gc" #'evilnc-comment-or-uncomment-lines)
 (general-imap ;; insert
   "C-S-v" #'clipboard-yank)
 (general-omap ;; operation
   "s" #'evil-surround-edit
   "S" #'evil-Surround-edit))

 ;; leader
 (mk/leader-def
   :states 'n
   ;; file
   "f" '(:ignore t :which-key "file")
   "ff" #'find-file
	 
   ;; window
   "w"  '(:ignore t :which-key "window")
   "ww" #'evil-window-next

   ;; buffer
   "b"  '(:ignore t :which-key "buffer")
   "bb" #'buffer-menu

   ;; help
   "h" '(:ignore t :which-key "help")
   "hf" #'describe-function
   "hk" #'describe-key
   "ho" #'describe-symbol
   "hm" #'describe-mode

   ;; toggle
   "t" '(:ignore t :which-key "toggle")
   "tw" 'whitespace-mode
   "tt" '(counsel-load-theme :which-key "choose theme"))

(provide 'init-key)
