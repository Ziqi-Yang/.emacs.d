;;; init-ui.el --- UI settings -*- lexical-binding: t -*-
;;; Commentary:
;; primiary settings are in the early-init.el file, while extended setting like theme and font are here
;;; Code:

;;; Theme ===================================================
;; @ editor theme
;; (use-package doom-themes
;; 	;; :hook (server-after-make-frame .
;;   ;;         (lambda ()
;;   ;;           (progn
;;   ;;             (load-theme 'doom-one-light t))))
;; 	:init
;; 	;; (load-theme 'doom-solarized-light t)
;;   ;; (progn
;; 	;;   (load-theme 'doom-earl-grey t))
;;   :config
;;   (doom-themes-visual-bell-config)
;;   (setq doom-themes-treemacs-theme "doom-one-light")
;;   (doom-themes-treemacs-config)
;;   (doom-themes-org-config))

;; (use-package almost-mono-themes
;;   :ensure (:host github :repo "Ziqi-Yang/almost-mono-themes")
;;   :hook (server-after-make-frame .
;; 				  (lambda ()
;; 				    (progn
;; 				      (load-theme 'almost-mono-gray t))))
;;   :config
;;   ;; (load-theme 'almost-mono-black t)
;;   (load-theme 'almost-mono-gray t)
;;   ;; (load-theme 'almost-mono-cream t)
;;   ;; (load-theme 'almost-mono-white t)
;;   )

(defun mk/setup-theme()
  "Load theme."
  (require-theme 'modus-themes)
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-custom-auto-reload t
        modus-themes-disable-other-themes t
        
        modus-themes-prompts '(italic bold)
        modus-themes-completions
        '((matches . (extrabold))
          (selection . (semibold italic text-also))))
  (if (display-graphic-p)
      (load-theme 'modus-operandi t)
    (load-theme 'modus-vivendi t)))

(add-hook 'after-init-hook #'mk/setup-theme)
(add-hook 'server-after-make-frame-hook #'mk/setup-theme)

(use-package delight)

(defun mk/setup-font-lock()
  "Set minimum font lock level for both treesit and font-lock"
  ;; (setq treesit-font-lock-level 1) ;; treesit NOTE change back to 1
  (setq font-lock-maximum-decoration 3) ;; font lock
  ;; setup jit-lock
  (setq jit-lock-chunk-size 4096
	      jit-lock-defer-time 0
	      jit-lock-stealth-time 1.5
        jit-lock-stealth-nice 0.2))

(add-hook 'after-init-hook #'mk/setup-font-lock())

;; icon theme

(use-package nerd-icons)

;; (defun mk/check-and-install-all-the-icons()
;;   "Check and install all-the-icons font if in emacs GUI(client and single)."
;;   (interactive)
;;   (when (display-graphic-p)
;;     (when (not (member "all-the-icons" (font-family-list)))
;;       (all-the-icons-install-fonts t))))

;; (use-package all-the-icons
;;   :config
;;   (mk/check-and-install-all-the-icons)
;;   (add-hook 'server-after-make-frame-hook #'mk/check-and-install-all-the-icons))

;; (use-package all-the-icons-completion
;; 	:after all-the-icons
;; 	:hook (after-init . all-the-icons-completion-mode))

;;; dim unreal buffer =======================================
;; (use-package solaire-mode
;;   :config
;;   (solaire-global-mode)
;;   ;; https://github.com/hlissner/emacs-solaire-mode/issues/28#issuecomment-968126872
;;   ;; disable solaire mode in dashboard, since the banner background doesn't change(one way:
;;   (setq solaire-mode-real-buffer-fn '(lambda ()
;; 				                               (or (solaire-mode-real-buffer-p)
;; 					                               (equal (buffer-name) "*dashboard*")))))

;;; 80 column indicator =====================================
(global-display-fill-column-indicator-mode 1)
;; need to explicit set value to 80 (due to olivetti mode?)
(setq-default display-fill-column-indicator-column 80)

;;; Dashboard ===============================================

;; (add-hook 'dashboard-mode-hook '(lambda () (progn (setq buffer-face-mode-face '(:family "Zpix" :height 120))
;; 																						 (buffer-face-mode)) ))

;; (use-package dashboard
;;   :config
;;   ;; configuration for emacsclient
;;   ;; (set-face-background 'dashboard-banner-logo-title nil) ;; solaire-mode integration
;;   ;; icons display in the emacsclient
;;   (add-hook 'server-after-make-frame-hook  #'(lambda () (dashboard-refresh-buffer)))
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-projects-backend 'project-el
;; 	  dashboard-center-content t
;; 	  dashboard-set-heading-icons t
;; 	  dashboard-set-file-icons t
;; 	  dashboard-set-navigator t
;; 	  dashboard-set-init-info t
;; 	  dashboard-startup-banner (concat user-emacs-directory "assets/banners/ue-dark-small.png")
;; 	  dashboard-banner-logo-title "El Psy Kongaroo"
;; 	  dashboard-items '((recents  . 5)
;; 			                 ;; (bookmarks . 5)
;; 			                 (projects . 5)
;; 			                 ;; (agenda . 5)
;; 			                 ;; (registers . 5)
;; 			                 ))

;;   ;; Format: "(icon title help action face prefix suffix)"
;;   (setq dashboard-navigator-buttons
;; 	  `(((nil
;; 	       ,(concat "Email [" (shell-command-to-string "~/myBin/get-mu-unread-emails-num") "]") nil (lambda (&rest _) (mu4e)))
;; 	      (nil
;; 	        "Todos" nil (lambda (&rest _) (find-file "~/notes/agenda.org")))
;;         ))))


;;; font settings ===========================================
;; font faces only works in emacs GUI, terminal emcas should change terminal font instead
(defun mk/setup-font-faces ()
  "Setup Fonts."
  (let ((default-font "Iosevka")  ; , IBM Plex Mono, Cascadia Code
        (CJK-font "LXGW Neo XiHei")
        (font-size 30))
    (when (display-graphic-p)
      (when (member default-font (font-family-list))
        ;; @ default font
	      (set-face-attribute 'default nil :font (font-spec :family default-font :size font-size))
        ;; @ fixed-pitch font ;; i.e. Monospaced font
        (set-face-attribute 'fixed-pitch nil :inherit 'default)
        ;; @ variable-pitch font ;; i.e. variable-width font
        (set-face-attribute 'variable-pitch nil :inherit 'default)
        ;; @ CJK font 包括中文、日语、韩语中的汉字，但是不包含日语假名
        (when (member CJK-font (font-family-list))
	        (set-fontset-font t 'han (font-spec :family CJK-font :size font-size)))
        ;; @ Japanese Kana 日语假名
        ;; (when (member CJK-font (font-family-list))
	      ;;   (set-fontset-font t 'kana (font-spec :family CJK-font :size font-size))
        ;; @ unicode ('unicode)
	      ;; @ symbol font ('symbol)
	      ;; @ emoji ('emoji)
	      ;; 
	      ;; reference:
	      ;;   1. http://xahlee.info/emacs/emacs/emacs_set_font_emoji.html
	      ;;   2. https://emacs-china.org/t/emacs/22193/6
	      ))))
;; run this hook after we have initialized the first time
(add-hook 'after-init-hook 'mk/setup-font-faces)
;; re-run this hook if we create a new frame from daeamonized Emacs
(add-hook 'server-after-make-frame-hook 'mk/setup-font-faces)

;;; Mode line & Header line ====================================================
;; regarding the configuration of the header-line, see `init-ui'
(use-package eldoc-headline
  :ensure (:type git :host sourcehut :repo "meow_king/eldoc-headline")
  :delight eldoc-headline-local-mode
  :custom (eldoc-headline-disable-echo-area t)
  :config
  (eldoc-headline-mode 1))

(use-package breadcrumb
  :ensure (:host github :repo "joaotavora/breadcrumb"))

;; `prjoect-current' can cause performance issue(IO) in some cases, so I use a
;; variable to store the result
(defvar-local mk/vars/project-file-name nil)

(defun mk/mode-line/abbreviate-file-name ()
  (unless mk/vars/project-file-name
    (setq-local mk/vars/project-file-name (breadcrumb-project-crumbs)))
  mk/vars/project-file-name)

(defun mk/setup-modeline ()
  (setq-default mode-line-buffer-identification
                '((:eval (mk/mode-line/abbreviate-file-name)))))

(defun mk/setup-header-line()
  (setq-default
   header-line-format
   '((:eval eldoc-headline-string)
     (:propertize " # " face error)
     (:eval (breadcrumb-imenu-crumbs)))))

(with-eval-after-load 'emacs
  (mk/setup-modeline)
  (mk/setup-header-line))

;;; Enhanced Calc (Use C-o to define key)
(use-package casual
  :ensure (:host github :repo "kickingvegas/Casual"))

;;; Display Buffer Alist ========================================================
(setq display-buffer-alist
      '(("\\*Embark Collect" (display-buffer-in-side-window) (side . bottom)
         (height . 0.4))
        ("\\*Typst-Watch\\*" display-buffer-at-bottom (window-height . fit-window-to-buffer))
        ("\\*Embark Actions\\*" (display-buffer-in-tab) (side . right)
         (width . 0.3))
        ("\\*Shortdoc.*?\\*" display-buffer-in-side-window (side . right)
         (window-width . 0.5))
        ("\\*SQLite.*?\\*" (display-buffer-same-window) (reusable-frames))
        ("\\*info\\*" display-buffer-in-side-window (side . right)
         (window-width . 0.5))
        ("\\*Man.*?\\*" display-buffer-in-side-window (side . right)
         (window-width . 0.5))
        ("*Help*" display-buffer-in-side-window (side . right)
         (window-width . 0.5))
        ("\\*helpful.*?\\*" display-buffer-in-side-window (side . right)
         (window-width . 0.5))
        ("\\*vc-git.*?\\*" (display-buffer-in-side-window) (side . top))
        ((derived-mode . compilation-mode) (display-buffer-in-side-window)
         (side . right) (window-width . 0.5))
        ("^\\*tree-sitter explorer for [^z-a]+\\*"
         display-buffer-in-side-window (side . right) (window-width . 70))
        ("^\\*Dictionary\\*" display-buffer-in-side-window (side . left)
         (window-width . 100))
        ("^\\*ielm\\*"
         display-buffer-in-side-window (side . bottom))
        ("^\\*Flymake diagnostics for.*?\\*"
         display-buffer-in-side-window (side . bottom))))

(provide 'init-ui)

;;; init-ui.el ends here
