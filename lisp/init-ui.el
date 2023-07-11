;;; init-ui.el --- UI settings -*- lexical-binding: t -*-
;;; Commentary:
;; primiary settings are in the early-init.el file, while extended setting like theme and font are here
;;; Code:

;;; Theme ===================================================
;; @ editor theme
(use-package doom-themes
	:defer t
	;; :hook (server-after-make-frame .
  ;;         (lambda ()
  ;;           (progn
  ;;             (load-theme 'doom-one-light t))))
	:init
	;; (load-theme 'doom-solarized-light t)
  ;; (progn
	;;   (load-theme 'doom-earl-grey t))
  :config
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-one-light")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package almost-mono-themes
  :hook (server-after-make-frame .
          (lambda ()
            (progn
              (load-theme 'almost-mono-white t))))
  :config
  ;; (load-theme 'almost-mono-black t)
  ;; (load-theme 'almost-mono-gray t)
  ;; (load-theme 'almost-mono-cream t)
  (progn
	  (load-theme 'almost-mono-white t)))

(defun mk/setup-font-lock()
  "Set minimum font lock level for both treesit and font-lock"
  ;; (setq treesit-font-lock-level 1) ;; treesit NOTE change back to 1
  (setq font-lock-maximum-decoration 1) ;; font lock
  ;; setup jit-lock
  (setq jit-lock-chunk-size 4096
    jit-lock-defer-time 0.25
    jit-lock-stealth-time 1.25))

(add-hook 'after-init-hook #'mk/setup-font-lock())

;; icon theme
(defun mk/check-and-install-all-the-icons()
  "Check and install all-the-icons font if in emacs GUI(client and single)."
  (interactive)
  (if (display-graphic-p)
    (when (not (member "all-the-icons" (font-family-list)))
      (all-the-icons-install-fonts t))
    nil))
(use-package all-the-icons
  :config
  (mk/check-and-install-all-the-icons)
  (add-hook 'server-after-make-frame-hook #'mk/check-and-install-all-the-icons))

(use-package all-the-icons-completion
	:after all-the-icons
	:hook (after-init . all-the-icons-completion-mode))

;;; dim unreal buffer =======================================
(use-package solaire-mode
	:hook ((after-init . solaire-global-mode ))
	:config
	;; https://github.com/hlissner/emacs-solaire-mode/issues/28#issuecomment-968126872
	;; disable solaire mode in dashboard, since the banner background doesn't change(one way:
	(setq solaire-mode-real-buffer-fn '(lambda ()
																			 (or (solaire-mode-real-buffer-p)
																				 (equal (buffer-name) "*dashboard*")))))

;;; 80 column indicator =====================================
(global-display-fill-column-indicator-mode 1)
;; need to explicit set value to 80 (due to olivetti mode?)
(setq-default display-fill-column-indicator-column 80)

;;; Dashboard ===============================================

;; (add-hook 'dashboard-mode-hook '(lambda () (progn (setq buffer-face-mode-face '(:family "Zpix" :height 120))
;; 																						 (buffer-face-mode)) ))
(use-package dashboard
  :config
	;; configuration for emacsclient
	;; (set-face-background 'dashboard-banner-logo-title nil) ;; solaire-mode integration
	(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
	;; icons display in the emacsclient
	(add-hook 'server-after-make-frame-hook  '(lambda () (dashboard-refresh-buffer)))
  (dashboard-setup-startup-hook)
	(setq dashboard-projects-backend 'project-el
		dashboard-center-content t
		dashboard-set-heading-icons t
		dashboard-set-file-icons t
		dashboard-set-navigator t
		dashboard-set-init-info t
		dashboard-startup-banner (concat user-emacs-directory "assets/banners/ue-dark-small.png")
		dashboard-banner-logo-title "Bocchi the rock!"
		dashboard-items '((recents  . 5)
											 ;; (bookmarks . 5)
											 (projects . 5)
											 (agenda . 5)
											 ;; (registers . 5)
											 )
		)

	;; Format: "(icon title help action face prefix suffix)"
	(setq dashboard-navigator-buttons
		`(((nil
				 ,(concat "Email [" (shell-command-to-string "~/myBin/get-mu-unread-emails-num") "]") nil (lambda (&rest _) (mu4e)))
				(nil
					"Todos" nil (lambda (&rest _) (find-file "~/notes/agenda.org")))
        ))))

;;; font settings ===========================================
;; Set the font face based on platform
;; @ default font
(defun mk/setup-font-faces ()
	"Setup Fonts."
	;; font faces only works in emacs GUI, terminal emcas should change terminal font instead
  (let ((default-font "Cascadia Code") ;; IBM Plex Mono
         (font-size 19)
         (CJK-font "LXGW WenKai"))
    (when (display-graphic-p) 
		  (when (member default-font (font-family-list))
			  (set-face-attribute 'default nil :font (font-spec :family default-font :size font-size)))
		  ;; @ fixed-pitch font ;; i.e. Monospaced font
		  ;;(when (member "BlexMono Nerd Font" (font-family-list))
		  ;;(set-face-attribute 'fixed-pitch nil :font (font-spec :family "BlexMono Nerd Font" :size 13.5)))
		  (set-face-attribute 'fixed-pitch nil :inherit 'default)
		  ;; @ variable-pitch font ;; i.e. variable-width font
		  (set-face-attribute 'variable-pitch nil :inherit 'default)
		  ;; @ CJK font 包括中文、日语、韩语中的汉字，但是不包含日语假名
		  (when (member CJK-font (font-family-list))
			  (set-fontset-font t 'han (font-spec :family CJK-font :size font-size)))
		  ;; @ Japanese Kana 日语假名
		  (when (member CJK-font (font-family-list))
			  (set-fontset-font t 'kana (font-spec :family CJK-font :size font-size))
		    ;; @ symbol font ('symbol)
		    ;; @ emoji ('emoji)
		    ;; 
		    ;; reference:
		    ;;   1. http://xahlee.info/emacs/emacs/emacs_set_font_emoji.html
		    ;;   2. https://emacs-china.org/t/emacs/22193/6
			  )
		  )
    )
	)
;; run this hook after we have initialized the first time
(add-hook 'after-init-hook 'mk/setup-font-faces)
;; re-run this hook if we create a new frame from daemonized Emacs
(add-hook 'server-after-make-frame-hook 'mk/setup-font-faces)

;;; mode line ===============================================
;; @ doom modeline
(use-package doom-modeline
	:after all-the-icons
  :init (doom-modeline-mode 1))

;;; Navigation Highlight ====================================
(use-package beacon
	:defer 1
	:hook ((after-init . (lambda () (beacon-mode 1)))
				  ;; disable in org-tree-slide
				  (org-tree-slide-play . (lambda () (beacon-mode -1)))
				  (org-tree-slide-stop . (lambda () (beacon-mode 1)))))

;;; Center Area =============================================
(use-package olivetti
  ;; :hook ((text-mode         . olivetti-mode)
  ;;         (prog-mode         . olivetti-mode)
  ;;         (Info-mode         . olivetti-mode)
  ;;         (org-mode          . olivetti-mode)
  ;;         (markdown-mode     . olivetti-mode))
  :custom
  (olivetti-body-width 111))

(use-package auto-olivetti
  :straight (:type git :host sourcehut :repo "ashton314/auto-olivetti")
  :custom
  (auto-olivetti-enabled-modes '(text-mode prog-mode))
  :config
  (auto-olivetti-mode))

;;; Cursor ==================================================
;; @ disable cursor blink
(setq blink-cursor-mode nil)

;;; Compilation =============================================
;; @ colorful
(use-package ansi-color ;; @ emacs 28 buildin
  :hook (compilation-filter . ansi-color-compilation-filter))

;;; Hi-lock =================================================
;; to highlight-symbol-at-point (which is integrated to `mk/evil-search-symbol-forward')

(setq hi-lock-face-defaults '("mk/face/hi-yellow" "mk/face/hi-pink" "hi-green" "hi-blue" "hi-salmon" "hi-aquamarine" "hi-black-b" "hi-blue-b" "hi-red-b" "hi-green-b" "hi-black-hb"))

(defface mk/face/hi-yellow
  '((t (:background "#feca57")))
  "Custom face for hi-lock mode."
  :group 'hi-lock-faces)

(defface mk/face/hi-pink
  '((t (:background "#ff9ff3")))
  "Custom face for hi-lock mode."
  :group 'hi-lock-faces)

(provide 'init-ui)
