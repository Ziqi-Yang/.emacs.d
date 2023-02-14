;;; init-ui.el --- UI settings -*- lexical-binding: t -*-
;;; Commentary:
;; primiary settings are in the early-init.el file, while extended setting like theme and font are here
;;; Code:

;;; Theme ===================================================
;; @ editor theme
(use-package doom-themes
	:defer t
	:hook (server-after-make-frame . (lambda () (load-theme
																							  'doom-solarized-light t)))
	:init
	(load-theme 'doom-solarized-light t))

;; @ icon theme
(use-package all-the-icons
	;; comment the below line to enable emacsclient capibility, but not perfect.
	;; :if (display-graphic-p)
  :config
  (when (not (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t)))

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

;;; Dashboard ===============================================

(add-hook 'dashboard-mode-hook '(lambda () (progn (setq buffer-face-mode-face '(:family "Zpix" :height 140))
																									(buffer-face-mode)) ))
(use-package dashboard
	:after all-the-icons
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
				dashboard-banner-logo-title "Happy coding, MeowKing!"
				dashboard-items '((recents  . 5)
													;; (bookmarks . 5)
													(projects . 5)
													;; (agenda . 5)
													;; (registers . 5)
													)
				)

	;; Format: "(icon title help action face prefix suffix)"
	(setq dashboard-navigator-buttons
				`(((,(all-the-icons-octicon "octoface" :height 1.1 :v-adjust 0.0)
						"Homepage" "Browse homepage" (lambda (&rest _) (browse-url "https://github.com/Ziqi-Yang")))
					 (,(all-the-icons-faicon "user-secret" :height 1.0 :v-adjust 0.0)
						"Load Sessoin" nil (lambda (&rest _) (desktop-read))))
					)))

;;; font settings ===========================================
;; Set the font face based on platform
;; @ default font
(defun mk/setup-font-faces ()
	"Setup Fonts."
	;; font faces only works in emacs GUI, terminal emcas should change terminal font instead
	(when (display-graphic-p) 
		(when (member "BlexMono Nerd Font" (font-family-list))
			(set-face-attribute 'default nil :font (font-spec :family "BlexMono Nerd Font" :size 13.5)))
		;; @ fixed-pitch font ;; i.e. Monospaced font
		;;(when (member "BlexMono Nerd Font" (font-family-list))
		;;(set-face-attribute 'fixed-pitch nil :font (font-spec :family "BlexMono Nerd Font" :size 13.5)))
		(set-face-attribute 'fixed-pitch nil :inherit 'default)
		;; @ variable-pitch font ;; i.e. variable-width font
		(set-face-attribute 'variable-pitch nil :inherit 'default)
		;; @ CJK font 包括中文、日语、韩语中的汉字，但是不包含日语假名
		(when (member "LXGW WenKai" (font-family-list))
			(set-fontset-font t 'han (font-spec :family "LXGW WenKai" :size 13.5)))
		;; @ Japanese Kana 日语假名
		(when (member "LXGW WenKai" (font-family-list))
			(set-fontset-font t 'kana (font-spec :family "LXGW WenKai" :size 13.5))
		;; @ symbol font ('symbol)
		;; @ emoji ('emoji)
		;; 
		;; reference:
		;;   1. http://xahlee.info/emacs/emacs/emacs_set_font_emoji.html
		;;   2. https://emacs-china.org/t/emacs/22193/6
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
  :hook ((text-mode         . olivetti-mode)
          (prog-mode         . olivetti-mode)
          (Info-mode         . olivetti-mode)
          (org-mode          . olivetti-mode)
          (markdown-mode     . olivetti-mode))
  :custom
  (olivetti-body-width 0.8))

;;; Cursor ==================================================
;; @ disable cursor blink
(setq blink-cursor-mode nil)


(provide 'init-ui)
