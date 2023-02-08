;;; init-ui.el --- UI settings -*- lexical-binding: t -*-
;;; Commentary:
;; primiary settings are in the early-init.el file, while extended setting like theme and font are here
;;; Code:

;;; theme ===================================================
;; @ editor theme
(use-package doom-themes
  :config
  (load-theme 'doom-solarized-light t))

;; @ icon theme
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (when (not (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t)))

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
  :init (doom-modeline-mode 1))

;;; Navigation Highlight ====================================
(use-package beacon
	:defer 1
	:hook ((after-init . (lambda () (beacon-mode 1)))))


;;; Center Area =============================================
(use-package olivetti
  :hook ((text-mode         . olivetti-mode)
         (prog-mode         . olivetti-mode)
         (Info-mode         . olivetti-mode)
         (org-mode          . olivetti-mode)
         (markdown-mode     . olivetti-mode))
  :custom
  (olivetti-body-width 0.8))

(provide 'init-ui)
