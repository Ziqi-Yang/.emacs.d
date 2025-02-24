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

(use-package apropospriate-theme)

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
      (load-theme 'modus-operandi-tinted t)
    (load-theme 'modus-vivendi t)))

(add-hook 'after-init-hook #'mk/setup-theme)
(add-hook 'server-after-make-frame-hook #'mk/setup-theme)

(use-package delight)

(defun mk/setup-font-lock()
  "Set minimum font lock level for both treesit and font-lock"
  ;; (setq treesit-font-lock-level 1) ;; treesit NOTE change back to 1
  (setq font-lock-maximum-decoration 3) ;; font lock
  ;; setup jit-lock
  (setq jit-lock-chunk-size 1500
	      jit-lock-defer-time 0
	      jit-lock-stealth-time 1.5
        jit-lock-stealth-nice 0.2))

(add-hook 'after-init-hook #'mk/setup-font-lock())

;; icon theme

(use-package nerd-icons)

;;; 80 column indicator =====================================
(global-display-fill-column-indicator-mode 1)
;; need to explicit set value to 80 (due to olivetti mode?)
(setq-default display-fill-column-indicator-column 80)


;;; font settings ===========================================
;; font faces only works in emacs GUI, terminal emcas should change terminal font instead
(defun mk/setup-font-faces ()
  "Setup Fonts."
  (let ((default-font "Aporetic Serif Mono")  ; Aporetic Serif Mono, Iosevka, IBM Plex Mono, Cascadia Code
        (CJK-font "LXGW Neo XiHei")
        (font-size 20))
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
          ;; don't set font size, since it will prevent size change when using
          ;; commands like `global-text-scale-adjust'
	        (set-fontset-font t 'han (font-spec :family CJK-font)))
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

(add-hook 'prog-mode-hook 'which-function-mode)

;; note that there is also tab line (`tab-line-format')

(defun mk/setup-header-line()
  (setq-default
   header-line-format
   '(
     ;; (:eval eldoc-headline-string)
     ;; (:propertize " # " face error)
     ;; (:eval (breadcrumb-imenu-crumbs))
     ;; (:propertize " # " face error)
     ;; "GC: " (:eval (number-to-string gcs-done)) " - " (:eval (number-to-string gc-elapsed)) "s"
     )))

(with-eval-after-load 'emacs
  (mk/setup-modeline)
  ;; (mk/setup-header-line)
  )

(use-package window
  :ensure nil
  :custom
  (split-window-preferred-direction 'longest))

;;; Display Buffer Alist ========================================================
(setq display-buffer-alist
      '(("\\*Embark Collect" (display-buffer-in-side-window) (side . bottom)
         (height . 0.4))
        ("\\*Typst-Watch\\*" display-buffer-at-bottom (window-height . fit-window-to-buffer))
        ("\\*Embark Actions\\*" (display-buffer-in-tab) (side . right)
         (width . 0.3))
        ("\\*SQLite.*?\\*" (display-buffer-same-window) (reusable-frames))
        ("\\*vc-git.*?\\*" (display-buffer-in-side-window) (side . top))
        ((derived-mode . eat-mode) (display-buffer-in-side-window)
         (side . right) (window-width . 0.5))
        ((derived-mode . compilation-mode) (display-buffer-in-side-window)
         (side . right) (window-width . 0.5))
        ("^\\*ielm\\*"
         display-buffer-in-side-window (side . bottom))
        ("^\\*Flymake diagnostics for.*?\\*"
         display-buffer-in-side-window (side . bottom))))


;;; Misc =======================================================================
(with-eval-after-load 'image-mode
  (customize-set-variable 'image-auto-resize 'fit-window)
  (customize-set-variable 'image-auto-resize-on-window-resize 0))

(provide 'init-ui)

;;; init-ui.el ends here
