;;; completion.el --- For Completions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Vertico =================================================
(use-package vertico
	:straight (:host github :repo "minad/vertico"
							:files ("*.el" "extensions/*.el"))
	:bind (:map vertico-map
					("C-j" . vertico-next)
					("C-k" . vertico-previous)
					:map minibuffer-local-map
					("M-h" . backward-kill-word)
					("C-w" . backward-kill-word))
  :init
  (vertico-mode)

  (setq vertico-cycle t
	  vertico-resize t
    read-file-name-completion-ignore-case t
    read-buffer-completion-ignore-case t
    completion-ignore-case t)

	;; can selete entry with M-<number> <ret>
  ;; (vertico-flat-mode)
  ;; (setq vertico-flat-max-lines 8)
  ;; (vertico-grid-mode)
	(vertico-indexed-mode)
	;; configure display per command
	(vertico-multiform-mode)

  (setq completion-in-region-function
    (lambda (&rest args)
      (apply (if vertico-mode
               #'consult-completion-in-region
               #'completion--in-region)
        args)))

  (defun mk/create-vertico-multiform-commands (commands common-properties)
    (let ((result '()))
      (dolist (cmd commands)
        (setq result (append result (list (cons cmd common-properties)))))
      result))

  (setq vertico-multiform-commands
    (append
      '()
      (mk/create-vertico-multiform-commands
        '(mk/better-consult-ripgrep mk/better-consult-git-grep mk/better-consult-line consult-line consult-outline consult-ripgrep consult-imenu consult-imenu-multi xref-find-references consult-info)
        '(buffer
           (vertico-buffer-display-action . (display-buffer-in-side-window
                                              (side . right)
                                              (window-width . 0.5)))
           (:not grid))))))

;; Configure directory extension.
;; TODO don't know the actual working scenario
(use-package vertico-directory
  :straight nil
  :ensure nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("C-w" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; @ vertico recommended defualt configuration
(use-package emacs
	:ensure nil
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
            (replace-regexp-in-string
              "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
              crm-separator)
            (car args))
      (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq read-extended-command-predicate
    #'command-completion-default-include-p)
  (setq enable-recursive-minibuffers t))


;; @ posframe
;; vertico-posframe depends on posframe(thus it is auto-installed)
;; (use-package vertico-posframe
;; 	:after vertico
;; 	:config
;; 	;; (vertico-posframe-mode)
;; 	(setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center
;;     vertico-posframe-font "BlexMono Nerd Font 12") ;; smaller font size to show more content(often with long file path :(  )
;; 	(setq vertico-posframe-parameters
;;     '((left-fringe . 8)
;;        (right-fringe . 8))))

;;; Annotations in completion =============================
(use-package marginalia
  :init
  (marginalia-mode))


;;; Orderless completion ====================================
(use-package orderless
  :init
  (setq completion-styles '(orderless basic) ;; orderless, and basic as fallback
    completion-category-defaults nil
    completion-category-overrides '((file (styles basic partial-completion)))))

;;; Embark ==================================================
(use-package embark
  :bind (("C-." . embark-act))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
	;; Show Embark actions via which-key
  (setq embark-action-indicator
    (lambda (map)
      (which-key--show-keymap "Embark" map nil nil 'no-paging)
      #'which-key--hide-popup-ignore-command)
    embark-become-indicator embark-action-indicator)

  (add-to-list 'display-buffer-alist
    '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
       nil
       (window-parameters (mode-line-format . none)))))

;; @ Interact at Consult 
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Consult =================================================
(use-package consult
  :custom
  (consult-imenu-config '((java-ts-mode :toplevel "Method" :types
                            ((?m "Method" font-lock-function-name-face)
                              (?c "Class" font-lock-type-face)
                              (?i "Interface" font-lock-type-face)))
                           (emacs-lisp-mode :toplevel "Functions" :types
									           ((102 "Functions" font-lock-function-name-face)
									             (109 "Macros" font-lock-function-name-face)
									             (112 "Packages" font-lock-constant-face)
									             (116 "Types" font-lock-type-face)
									             (118 "Variables" font-lock-variable-name-face)))))
  :config
  ;; integrated with xref
  (setq xref-show-xrefs-function #'consult-xref)
  (consult-customize consult-recent-file :preview-key nil)) ;; disable preview for recent file  

;; NOTE: disable these to using lsp-bridge
;;; Corfu: In Region Completion  ============================
;; interacted with orderless (use M-SPC(M: Alt) to insert seperator)
;; use vertico completion instead(since I don't use completion often)
;; (use-package corfu
;; 	:straight (:host github :repo "minad/corfu"
;; 							:files ("*.el" "extensions/*.el"))
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto nil)               ;; Disable auto completion
;;   (corfu-on-exact-match 'quit) 
;; 	(corfu-auto-delay 0)           ;; Enable auto completion
;; 	(corfu-auto-prefix 2)          ;; Enable auto completion
;;   :init
;;   (global-corfu-mode)
;; 	;; remembers selected candidates and sorts the candidates
;; 	(corfu-history-mode)
;; 	;; quick select, M-<number> <ret>
;; 	(corfu-indexed-mode)
;; 	;; popup info
;; 	(corfu-popupinfo-mode))

;; @ corfu recommended defualt configuration
(use-package emacs
	:ensure nil
  :init
  (setq completion-cycle-threshold 3)
  (setq read-extended-command-predicate
    #'command-completion-default-include-p)
  (setq tab-always-indent 'complete))

;; @ enable corfu in terminal emacs
;; (use-package corfu-terminal
;; 	:straight (:type git
;; 							:repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
;; 	:config
;; 	(unless (display-graphic-p)
;; 		(corfu-terminal-mode +1)))

;; Cafe ====================================================
;; add completion etension
;; FIXME it seems like this package doesn't work well with citre (may be citre's problem)
(use-package cape
	:hook ((prog-mode . mk/setup-cape))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (defun mk/setup-cape()
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    ))

;;; Snippet =================================================
;; @ Tempel
(use-package tempel
	:custom
	(tempel-path (expand-file-name "templates/*.eld" user-emacs-directory))
	:hook ((prog-mode . tempel-setup-capf)
				  (text-mode . tempel-setup-capf))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; NOTE: We add	`tempel-expand' *before* the main programming mode Capf,
		;; such	that it will be tried first.
    (setq-local completion-at-point-functions
      (cons #'tempel-expand
        completion-at-point-functions)))
  (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  (add-hook 'text-mode-hook #'tempel-abbrev-mode))

(use-package tempel-collection
  :after tempel)

;;; Custom Cape Functions ===================================
(defun mk/get-lines-from-previous-buffer ()
  "Collect all visible lines from the previously visited buffer and store them in a list."
  (interactive)
  (let ((previous-buffer (other-buffer))
         (lines))
    (with-current-buffer previous-buffer
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (setq lines (cons (buffer-substring (line-beginning-position) (line-end-position)) lines))
          (forward-line 1))))
    (setq lines (nreverse lines))
    lines))

(defun mk/cape-line-previous-buffer (&optional interactive)
  "Complete current line from other lines.
The buffers returned by `cape-line-buffer-function' are scanned for lines.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
    (cape-interactive #'mk/cape-line-previous-buffer)
    `(,(pos-bol) ,(point)
       ,(cape--table-with-properties (mk/get-lines-from-previous-buffer) :sort nil)
       ,@cape--line-properties)))

(provide 'completion)
