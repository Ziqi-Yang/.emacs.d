;;; completion.el --- For Completions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Vertico =================================================
(use-package vertico
	:straight (:host github :repo "minad/vertico"
									 :files ("*.el" "extensions/vertico-indexed.el"))
	:bind (:map vertico-map
				 ("C-j" . vertico-next)
				 ("C-k" . vertico-previous)
				 :map minibuffer-local-map
				 ("M-h" . backward-kill-word)
				 ("C-w" . backward-kill-word))
  :init
  (vertico-mode)
  (setq vertico-cycle t)
	(setq vertico-resize t)

	;; can selete entry with M-<number> <ret>
	(vertico-indexed-mode))

;; @ save minibuffer history
;; such that vertico can make use of
(use-package savehist
  :init
  (savehist-mode 1))

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
(use-package vertico-posframe
	:after vertico
	:config
	(vertico-posframe-mode)
	(setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)
	(setq vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8))))

;;; Annotations in completetion =============================
(use-package marginalia
  :init
  (marginalia-mode))


;;; Orderless completion ====================================
(use-package orderless
  :init
  (setq completion-styles '(orderless basic) ;; orderless, and basic as fallback
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;;; Embark ==================================================
(use-package embark
  :bind (("C-." . embark-act))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
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
	:config
	;; integrated with xref
	(setq xref-show-xrefs-function #'consult-xref))

;;; Corfu: In Region Completion  ============================
;; interacted with orderless (use M-SPC(M: Alt) to insert seperator)
(use-package corfu
	:straight (:host github :repo "minad/corfu"
									 :files ("*.el" "extensions/*.el"))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
	(corfu-auto-delay 0)           ;; Enable auto completion
	(corfu-auto-prefix 2)          ;; Enable auto completion
	:bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode)

	;; remembers selected candidates and sorts the candidates
	(corfu-history-mode)

	;; quick select, M-<number> <ret>
	(corfu-indexed-mode)

	;; popup info
	(corfu-popupinfo-mode))

;; @ corfu recommended defualt configuration
(use-package emacs
	:ensure nil
  :init
  (setq completion-cycle-threshold 3)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  (setq tab-always-indent 'complete))

(use-package corfu-terminal
	:straight (:type git
									 :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
	:config
	(unless (display-graphic-p)
		(corfu-terminal-mode +1)))




(provide 'completion)
