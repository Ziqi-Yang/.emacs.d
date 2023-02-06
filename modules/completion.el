;;; completion.el --- For Completions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Vertico =============================
(use-package vertico
	:straight (:host github :repo "minad/vertico"
									 :files ("*.el" "extensions/*.el"))
	:bind (:map vertico-map
							("C-j" . vertico-next)
							("C-k" . vertico-previous)
							:map minibuffer-local-map
							("M-h" . backword-kill-word)
							("C-w" . backword-kill-word))
  :init
  (vertico-mode)
  (setq vertico-count 20)
  (setq vertico-cycle t))

;; vertico recommended defualt configuration
(use-package savehist
  :init
  (savehist-mode))

;; vertico recommended defualt configuration
(use-package emacs
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

(provide 'completion)
