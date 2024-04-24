;;; my-eglot.el --- eglot configuration -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Meow King <mr.meowking@anche.no>

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Eglot  ===================

;; check eglot-server-programs to know the language programs that corresponding
;; to a certain language.
;; 
;; Note that a language server can also execute other *optional package command*
;; like bash-language-server can execute shellcheck, you should check the optional
;; package required by the main pakcage is also installed(pacman -Qi to view)

;; manually do `eglot' for every workspace is easy, so we dont' use `eglot-ensure'

;;; Code:

;;; Eglot ======================================================================

;; need https://aur.archlinux.org/packages/emacs-lsp-booster-git
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
	:after eglot
	:config	(eglot-booster-mode))

(defun mk/add-eglot-ensure (hook-list)
	(dolist (mode hook-list)
		(add-hook mode #'eglot-ensure)))

(with-eval-after-load 'eglot
	;; NOTE
	;; install markdown-mode to rich the doc
  ;; performance improvemence: https://www.reddit.com/r/emacs/comments/16vixg6/how_to_make_lsp_and_eglot_way_faster_like_neovim/
  (fset #'jsonrpc--log-event #'ignore) ;; remove laggy typing it probably reduces chatty json from lsp to eglot i guess
	(setq-default eglot-events-buffer-size 0) ;; disable log, improve performance
  ;; list of things that eglot won't change
	;; (customize-set-variable 'eglot-stay-out-of '(imenu))
  (customize-set-variable 'eglot-extend-to-xref t)
	(customize-set-variable 'eglot-autoshutdown t) ;; automatically shutdown
  (add-hook 'eglot-managed-mode-hook
            (lambda () (eglot-inlay-hints-mode -1)))
  (setq-default eglot-send-changes-idle-time 0.25)
  ;; see outer files(like header files) as in project temporarily

  ;; NOTE: pyright is better for handling virtual environment with a configuration flie per project
  ;; how to configure eglot-workspace-configuration:
  ;; https://paste.sr.ht/~meow_king/df83c4dd8541e54befe511ddaf0eeee7cb59eaba
  (setq-default eglot-workspace-configuration
                ;; install python-lsp-server and python-lsp-ruff
                ;; see https://github.com/python-lsp/python-lsp-server
                ;; and https://github.com/charliermarsh/ruff
                '((:pylsp . (:plugins (:ruff (:enabled t)
                                             ;; :rope_autoimport doens't work ...
                                             )))))

  (add-hook 'eglot-managed-mode-hook #'mk/setup-eglot-eldoc))

(defun mk/setup-eglot-eldoc ()
  "Set the eldoc documentation functions to be the following.
1. flymake-eldoc-function (ensure we can see error in echo line when hover)
2. eglot-signature-eldoc-function
3. eglot-hover-eldoc-function"
  (setq eldoc-documentation-functions
        (cons #'eglot-signature-eldoc-function
	            (remove #'eglot-signature-eldoc-function eldoc-documentation-functions))))


(use-package eglot-hierarchy
  :ensure (:host github :repo "dolmens/eglot-hierarchy"))

;;; @ lsp-bridge ============================================
(use-package yasnippet
  :delight yas-minor-mode
  :config
  (yas-global-mode 1))

;; global (only enable disable hooks)
;; activate: (global-lsp-bridge-mode)
;; de-activate: mk/disbale-global-lsp-bridge-mode
;; single buffer
;; lsp-bridge-mode
(use-package lsp-bridge
  :ensure '(lsp-bridge
            :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not elpaca--byte-compile))
  :custom
  (lsp-bridge-code-action-enable-popup-menu nil)  ; FIXME quit popup menu will cause weird problem
  (lsp-bridge-enable-hover-diagnostic t)
  (lsp-bridge-complete-manually t)
  (acm-candidate-match-function 'orderless-regexp)
  (lsp-bridge-python-lsp-server "ruff")
  ;; pyright is good at handling virtual environment with a configuration file
  ;; see https://microsoft.github.io/pyright/#/configuration
  ;; pyrightconfig.json
  ;; {
  ;; "venvPath": ".",
  ;; "venv": ".venv"
  ;; }
  (lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (lsp-bridge-multi-lang-server-mode-list
   '(((python-mode python-ts-mode) . lsp-bridge-python-multi-lsp-server)
     ((web-mode) . "html_emmet_tailwindcss")
     ((vue-mode) . "volar_emmet")
     ;; ((typescript-ts-mode) . "typescript_eslint") ;; eslint part seems does nothing at all?
     ((qml-mode qml-ts-mode) . "qmlls_javascript")))
  ;; use my `eldoc-headline' to display signature information
  (lsp-bridge-signature-show-function '(lambda (str) (setq-local eldoc-headline-string str)))
  (lsp-bridge-user-langserver-dir (expand-file-name "lsp-bridge-config/langserver" user-emacs-directory))
  (lsp-bridge-user-multiserver-dir (expand-file-name "lsp-bridge-config/multiserver" user-emacs-directory))
  :config
  ;; lsp-bridge doesn't work well on *scratch* buffer
  (dolist (hook '(lisp-interaction-mode-hook emacs-lisp-mode-hook))
    (setq lsp-bridge-default-mode-hooks (remove hook lsp-bridge-default-mode-hooks)))
  
  (setq lsp-bridge-enable-with-tramp nil)  ; goto local sudo bookmark will cause error

  (add-hook 'web-mode-hook (lambda () (setq-local lsp-bridge-enable-completion-in-string t)))
  (add-hook 'vue-mode-hook (lambda () (setq-local lsp-bridge-enable-completion-in-string t))))

(defun mk/disbale-global-lsp-bridge-mode ()
  (interactive)
  (dolist (hook lsp-bridge-default-mode-hooks)
    (remove-hook hook (lambda ()
                        (when (and (lsp-bridge--not-mind-wave-chat-buffer)
                                   (lsp-bridge--not-acm-doc-markdown-buffer))
                          (lsp-bridge-mode 1)))))
  (dolist (buf (buffer-list))
    (when lsp-bridge-mode
      (lsp-bridge-mode -1))))

;;; citre ===================================================

;; use ggtags instead? https://github.com/yoshizow/global-pygments-plugin.git
;; hacks for ggtags: https://github.com/lynnux/.emacs.d/blob/a4fb0a6cf6abe9f62f3cbadf4d77a11d9ff09a13/settings/package_extra.el#L5801

(use-package citre
  :init
  (require 'citre-config)
  :config
  ;; use `citre-mode' to manually enable citre capf-integration
  ;; citre can do jump without enabling `citre-mode'
  (remove-hook 'find-file-hook #'citre-auto-enable-citre-mode)
  (setq-default
   citre-enable-capf-integration t  ; completion-at-point integration
   citre-enable-imenu-integration nil
   citre-enable-xref-integration nil)
  (setq
   citre-default-create-tags-file-location 'global-cache
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ;; citre-capf-substr-completion t
   ;; for my custom MarkdownTAG
   ;; citre-auto-enable-citre-mode-modes '(prog-mode markdown-mode))
   ;; (setq evil-lookup-func #'citre-peek) ;; mapping key "K"
   ))

(defun mk/update-all-tags()
  "Update both ctags and gtags file (for citre)."
  (interactive)
  (citre-update-this-tags-file)
  (citre-global-update-database))

;;; Other ======================================================================

;; @ eldoc
(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-minor-mode-string nil)
  (setq eldoc-echo-area-use-multiline-p nil))

(defun mk/setup-flymake-eldoc ()
  "Better setting for displaying flymake diagnostics in eldoc documentation."
  (setq eldoc-documentation-functions
	      (cons #'flymake-eldoc-function
	            (remove #'flymake-eldoc-function eldoc-documentation-functions)))
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose))

(with-eval-after-load 'flymake
  (add-hook 'emacs-lisp-mode-hook #'flymake-mode)
  (add-hook 'flymake-mode-hook #'mk/setup-flymake-eldoc))

(use-package dape
  ;; Currently only on github
  :ensure (dape :type git :host github :repo "svaante/dape"))

;; format file
(use-package apheleia)

;;; Custom glue functions ======================================================

(defun mk/code/find-definition ()
  (interactive)
  (if lsp-bridge-mode
      (lsp-bridge-find-def)
    (let ((this-command 'xref-find-definitions))
      (call-interactively #'xref-find-definitions))))

(defun mk/code/find-definition-other-window ()
  (interactive)
  (if lsp-bridge-mode
      (lsp-bridge-find-def-other-window)
    (let ((this-command 'xref-find-definitions-other-window))
      (call-interactively #'xref-find-definitions-other-window))))

(defun mk/code/find-references ()
  (interactive)
  (if lsp-bridge-mode
      (lsp-bridge-find-references)
    (let ((this-command 'xref-find-references))
      (call-interactively #'xref-find-references))))

(defun mk/code/find-implementation ()
  (interactive)
  (if lsp-bridge-mode
      (lsp-bridge-find-impl)
    (call-interactively #'eglot-find-implementation)))

(defun mk/code/toggle-inlay-hint ()
  (interactive)
  (if lsp-bridge-mode
      (progn
        (setq-local lsp-bridge-enable-inlay-hint (not lsp-bridge-enable-inlay-hint))
        (if lsp-bridge-enable-inlay-hint
            (lsp-bridge-inlay-hint)
          (lsp-bridge-inlay-hint-hide-overlays)))
    (call-interactively #'eglot-inlay-hints-mode)))

(defun mk/code/error-list (&optional arg)
  (interactive "P")
  (if arg
      (call-interactively #'flymake-show-buffer-diagnostics)
    (if lsp-bridge-mode
        (lsp-bridge-diagnostic-list)
      (call-interactively #'flymake-show-buffer-diagnostics))))

(defun mk/code/action ()
  (interactive)
  (if lsp-bridge-mode
      (lsp-bridge-code-action)
    (call-interactively #'eglot-code-actions)))

(defun mk/code/documentation()
  (interactive)
  (if lsp-bridge-mode
      (call-interactively #'lsp-bridge-show-documentation)
    (call-interactively #'eldoc)))

(provide 'my-lsp)

;;; my-lsp.el ends here
