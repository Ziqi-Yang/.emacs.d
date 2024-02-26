;;; l-eglot.el --- eglot configuration -*- lexical-binding: t; -*-
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

;; check eglot-server-programs to know the language programs that corresponding
;; to a certain language.
;; 
;; Note that a language server can also execute other *optional package command*
;; like bash-language-server can execute shellcheck, you should check the optional
;; package required by the main pakcage is also installed(pacman -Qi to view)

;; manually do `eglot' for every workspace is easy, so we dont' use `eglot-ensure'

;;; Code:

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
                                             ))))))

(use-package eglot-hierarchy
  :ensure (:host github :repo "dolmens/eglot-hierarchy"))

(provide 'l-eglot)

;;; l-eglot.el ends here
