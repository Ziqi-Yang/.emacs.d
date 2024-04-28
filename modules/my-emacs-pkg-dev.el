;;; my-emacs-pkg-dev.el --- Dev Libraries -*- lexical-binding: t -*-
;;; Commentary:

;; other use libraries for use in ur packages
;; (use-package compat)

;;; Code:

(use-package package-lint)

;;; tree-sitter
(use-package ts-query-highlight
  :ensure (:type git :host sourcehut :repo "meow_king/ts-query-highlight")
  :config
  (setq ts-query-highlight-dabbrev-expand-function 'cape-dabbrev))

(defun others/eval-buffer ()
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

(defun others/byte-compile-and-load-directory (directory)
  "Byte-compile and load all elisp files in DIRECTORY.
Interactively, directory defaults to `default-directory' and asks
for confirmation."
  (interactive (list default-directory))
  (let* ((load-path (cons directory load-path))
         (files (directory-files directory 't (rx ".el" eos))))
    (dolist (file files)
      (byte-compile-file file 'load))))

(provide 'my-emacs-pkg-dev)

;;; my-emacs-pkg-dev.el ends here
