;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;; Commentary:
;;; Code:

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; straight.el
(setq straight-base-dir (expand-file-name ".local" user-emacs-directory))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name ".local/straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(push (expand-file-name "lisp" user-emacs-directory) load-path)
(push (expand-file-name "modules" user-emacs-directory) load-path)
(push (expand-file-name "modules/languages" user-emacs-directory) load-path)

(with-temp-message ""
  (require 'init-base)
  (require 'init-key)
  (require 'init-ui)
  (require 'editor)
  (require 'completion)
  (require 'file-browser)
  (require 'git)
  (require 'l-general))

;; remove old version native-compiled files
(native-compile-prune-cache) 

(provide 'init)
