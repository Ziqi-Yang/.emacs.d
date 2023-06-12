;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;; Commentary:
;;; Code:

;; straight.el
;; (setq straight-repository-branch "develop")
(define-obsolete-variable-alias
  'native-comp-deferred-compilation-deny-list
  'native-comp-jit-compilation-deny-list
  "Renamed in emacs#95692f6")

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

;; (use-package benchmark-init ;; when needed, enable it
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(setq user-full-name "Ziqi Yang"
  user-mail-address "mr.ziqiyang@gmail.com"
  default-directory (expand-file-name "~/.emacs.d"))

;; add load path
;; adding "~/.npm-global/bin/" cause vls for vue-mode error, maybe the system vls and
;; vls here are different
;; (add-to-list (expand-file-name "~/.npm-global/bin/") exec-path)

(push (expand-file-name "lisp" user-emacs-directory) load-path)
(push (expand-file-name "modules" user-emacs-directory) load-path)
(push (expand-file-name "modules/languages" user-emacs-directory) load-path)

(with-temp-message ""
  (require 'init-base)
  (require 'meow)
  (require 'init-key)
  ;; (require 'evil) ;; don't enable this module when enabling meow and init-key
  (require 'init-ui)
  (require 'editor)
  (require 'completion)
  (require 'file-browser)
  (require 'git)
  (require 'init-proxy)
  (require 'auto-insert)
  (require 'ai)
  (require 'info) ;; FIXME this file cannot be loaded
  (require 'emacs-developer) ;; and If I comment this line, l-general file cannot be loaded
  (require 'hugo))

;; load language
(with-temp-message ""
  (require 'l-markdown)
  (require 'l-org)
  (require 'l-web)
  (require 'l-lisp)
  (require 'l-rust)
  (require 'l-cc)
  (require 'l-java)
  (require 'l-zig)
  (require 'l-latex)
  (require 'l-shell)
  (require 'l-draw)
  (require 'l-typst)
  (require 'l-general)) ;; l-general must loaded after l-rust

;; remove old version native-compiled files
(native-compile-prune-cache) 

(provide 'init)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
