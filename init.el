;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;; Commentary:
;;; Code:

;; straight.el
;; (setq straight-repository-branch "develop")

;; (setq debug-on-error t)

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name ".local/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                        :ref nil
                        :files (:defaults (:exclude "extensions"))
                        :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
        (build (expand-file-name "elpaca/" elpaca-builds-directory))
        (order (cdr elpaca-order))
        (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                ((zerop (call-process "git" nil buffer t "clone"
                          (plist-get order :repo) repo)))
                ((zerop (call-process "git" nil buffer t "checkout"
                          (or (plist-get order :ref) "--"))))
                (emacs (concat invocation-directory invocation-name))
                ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                          "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                ((require 'elpaca))
                ((elpaca-generate-autoloads "elpaca" repo)))
        (progn (message "%s" (buffer-string)) (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; (use-package benchmark-init ;; when needed, enable it
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(setq user-full-name "Meow King"
  user-mail-address "mr.meowking@anche.no"
  default-directory (expand-file-name "~/.emacs.d"))

;; add load path
;; adding "~/.npm-global/bin/" cause vls for vue-mode error, maybe the system vls and
;; vls here are different
;; (add-to-list (expand-file-name "~/.npm-global/bin/") exec-path)

(push (expand-file-name "lisp" user-emacs-directory) load-path)
(push (expand-file-name "modules" user-emacs-directory) load-path)
(push (expand-file-name "modules/languages" user-emacs-directory) load-path)

;; NOTE: module name should be unique(also to the built-in module)
(with-temp-message ""
  (require 'init-base)
  (require 'meow-keybindings)
  ;; ;; (require 'evil) ;; don't enable this module when enabling meow and init-key
  (require 'init-key)
  (require 'init-ui)
  (require 'editor)
  (require 'completion)
  (require 'file-browser)
  (require 'git)
  (require 'init-proxy)
  (require 'mail)
  (require 'auto-insert)
  ;; (require 'ai)
  (require 'browser)
  (require 'info-config)
  (require 'my-debug)
  (require 'adbkeyboard)
  (require 'emacs-developer)
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
  (require 'l-makefile)
  (require 'l-python)
  (require 'l-kotlin)
  (require 'l-lua)
  (require 'l-typst)
  (require 'l-general)) ;; l-general must loaded after l-rust

;; remove old version native-compiled files in the end
(use-package comp
  :elpaca nil
  :config
  (native-compile-prune-cache))

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(provide 'init)

;;; init.el ends here
