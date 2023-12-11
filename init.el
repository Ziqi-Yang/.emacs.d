;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; (setq debug-on-error t)

(defvar elpaca-installer-version 0.6)
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

(add-to-list 'exec-path (expand-file-name "~/myBin/"))
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))
;; for compile to work
(setenv "PATH" (concat (format "%s:%s:" (expand-file-name "~/myBin/") (expand-file-name "~/.local/bin")) (getenv "PATH")))

(push (expand-file-name "lisp" user-emacs-directory) load-path)
(push (expand-file-name "modules" user-emacs-directory) load-path)
(push (expand-file-name "modules/languages" user-emacs-directory) load-path)

;; modern looking
;; https://emacsconf.org/2023/talks/flat/
;; with a little modifications
(defun flat-style(theme &rest args)
  (custom-set-faces
    '(header-line
       ((t (:inherit mode-line
             :box (:style flat-button)))) t)
    '(mode-line
       ((t (:inherit mode-line
             :box (:style flat-button)))) t)
    '(mode-line-inactive
       ((t (:inherit mode-line-inactive
             :box (:style flat-button)))) t)))
(advice-add 'load-theme :after #'flat-style)

;; NOTE: module name should be unique(also to the built-in module)
(with-temp-message ""
  (require 'init-base)
  (require 'meow-keybindings)
  (require 'init-key)
  (require 'init-ui)
  (require 'editor)
  (require 'completion)
  (require 'custom-consult-collection)
  (require 'file-browser)
  (require 'my-vc)
  (require 'my-minibuffer)
  (require 'init-proxy)
  (require 'mail)
  (require 'auto-insert)
  ;; (require 'ai)
  (require 'browser)
  (require 'info-config)
  (require 'my-debug)
  (require 'adbkeyboard)
  (require 'emacs-developer)
  (require 'hugo)
  (require 'my-advice-adds)
  (require 'emacs30))

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
  (require 'l-crystal)
  (require 'l-general)) ;; l-general must loaded after l-rust

;; remove old version native-compiled files in the end
(use-package comp
  :elpaca nil
  :config
  (native-compile-prune-cache))


;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

(setq find-file-visit-truename t)

;; disable functions
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(provide 'init)

;;; init.el ends here
