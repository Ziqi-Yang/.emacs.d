;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; (setq debug-on-error t)

;; https://github.com/progfolio/elpaca/wiki/Warnings-and-Errors#unable-to-determine-elpaca-core-date
(defvar elpaca-core-date '(20241107))

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name ".local/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
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
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
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

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; (use-package benchmark-init ;; when needed, enable it
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(push (expand-file-name "init" user-emacs-directory) load-path)
(push (expand-file-name "modules" user-emacs-directory) load-path)
(push (expand-file-name "languages" user-emacs-directory) load-path)
(push (expand-file-name "lib" user-emacs-directory) load-path)

(use-package compat
  :ensure nil)

;; common modules
(with-temp-message ""
  ;; all file under `lib' directory
  (require 'lib-0)

  ;; all files under `init' directory
  (require 'init-vars)
  (require 'init-advice)
  (require 'init-base)
  (require 'init-key)
  (require 'init-meow-keys)
  (require 'init-key-transient)
  (require 'init-ui)
  
  (require 'my-private-configs))

(if (not (display-graphic-p))
    (with-temp-message ""
      (require 'my-advice-adds)
      ;; (require 'my-ai)
      (require 'my-auto-insert)
      (require 'my-completion)
      ;; (require 'my-debug)
      (require 'my-dired)
      (require 'my-edit)
      ;; (require 'my-emacs-pkg-dev)
      ;; (require 'my-lsp)
      ;; (require 'my-mail)
      (require 'my-minibuffer)
      (require 'my-misc)
      ;; (require 'my-note)
      (require 'my-search-replace)
      (require 'my-spellcheck)
      (require 'my-utils)
      ;; (require 'my-vc)


      ;; (require 'l-org)
      (require 'l-web)
      (require 'l-treesit)
      (require 'l-languages)

      ;; Kitty Keyboard protocol support (so I can use Ctrl + Return in Kitty)
      (use-package kkp
        :defer 1
        :config
        (global-kkp-mode +1)))

  (with-temp-message ""
    ;; load all files under `module' directory (require will only load once)
    (require 'my-advice-adds)
    (require 'my-ai)
    (require 'my-auto-insert)
    (require 'my-completion)
    (require 'my-debug)
    (require 'my-dired)
    (require 'my-edit)
    (require 'my-emacs-pkg-dev)
    (require 'my-lsp)
    (require 'my-mail)
    (require 'my-minibuffer)
    (require 'my-misc)
    (require 'my-note)
    (require 'my-search-replace)
    (require 'my-spellcheck)
    (require 'my-utils)
    (require 'my-vc)

    ;; load all files under `language' directory
    (require 'l-org)
    (require 'l-web)
    (require 'l-treesit)
    (require 'l-languages)))

;; remove old version native-compiled files in the end
(use-package comp
  :ensure nil
  :config
  (native-compile-prune-cache))

(setq find-file-visit-truename t)

;; disable functions
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; (add-hook 'after-init-hook #'split-window-horizontally)
;; (add-hook 'server-after-make-frame-hook #'split-window-horizontally)

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

(provide 'init)

;;; init.el ends here
