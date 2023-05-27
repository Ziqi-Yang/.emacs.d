;;; early-init.el --- Emacs pre-initialization config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-default-coding-systems 'utf-8)

;;; Disable package.el ======================================
(setq package-enable-at-startup nil ;; disable package.el at startup
  package-quickstart nil) ;; don't load from package cache

;;; Performance =============================================
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;; NOTE: This GCMH minimizes GC interference with the activity by using a high GC
;; threshold during normal use, then when Emacs is idling, GC is triggered and a
;; low threshold is set. We set the threshold (`gc-cons-threshold'
;; variable) to an unlimited size in "early-init.el", this helps improving the
;; startup time, but needs to be set down to a more reasonable value after Emacs
;; gets loaded. The use of `gcmh-mode' ensures reverting this value so we don't
;; need to do it manually.
(setq gc-cons-threshold most-positive-fixnum
  ;; gc-cons-percentage 0.5
  read-process-output-max (* 10 1024 1024))

;; ;; @ time ;; discord, since dashboard owns the same function
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "*** Emacs loaded in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))


;; Change eln-cache folder place ============================
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
    (convert-standard-filename
	    (expand-file-name  ".local/eln-cache/" user-emacs-directory))))

;;; UI settings =============================================
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 3) ;; diff-hl make use of fringe

;; Emacs 29 alpha background
(push (cons 'alpha-background 92) default-frame-alist)

(column-number-mode)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(setq
  inhibit-startup-message t
  visible-bell t)

;; Suppress warnings and errors during asynchronous native compilation
(setq native-comp-async-report-warnings-errors nil)

(provide 'early-init)
