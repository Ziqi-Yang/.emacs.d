;;; early-init.el --- Emacs pre-initialization config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-default-coding-systems 'utf-8)

;;; Disable package.el ======================================
;; (setq package-enable-at-startup nil ;; disable package.el at startup
;;   package-quickstart nil) ;; don't load from package cache

;;; Performance =============================================
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;; NOTE: This GCMH minimizes GC interference with the activity by using a high GC
;; threshold during normal use, then when Emacs is idling, GC is triggered and a
;; low threshold is set. We set the threshold (`gc-cons-threshold'
;; variable) to an unlimited size in "early-init.el", this helps improving the
;; startup time, but needs to be set down to a more reasonable value after Emacs
;; gets loaded. The use of `gcmh-mode' ensures reverting this value so we don't
;; need to do it manually. -- I changed this configuration, see my comments around
;; my setup of gcmh
(setq
 ;; set a high value before initialization, and it should be reduced to a
 ;; proper value after init
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.3
 read-process-output-max (* 10 1024 1024))

(defun mk/setup-gc()
  (setq
   gc-cons-threshold (* 100 1024 1024)
   gc-cons-percentage 0.3
   read-process-output-max (* 10 1024 1024)
   
   ;; Don’t compact font caches during GC.
   inhibit-compacting-font-caches t))
(add-hook 'after-init-hook #'mk/setup-gc)

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
(setq default-frame-alist '((fullscreen . maxmized) (alpha-background . 92)
                             (vertical-scroll-bars)))
(setq initial-frame-alist default-frame-alist)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; set fringe width
(set-fringe-mode 6) ;; diff-hl make use of fringe


(tooltip-mode 1)
(column-number-mode 1)
;; (global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'absolute)

(setq
  inhibit-startup-message t
  visible-bell t)

;; Suppress warnings and errors during asynchronous native compilation
(setq native-comp-async-report-warnings-errors nil)

;;; Misc =======================================================================
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


(provide 'early-init)
;;; early-init.el ends here
