;;; early-init.el --- Emacs pre-initialization config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-default-coding-systems 'utf-8)

;;; Disable package.el ======================================
;; disable package.el at startup
(setq package-enable-at-startup nil)
;; don't load from package cache
(setq package-quickstart nil)


;;; Performance =============================================
;; The default is ~800kB. Now allocating 200 MB.
(setq gc-cons-threshold (* 200 1024 1024))
;; increase performance (origin 4096 byte)
(setq read-process-output-max (* 3 1024 1024))

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

(column-number-mode)
(global-display-line-numbers-mode 1)

(setq
 inhibit-startup-message t
 visible-bell t)


(provide 'early-init)
