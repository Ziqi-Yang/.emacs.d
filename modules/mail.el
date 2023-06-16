;;; mail.el --- Email Configuartion -*- lexical-binding: t -*-
;;; Commentary:
;; I have difficulty using email itself to send email by Gmail SMTP server (whether using proxy setting via `proxy-http-toggle' command or proxcychains external command-line utility).
;; set firefox as default mail client:
;;   xdg-settings set default-url-scheme-handler mailto firefox.desktop
;; then set gmail website as default mailto associated application in firefox in 'setting'
;;; Code:

(defun mk/setup-mail()
  "Setup Email Configuration."
  (setq message-send-mail-function 'message-send-mail-with-mailclient))

(add-hook 'after-init-hook #'mk/setup-mail)

(provide 'mail)
