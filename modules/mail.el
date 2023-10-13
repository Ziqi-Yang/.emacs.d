;;; mail.el --- Email Configuartion -*- lexical-binding: t -*-
;;; Commentary:
;; I have difficulty using email itself to send email by Gmail SMTP server (whether using proxy setting via `proxy-http-toggle' command or proxcychains external command-line utility).
;; set firefox as default mail client:
;;   xdg-settings set default-url-scheme-handler mailto firefox.desktop
;; then set gmail website as default mailto associated application in firefox in 'setting'
;;; Code:

(defun mk/setup-mail()
  "Setup Email Configuration."
  ;; (setq message-send-mail-function 'message-send-mail-with-mailclient)
  (setq send-mail-function 'sendmail-send-it
    sendmail-program "/usr/bin/msmtp"
    ;; mail-specify-envelope-from t
    ;; message-sendmail-envelope-from 'header
    message-auto-save-directory "~/mail_save"
    message-default-mail-headers "Cc: \nBcc: \n"))

(add-hook 'after-init-hook #'mk/setup-mail)

(use-package mu4e
  ;; installing 'mu' though 'pacman -S mu' will automatically add package 'mu4e' into emacs site-package
  :elpaca nil
  :config
  (setq mu4e-mu-binary (executable-find "mu")
    mu4e-confirm-quit nil
    mu4e-maildir-list '("~/mail")
    mu4e-get-mail-command "offlineimap"
    mu4e-update-interval 300
    ;; mu4e-sent-messages-behavior 'delete ;; It seems like the imap server for 'mr.meowking@anche.no' won't backup mails sent by clients. So the sent mail should be stored in our local machine.
    mu4e-user-mail-address-list '("mr.meowking@anche.no")
    mu4e-use-fancy-chars t)
  
  (setq mu4e-sent-folder "/autistici/Sent"
    mu4e-drafts-folder "/autistici/Drafts"
    mu4e-trash-folder "/autistici/Trash")

  (setq mu4e-maildir-shortcuts
    '(("/autistici/INBOX" . ?i))))

(defun mk/mu4e-main-local-keybinding-setup()
  (keymap-local-set "J" #'mu4e-search-maildir))

(defun mk/mu4e-view-local-keybinding-setup()
  ;; SPC-m r
  (keymap-local-set "M-r" #'mu4e-compose-reply))

(add-hook 'mu4e-main-mode-hook 'mk/mu4e-main-local-keybinding-setup)
(add-hook 'mu4e-view-mode-hook 'mk/mu4e-view-local-keybinding-setup)

(provide 'mail)
