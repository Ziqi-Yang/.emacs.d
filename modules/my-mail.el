;;; my-mail.el --- Email Configuartion -*- lexical-binding: t -*-
;;; Commentary:
;; msmtp settings: ~/.msmtprc
;; offlineimap settings: ~/.config/offlineimap
;; I have difficulty using email itself to send email by Gmail SMTP server (whether using proxy setting via `proxy-http-toggle' command or proxcychains external command-line utility).
;; set firefox as default mail client:
;;   xdg-settings set default-url-scheme-handler mailto firefox.desktop
;; then set gmail website as default mailto associated application in firefox in 'setting'
;;; Code:

(defun mk/setup-mail()
  "Setup Email Configuration."
  ;; (setq message-send-mail-function 'message-send-mail-with-mailclient)
  (setq send-mail-function 'sendmail-send-it
        sendmail-program "msmtp"
        ;; mail-specify-envelope-from t
        ;; message-sendmail-envelope-from 'header
        message-auto-save-directory "~/mail_save"
        message-default-mail-headers "Cc: \nBcc: \n")
  (add-hook 'message-send-hook 'others/sign-or-encrypt-message))

(add-hook 'after-init-hook #'mk/setup-mail)

;; https://www.djcbsoftware.nl/code/mu/mu4e/Getting-mail.html
;; External Package (installed by system package manager - 'mu')
;; after updating mu, you may need to manually initailize again if you encounter an error
;; do 'mu init --my-address mr.meowking@anche.no -m ~/Maildir/'
(use-package mu4e
  :disabled t
  :ensure nil
  :config
  (setq mu4e-mu-binary (executable-find "mu")
        mu4e-confirm-quit nil
        mu4e-maildir-list '("~/Maildir")
        mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 300
        ;; mu4e-sent-messages-behavior 'delete ;; It seems like the imap server for 'mr.meowking@anche.no' won't backup mails sent by clients. So the sent mail should be stored in our local machine.
        mu4e-user-mail-address-list '("mr.meowking@anche.no")
        mu4e-use-fancy-chars t)
  
  (setq mu4e-sent-folder "/mr.meowking@anche.no/Sent"
        mu4e-drafts-folder "/mr.meowking@anche.no/Drafts"
        mu4e-trash-folder "/mr.meowking@anche.no/Trash")

  (setq mu4e-maildir-shortcuts
        '(("/mr.meowking@anche.no/INBOX" . ?i)))

  (keymap-set mu4e-main-mode-map "J" #'mu4e-search-maildir)
  (keymap-set mu4e-view-mode-map "M-r" #'mu4e-compose-reply))

(defun others/sign-or-encrypt-message ()
  "Use it with `message-send-hook'."
  (let ((answer (read-from-minibuffer "Sign or encrypt?\nEmpty to do nothing.\n[s/e]: ")))
    (cond
     ((string-equal answer "s") (progn
                                  (message "Signing message.")
                                  (mml-secure-message-sign-pgpmime)))
     ((string-equal answer "e") (progn
                                  (message "Encrypt and signing message.")
                                  (mml-secure-message-encrypt-pgpmime)))
     (t (progn
          (message "Dont signing or encrypting message.")
          nil)))))

(provide 'my-mail)

;;; my-mail.el ends here
