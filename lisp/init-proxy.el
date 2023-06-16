;;; init-proxy.el --- Toogle Proxy -*- lexical-binding: t -*-
;;; Commentary:
;; origin: Centaur Emacs
;; https://github.com/seagle0128/.emacs.d
;;; Code:

(defcustom centaur-proxy "127.0.0.1:7890"
  "Set HTTP/HTTPS proxy."
  :group 'happyo
  :type 'string)

(defcustom centaur-socks-proxy "127.0.0.1:7890"
  "Set HTTP/HTTPS proxy."
  :group 'happyo
  :type 'string)

(setq centaur-socks-proxy "127.0.0.1:7890")    ; SOCKS proxy

(setq centaur-proxy "127.0.0.1:7890")          ; HTTP/HTTPS proxy

(defvar socks-noproxy)
(defvar socks-server)

;; Network Proxy
(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" centaur-proxy)
    (message "No HTTP proxy")))

(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,centaur-proxy)
          ("https" . ,centaur-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (proxy-http-disable)
    (proxy-http-enable)))

(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string centaur-socks-proxy ":"))
         (host (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" centaur-socks-proxy))
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
    (proxy-socks-disable)
    (proxy-socks-enable)))

;;; start emacs with proxy setting ==========================
;; (add-hook 'emacs-startup-hook (lambda () (proxy-http-enable)))

(provide 'init-proxy)
;;; init-proxy.el ends here
