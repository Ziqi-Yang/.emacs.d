;;; l-web.el --- Web Development -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;; in insert mode: C-j or C-<return> to expand
;; (use-package emmet-mode
;; 	:hook ((web-mode . emmet-mode)))

;; (use-package rainbow-mode
;; 	:hook ( ((mhtml-mode html-mode html-ts-mode css-mode web-mode) . rainbow-mode)))

(use-package colorful-mode
  :ensure (:type git :host github :repo "DevelopmentCool2449/colorful-mode")
  :hook (mhtml-mode html-mode html-ts-mode css-mode css-ts-mode web-mode))

;; put this line into .dir-locals
;; ((auto-mode-alist . (("\\.html\\'" . jinja2-mode))))
;; (use-package jinja2-mode)

(use-package web-mode
  ;; must be explicit or that mode loading will be depend on binding
  ;; so config won't be loaded
  :after eglot
  :bind (;; keybindings
         :map web-mode-map
         ("C-," . twind-insert-class-from-cheatsheet))
  :mode ("\\.html\\'" "\\.djhtml\\'" "\\.svelte\\'")
  :custom
  (web-mode-markup-indentation 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-part-face t)
  (web-mode-enable-comment-interpolation t)
  (web-mode-enable-heredoc-fontification t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-engine-detection t)
  :config
  (add-to-list 'eglot-server-programs '(web-mode . ("svelteserver" "--stdio"))))


(defun mk/setup-web-mode-for-emacs-client ()
  "Setup some values of web mode for emacs cliet.
Due to web-mode bug for emacs client, some customizable values need to be set after emacs client reload.  `display-graphic-p'."
  (if (display-graphic-p)
      (setq web-mode-enable-auto-closing t
            web-mode-enable-auto-pairing t
            web-mode-enable-auto-indentation nil
            web-mode-enable-auto-opening t
            web-mode-enable-auto-quoting t
            web-mode-enable-css-colorization t)))

(add-hook 'after-init-hook #'mk/setup-web-mode-for-emacs-client)
(add-hook 'server-after-make-frame-hook #'mk/setup-web-mode-for-emacs-client)

(use-package twind
  :ensure (:host github :repo "akirak/twind.el"))

(use-package typescript-ts-mode
  :ensure nil
  :bind (;; keybindings
         :map typescript-ts-mode-map
         ("C-," . twind-insert-class-from-cheatsheet)))


;;; Trivial =================================================
(defun mk/live-web-start()
  "Start live web server process using browser-sync."
  (interactive)
  (condition-case nil	(delete-process "live-web")	(error nil))
  (if (project-current) ;;; start browser-sync in the project root if in a project
      (start-process-shell-command "live-web"
                                   "*my-buffer*"
                                   (concat "browser-sync start --server " (project-root (project-current)) " --files '*.html,*.css,*.js,**/*.html,**/*.css,**/*.js'"))
    (start-process-shell-command "live-web"
                                 "*my-buffer*"
                                 "browser-sync start --server --files '*.html,*.css,*.js,**/*.html,**/*.css,**/*.js'"))
  (message "live web start"))

(defun mk/live-web-kill()
  "End live web server process."
  (interactive)
  (condition-case nil
      (delete-process "live-web")
    (error nil))
  (message "live web killed"))

(defun mk/live-web-toggle()
  "Toggle live web"
  (interactive)
  (if (get-process "live-web")
      (mk/live-web-kill)
    (mk/live-web-start)))


(provide 'l-web)

;;; l-web.el ends here
