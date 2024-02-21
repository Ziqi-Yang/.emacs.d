;;; l-web.el --- Web Development -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;;; html 
;; in insert mode: C-j or C-<return> to expand
;; (use-package emmet-mode
;; 	:hook ((web-mode . emmet-mode)))

;;; Css =====================================================
(use-package rainbow-mode
	:hook ( ((mhtml-mode html-mode html-ts-mode css-mode web-mode) . rainbow-mode) )) ;; TODO more specific mode

(use-package web-mode
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
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

(defun mk/setup-web-mode-for-emacs-client ()
  "Setup some values of web mode for emacs cliet.
Due to web-mode bug for emacs client, some customizable values need to be set after emacs client reload.  `display-graphic-p'."
  (if (display-graphic-p)
      (setq web-mode-enable-auto-closing t
            web-mode-enable-auto-pairing t
            web-mode-enable-auto-indentation t
            web-mode-enable-auto-opening t
            web-mode-enable-auto-quoting t
            web-mode-enable-css-colorization t)))

(add-hook 'server-after-make-frame-hook #'mk/setup-web-mode-for-emacs-client)

;;; Vue Mode Eglot server configuration =========================================
;; https://github.com/joaotavora/eglot/discussions/1184#discussioncomment-5431145
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(defun vue-eglot-init-options ()
  (let ((tsdk-path (expand-file-name
                    "lib"
                    (shell-command-to-string "npm list --global --parseable typescript | head -n1 | tr -d \"\n\""))))
    `(:typescript (:tsdk ,tsdk-path
                         :languageFeatures (:completion
                                            (:defaultTagNameCase "both"
                                                                 :defaultAttrNameCase "kebabCase"
                                                                 :getDocumentNameCasesRequest nil
                                                                 :getDocumentSelectionRequest nil)
                                            :diagnostics
                                            (:getDocumentVersionRequest nil))
                         :documentFeatures (:documentFormatting
                                            (:defaultPrintWidth 100
                                                                :getDocumentPrintWidthRequest nil)
                                            :documentSymbol t
                                            :documentColor t)))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options)))))

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

(defun mk/web-local-keybinding-setup()
  (keymap-local-set "C-c C-c s" #'mk/live-web-start)
  (keymap-local-set "C-c C-c t" #'mk/live-web-toggle)
  (keymap-local-set "C-c C-c k" #'mk/live-web-kill))

(defun mk/add-web-local-map-hook (hook-list)
	(dolist (mode hook-list)
		(add-hook mode #'mk/web-local-keybinding-setup)))

(mk/add-web-local-map-hook '(js-mode-hook js-ts-mode-hook tsx-ts-mode-hook typescript-ts-mode-hook typescript-mode-hook))

(mk/add-web-local-map-hook '(web-mode-hook html-mode-hook mhtml-mode-hook vue-mode-hook css-mode-hook css-ts-mode)) ;; web, vue(defined in l-web.el) and css


(provide 'l-web)

;;; l-web.el ends here
