;;; l-web.el --- Web Development -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;;; html 
;; in insert mode: C-j or C-<return> to expand
(use-package emmet-mode
	:hook ((html-mode . emmet-mode)))

;;; Css =====================================================
(use-package rainbow-mode
	:hook ( ((mhtml-mode html-mode css-mode) . rainbow-mode) )) ;; TODO more specific mode

;;; Vue =====================================================
;; @ Custom vue mode based on web-mode
(use-package web-mode)
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;; eglot for vue-mode
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(vue-mode . ("vls" "--stdio"))))

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

;;; Keybindings =============================================
(mapBegin!
 (mk/local-leader-def
	 :states 'normal
	 ;; :keymaps '(mhtml-mode html-mode) ;; no use :(
	 "l" '(:ignore t :which-key "live-web")
	 "ls" '(mk/live-web-start :which-key "start")
	 "lk" '(mk/live-web-kill :which-key "kill")
	 "ll" '(mk/live-web-toggle :which-key "toggle")
	 ))

(provide 'l-web)
