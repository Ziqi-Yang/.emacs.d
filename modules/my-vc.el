;;; my-vc.el --- version control -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Magit ===================================================
;; use C-j and C-k to navigate instead
;; when meet key conflicts, please refer to evil-collection
;; https://github.com/emacs-evil/evil-collection
;; FIXME it seems that magit has performance issue
;; (use-package magit
;; 	:config
;; 	(setq magit-status-buffer-switch-function #'switch-to-buffer)
;; 	:custom
;;   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; @ Forge
;; Work with Git forges, such as Github and Gitlab, from the comfort of Magit and the rest of Emacs.
;; (use-package forge 
;;   :disabled)

;; (use-package magit-todos)

;;; Diff-hl =================================================
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  ;; When Emacs runs in terminal, show the indicators in margin instead.

	;; make sure it works in daemon mode
	(add-hook 'server-after-make-frame-hook
		#'(lambda () (unless (display-graphic-p)
									 (diff-hl-margin-mode))))
	(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
	(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


(defun mk/log-edit/insert-gitmessages()
  (let (message)
    (when (file-readable-p "~/.gitmessage.txt")
      (with-temp-buffer
        (insert-file-contents "~/.gitmessage.txt")
        (setq message (buffer-string)))

      (save-excursion
        (goto-char (point-max))
        (overlay-put (make-overlay (point) (point))
          'after-string message)))))

(with-eval-after-load 'log-edit
  (customize-set-value 'log-edit-hook '(log-edit-insert-message-template
                                         mk/log-edit/insert-gitmessages
                                         log-edit-show-files)))

(provide 'my-vc)

;;; my-vc.el ends here
