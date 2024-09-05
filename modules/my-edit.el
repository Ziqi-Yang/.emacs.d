;;; my-edit.el --- editing experience -*- lexical-binding: t -*-
;;; Commentary:
;; Not Incluing KeyBindings
;;; Code:

(with-eval-after-load 'emacs
  (setq-default
   tab-always-indent t
   blink-cursor-mode nil
   tab-width 2
   indent-tabs-mode nil  ; use white spaces instead of tabs
   ;; scroll-margin 15
   scroll-step 1
   ;; make register indepentent from clipboard
   select-enable-clipboard nil))

(with-eval-after-load 'simple
  (setq-default fill-column 80)
  (add-hook 'prog-mode-hook 'auto-fill-mode)
  (add-hook 'text-mode-hook 'auto-fill-mode))

;; (with-eval-after-load 'visual-wrap
;;   (add-hook 'prog-mode-hook #'visual-wrap-prefix-mode))

;; @ remember cursor position
(use-package saveplace
  :ensure nil
  :config
  (save-place-mode))

;; change name case at ease (i.e. camel case, snake case)
(use-package string-inflection
  :ensure (:type git :host github :repo "akicho8/string-inflection"))


;; Surround ====================================================================
(use-package surround
  :ensure (:type git :host github :repo "mkleehammer/surround"))

;; native approach to surround text
(defun mk/set-surround-brackets-keybinding ()
  (when (display-graphic-p)
    ;; this line causes error in terminal
    ;; -- since Emacs maps win + .. to meta - [ in terminal Emacs
    ;; when I switch workspace in sway or do something else, it will automatically insert brackets
    (keymap-global-set "M-[" #'insert-pair)))

(add-hook 'after-init-hook #'mk/set-surround-brackets-keybinding)
(add-hook 'server-after-make-frame-hook #'mk/set-surround-brackets-keybinding)

(with-eval-after-load 'emacs
  (keymap-global-set "M-{" #'insert-pair)
  (keymap-global-set "M-\"" #'insert-pair)
  (keymap-global-set "M-'" #'insert-pair)
  (keymap-global-set "M-<" #'insert-pair))

;;; Paren =======================================================================

;; @ according to point position
;; (use-package highlight-parentheses
;;   ;; :hook ((prog-mode . highlight-parentheses-mode))
;;   :config
;;   (global-highlight-parentheses-mode)
;;   (setq highlight-parentheses-colors nil
;; 	  highlight-parentheses-highlight-adjacent t
;; 	  highlight-parentheses-attributes '((:weight ultra-bold :background "lightGray"
;; 						                             :box
;; 						                             ( :line-width (1 . -1)
;; 						                               :color ,(face-attribute 'shadow :foreground))))))

;; `show-parent-mode' when point is near a paren, highlight the matching paren
(use-package paren
  :ensure nil
  :custom
  ;; directly near the inner of paren
  (show-paren-when-point-inside-paren t)
  (show-paren-context-when-offscreen 'child-frame)
  :init
  ;; close this mode (default is open)
  (show-paren-mode t))

;; @ use electronic pair
(use-package elec-pair
  :ensure nil
  :hook ((prog-mode) . electric-pair-mode)
  :config
  (add-hook 'web-mode-hook #'(lambda () (electric-pair-local-mode -1)))
  (setq electric-pair-pairs '( ; make electric-pair-mode work on more brackets.
                              (?\( . ?\))
                              (?\{ . ?\})
                              (?\[ . ?\])
                              ;; (?\< . ?\>)
                              )))

;;; Indentation & format ====================================
(use-package aggressive-indent 
  ;; :hook ((prog-mode . aggressive-indent-mode))
  :delight
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;; note: .editorconfig configuration can lead to delete trailing characters on save
(use-package editorconfig
  :ensure nil
  :delight
  :config
  (editorconfig-mode 1))

;; expand region ===========
(use-package expreg
  :ensure (:type git :host github :repo "casouri/expreg"))

;;; My custom functions ========================================================

(defun mk/delete-symbol-at-point ()
  "Delete the symbol at point."
  (interactive)
  (let ((symbol-bounds (bounds-of-thing-at-point 'symbol)))
    (if symbol-bounds
        (delete-region (car symbol-bounds) (cdr symbol-bounds))
      (message "No symbol at point."))))

(defun mk/backward-delete-word (&optional arg)
  "Like `backward-kill-word', but don't modify kill-ring.
ARG: number of words to kill"
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun mk/better-clipboard-kill-ring-save ()
  "Copy region content or kill ring content to clipboard."
  (interactive)
  (if (display-graphic-p)
      (if (use-region-p)
          (call-interactively #'clipboard-kill-ring-save)
        (gui-set-selection 'CLIPBOARD (current-kill 0)))
    ;; for terminal emacs under Wayland
    (when (use-region-p)
      (call-interactively #'kill-ring-save))
    (call-process "wl-copy"
                  nil nil nil
                  "--" (current-kill 0))))

(defun mk/clipboard-yank (&optional arg)
  (interactive "P")
  (if arg
      (mk/yank-without-indent)  ; FIXME, not from clipboard
    (clipboard-yank)))

(defun mk/yank (&optional arg)
  (interactive "P")
  (if arg
      (mk/yank-without-indent)
    (yank)))

(defun mk/mark-line-smart ()
  "Mark the visible part of the current line.
  If current point is on a non-whitespace character, then mark the whole visible
  line; else the surrounding white spaces."
  (interactive)
  ;; (rx (or blank eol))
  (if (looking-at "[[:blank:]]\\|$") ;; if a white space is in current point
      ;; mark white spaces
      (when-let ((bounds (bounds-of-thing-at-point 'whitespace)))
        (push-mark (car bounds))
        (goto-char (cdr bounds)))
    ;; mark the whole visible line
    (progn
      (back-to-indentation) ;; go to the non-whitespace line beginning
      (push-mark (point))
      ;; go to the last non-whitespace line end
      (move-end-of-line nil)
      (re-search-backward "^\\|[^[:space:]]")
      (forward-char)))
  (activate-mark))

(provide 'my-edit)

;;; my-edit.el ends here
