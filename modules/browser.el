;;; browser.el --- browser settings -*- lexical-binding: t -*-

;;; Commentary:
;;;  specifically for eww

(defun mk/setup-eww ()
  "Basic EWW settings."
  ;; looking
  (setq shr-use-fonts  nil)

  ;; keybindings
  (define-key eww-mode-map (kbd "h") #'backward-char)
  (define-key eww-mode-map (kbd "l") #'forward-char)
  (define-key eww-mode-map (kbd "H") #'eww-back-url)
  (define-key eww-mode-map (kbd "L") #'eww-next-url)
  (define-key eww-mode-map (kbd "x") #'meow-line)
  (define-key eww-mode-map (kbd "w") #'meow-mark-word)
  (define-key eww-mode-map (kbd "W") #'meow-mark-symbol)
  (define-key eww-mode-map (kbd "y") #'meow-save)
  (define-key eww-mode-map (kbd "/") #'avy-goto-word-1)
  
  (define-key eww-mode-map (kbd "C-c H") #'eww-list-histories)
  (define-key eww-mode-map (kbd "C-c r") #'eww-reload))

(add-hook 'eww-mode-hook 'mk/setup-eww)

(provide 'browser)
