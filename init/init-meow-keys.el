;;; init-meow-keys.el --- meow model editing -*- lexical-binding: t; -*-
;; Copyright (C) 2023  Ziqi Yang
;; Author: Ziqi Yang <mr.ziqiyang@gmail.com>
;; Comments:

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow-keypad-start-keys '((?h . ?h)
                                 (?x . ?x)))

  ;; motion
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))

  ;; leader key
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")

   ;; USE SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)

   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  ;; normal mode
  (meow-normal-define-key
   '("C-x )" . meow-end-or-call-kmacro)
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '("=" . indent-region)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . mk/meow/find-backward)
   ;; '("g" . meow-cancel-selection)
   '("g" . mk/rectangle)
   '("G" . mk/better-meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   ;; '("O" . meow-to-block)
   '("O" . expreg-expand)
   '("p" . yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   ;; '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . mk/meow/till-backward)
   '("u" . undo-only)
   ;; '("U" . meow-undo-in-selection)
   '("U" . undo-redo)
   '("v" . avy-goto-char-in-line) ;; meow-visit
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . mk/mark-line-smart)
   ;; '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("Z" . consult-goto-line)
   '("'" . repeat)
   '("<escape>" . ignore)

   '("<" . indent-rigidly-left)
   '(">" . indent-rigidly-right)
   '("?" . lspx-show-documentation-eldoc)
   '("~" . upcase-dwim)
   ;; M-; comment-dwim (toggle comment)
   ;; '("C-i" . pop-global-mark)
   '("M-d" . surround-delete)
   '("M-D" . surround-change)
   '("C-o" . xref-go-back)
   '("C-i" . xref-go-forward)
   '(":" . async-shell-command)
   '("C-m" . set-mark-command)
   '("C-M-h" . backward-sexp)
   '("C-M-l" . forward-sep)
   '("C-." . embark-act)
   '("C-S-v" . clipboard-yank)
   '("C-S-c" . mk/better-clipboard-kill-ring-save)
   '("!" . hs-toggle-hiding)

   ;; window/tab management
   '("C-S-l" . tab-bar-duplicate-tab)
   '("C-S-h" . tab-bar-close-tab)
   '("C-S-j" . tab-bar-switch-to-next-tab)
   '("C-S-k" . tab-bar-switch-to-prev-tab)
   '("C-S-n" . rotate-windows)
   '("C-S-p" . rotate-windows-back)))

(use-package meow
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package meow-tree-sitter
  :after meow
  :config
  (meow-tree-sitter-register-defaults))


;; remap `q' in major modes
(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "q") #'kill-current-buffer))
(with-eval-after-load 'image-mode
  (define-key image-mode-map (kbd "q") #'kill-current-buffer))

;;; Add things to meow-cheatsheet

(define-advice meow-cheatsheet
    (:after (&rest args) add-notes)
  (let ((notes
         '(("Completion" .
            "Command | Keybinding | Description
ispell-complete-word | C-M-i")
           ("Selection Operation" .
            "Command | Keybinding | Description
dabbrev-copmletion | C-M-/ 
mk/trans-map/rectangle | C-x r r
align-regexp | C-u M-x | align regexp whole line")
           ("VC Operation" .
            "Command | Keybinding | Description
Smerge | C-c ^ | `=' diff; `o': merge below")
           ("Miscellaneous" . "Command | Keybinding | Description
table-insert
table-capture"))))
    (save-excursion
      (with-current-buffer (current-buffer)
        (setq buffer-read-only nil)
        (goto-char (point-max))
        (insert "\n\n" (propertize "My Custom Notes" 'font-lock-face 'org-level-1) "\n\n")
        (dolist (note notes)
          (insert (propertize (car note) 'font-lock-face 'org-level-2) "\n")
          (let ((point-beg (point)))
            (insert (cdr note))
            ;; note that `table-capture' won't update point
            (table-capture point-beg
                           (point)
                           "|" "$" 'left 20)
            (goto-char (point-max))
            (insert "\n")))
        (insert "1. In Beacon Mode, use `query-search' and then you can mark all
the occurrence with `meow-search' ('n'). [Though, `query-replace-regexp' can operate
on selection.]")
        (setq buffer-read-only t)))))

(defun mk/meow-grab-region()
  (interactive)
  (if (secondary-selection-exist-p)
    (meow--cancel-second-selection)
    (save-excursion
      (call-interactively #'meow-bounds-of-thing)
      (meow-grab))))

(defun mk/better-meow-grab()
  "Better `meow-grab'.
If there is no active region, do `mk/meow-grab-region'; else do `meow-grab'."
  (interactive)
  (if (use-region-p)
      (meow-grab)
    (mk/meow-grab-region)))

(defun mk/meow/find-backward ()
  (interactive)
  (let ((current-prefix-arg -1))
    (call-interactively #'meow-find)))

(defun mk/meow/till-backward ()
  (interactive)
  (let ((current-prefix-arg -1))
    (call-interactively #'meow-till)))

(provide 'init-meow-keys)

;;; init-meow-keys.el ends here
