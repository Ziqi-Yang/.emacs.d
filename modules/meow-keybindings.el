;;; meow-keybindings.el --- meow model editing                   -*- lexical-binding: t; -*-
;; Copyright (C) 2023  Ziqi Yang
;; Author: Ziqi Yang <mr.ziqiyang@gmail.com>
;; Comments:

(defun mk/better-clipboard-kill-ring-save ()
  "Copy region content or kill ring content to clipboard."
  (interactive)
  (if (use-region-p)
    (call-interactively #'clipboard-kill-ring-save)
    (gui-set-selection 'CLIPBOARD (current-kill 0))))

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
    '("g" . meow-cancel-selection)
    '("G" . meow-grab)
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
    '("p" . meow-yank)
    '("q" . meow-quit)
    '("Q" . meow-goto-line)
    '("r" . meow-replace)
    ;; '("R" . meow-swap-grab)
    '("s" . meow-kill)
    '("t" . meow-till)
    '("u" . meow-undo)
    ;; '("U" . meow-undo-in-selection)
    '("U" . undo-redo)
    '("v" . meow-visit)
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

    '("<tab>" . completion-at-point)
    '("<" . indent-rigidly-left)
    '(">" . indent-rigidly-right)
    '("?" . eldoc)
    '("~" . upcase-dwim)
    '("/" . avy-goto-word-1)
    ;; '("/" . combobulate-avy-jump)
    '("`" . combobulate-avy-jump-defun)
    ;; M-; comment-dwim (toggle comment)
    ;; '("C-i" . pop-global-mark)
    '("M-d" . surround-delete)
    '("M-D" . surround-change)
    '("C-o" . xref-go-back)
    '("C-i" . xref-go-forward)
    '(":" . async-shell-command)
    '("C-m" . set-mark-command)
    '("C-M-h" . backward-sexp)
    '("C-M-l" . forward-sexp)
    '("C-." . embark-act)
    '("C-S-v" . clipboard-yank)
    '("C-S-c" . mk/better-clipboard-kill-ring-save)
    '("!" . hs-toggle-hiding)))

(use-package meow
  :config
  (meow-setup)
  (meow-global-mode 1))

(provide 'meow-keybindings)

;;; meow-keybindings.el ends here
