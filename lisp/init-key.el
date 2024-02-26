;;; init-key.el --- keyBindings -*- lexical-binding: t -*-
;;; Commentary:
;; NOTE: global key bindings will be shadowed by local keybindings
;; TIPS:
;; meow-cheatsheet  (I've customed this command to also display my own notes)
;;; Code:

;; remove existing keybindings
;; (defun remove-elisp-mode-local-keybindings ()
;;   (keymap-local-unset "C-c C-b" t))
;;
;; (add-hook 'emacs-lisp-mode-hook 'remove-elisp-mode-local-keybindings)

;; native approach to surround text
(defun mk/set-surround-brackets-keybinding ()
  (when (display-graphic-p)
    ;; this line causes error in terminal
    ;; -- since Emacs maps win + .. to meta - [ in terminal Emacs
    ;; when I switch workspace in sway or do something else, it will automatically insert brackets
    (keymap-global-set "M-[" #'insert-pair)))

(add-hook 'after-init-hook #'mk/set-surround-brackets-keybinding)
(add-hook 'server-after-make-frame-hook #'mk/set-surround-brackets-keybinding)

(keymap-global-set "M-{" #'insert-pair)
(keymap-global-set "M-\"" #'insert-pair)
(keymap-global-set "M-'" #'insert-pair)
(keymap-global-set "M-<" #'insert-pair)

(defmacro mk/define&set-keymap (prefix keymap-name definition)
  "Macro for defining a keymap.
PREFIX: prefix for the keymap, like \"C-c t\"
KEYMAP-NAME: like mk/test-keymap
DEFINITION example:
  \='((\"a\" . consult-buffer)
     (\"b\" . consult-line))
return value: KEYMAP-NAME callable (not keymap)
Example:
  (mk/define&set-main-keymap
    mk/test-keymap
    \='((\"a\" . consult-buffer)
       (\"b\" . consult-line)))"
  `(progn
     (defvar ,keymap-name
       (let ((keymap (make-sparse-keymap)))
         (dolist (entry ,definition)
           (let ((key (car entry))
                  (command (cdr entry)))
             (define-key keymap (kbd key) command)))
         keymap))
     (defalias ',keymap-name ,keymap-name)
     (global-set-key (kbd ,prefix) ',keymap-name)
     ',keymap-name))

;; Vim-like  keybinding
(progn
  (keymap-global-set "C-w" #'mk/backward-delete-word)
  (keymap-global-set "C-M-#" #'kill-region)
  (setq meow--kbd-kill-region "C-M-#")

  (keymap-global-set "C-d" #'scroll-up-command)
  (keymap-global-set "C-M-!" #'delete-char)
  (setq meow--kbd-delete-char "C-M-!")

  (keymap-global-set "M-:" #'comment-box)

  (keymap-global-set "C-M-u" #'universal-argument)
  (keymap-global-set "C-u" #'scroll-down-command))

(progn
  (keymap-global-set "M-<left>" #'backward-sexp)
  (keymap-global-set "M-<right>" #'forward-sexp)
  (keymap-global-set "M-h" #'backward-sexp)
  (keymap-global-set "M-l" #'forward-sexp)

  (keymap-global-set "<backtab>" #'outline-cycle)
  
  (keymap-global-set "M-SPC" #'meow-last-buffer)

  (keymap-global-set "M-?" #'mk/toggle-documentation-buffer-display)

  (keymap-global-set "C-s" #'isearch-forward-regexp)
  (keymap-global-set "C-r" #'isearch-backward-regexp)
  (keymap-global-set "C-/" #'avy-goto-word-1)
  (keymap-global-set "C-M-/" #'avy-isearch)
  
  
  (keymap-global-set "S-<return>" #'meow-open-below)
  (keymap-global-set "M-S-<return>" #'meow-open-above)
  
  (keymap-global-set "C-M-@" #'forward-char)
  (setq meow--kbd-forward-char "C-M-@")
  (keymap-global-set "C-M-$" #'kill-line)
  (setq meow--kbd-kill-line "C-M-$")

  (keymap-global-set "C-`" #'popper-toggle)
  (keymap-global-set "M-`" #'popper-cyclce)
  (keymap-global-set "C-M-`" #'popper-toggle-type)

  (keymap-global-set "M-<backspace>" #'mk/delete-symbol-at-point)
  (keymap-global-set "C-S-v" #'mk/clipboard-yank)
  (keymap-global-set "C-<return>" #'mk/completion-at-point-with-tempel)
  ;; cape-dabbrev has been integrated into completion-at-point function already
  (keymap-global-set "M-/" #'dabbrev-completion)
  (keymap-global-set "C-S-f" #'cape-file)
  (keymap-global-set "C-S-l" #'mk/cape-line-previous-buffer))

(defun mk/keyBindingSetup ()
  ;; trivial
  (keymap-global-set "C-c :" #'eval-expression)
  (keymap-global-set "C-c \`" #'tab-switch)
  (keymap-global-set "C-c ;" #'async-shell-command)
  ;; (keymap-global-set "C-c SPC" #'which-key-show-major-mode)
  (keymap-global-set "C-c SPC" #'consult-mode-command) ;; use l/g/m to filter
  (keymap-global-set "C-c ~" #'list-processes)
  ;; C-h (SPC h) ================================================================
  ;; help (h) SPC h SPC <character>
  (keymap-global-set "C-h h" #'meow-cheatsheet)
  (keymap-global-set "C-h M" #'woman)
  (keymap-global-set "C-h d" #'shortdoc)
  (keymap-global-set "C-h c" #'helpful-callable)
  (keymap-global-set "C-h I" #'mk/trans-map/consult-info) ;; original: describe-input-method
  ;; C-x (SPC x) ================================================================
  ;; tab related (SPC x SPC t)
  ;; vundo ( SPC x u)
  (keymap-global-set "C-x C-u" #'vundo)
  ;; highlight symbols ( SPC x h/H)
  ;; meow built-in functionality is enough for doing search / replace job
  ;; (keymap-global-set "C-x h" #'symbol-overlay-put)
  ;; (keymap-global-set "C-x H" #'symbol-overlay-remove-all)
  (keymap-global-set "C-x h" #'highlight-regexp)
  (keymap-global-set "C-x H" #'unhighlight-regexp)
  ;; Mail (SPC x SPC m)
  (keymap-global-set "C-x m" #'mu4e)
  (keymap-global-set "C-x M" #'mu4e-compose-new) ;; use this to replace `compose-mail' function
  ;; vc commands (git)
  ;; C-x v (SPC x SPC v)
  ;; vc-next-action is useful, and can be used to commit. see info:emacs#Basic VC Editing
  ;; try use vc-next-action on vc-root-diff buffer, or on vc-dir buffer (first
  ;; mark using (prefix) m/M, then v (add), then v (commit)
  (keymap-global-set "C-x v p" #'vc-prepare-patch)
  (keymap-global-set "C-x v e" #'vc-ediff)
  (keymap-global-set "C-x v b p" #'mk/print-current-branch-name)
  
  (keymap-global-set "C-x v D" #'vc-dir)
  (mk/define&set-keymap
   "C-x v d" keymap/vc-diff 
   '(("d" . vc-diff)
     ("D" . vc-root-diff)
     ("e" . vc-ediff)
     ("v" . vc-version-diff)
     ("V" . vc-root-version-diff)
     ("E" . vc-version-ediff)))
  
  ;; diff (SPC x SPC d)
  (keymap-global-set "C-x d" #'diff)
  ;; smerge
  ;; use C-c ^
  ;; C-M- (SPC g) ===============================================================
  (keymap-global-set "C-M-l" #'recenter-top-bottom) ;; gl
  (keymap-global-set "C-M-s" #'scratch-buffer)      ;; gs
  (keymap-global-set "C-M-g" #'mk/project-git)      ;; gg

  ;; buffer(b)
  (mk/define&set-keymap
   "C-c b" keymap/buffer
   '(("b" . mk/smart-buffer-switch-no-hidden)
     ("B" . mk/smart-buffer-switch) ;; use SPC to filter hidden buffer 
     ("o" . switch-to-buffer-other-window)
     ("c" . mk/switch-to-compilation-buffer)
     ("e" . mk/switch-to-eww-buffer)
     ("i" . ibuffer)
     ("r" . mk/reload-buffer)
     ("d" . kill-current-buffer)
     ("k" . kill-current-buffer)
     ("K" . kill-matching-buffers-no-ask)))

  ;; bookmark(B)
  ;; also use consult-bookmark to jump to buffer
  (mk/define&set-keymap
   "C-c B" keymap/bookmark
   `(("e" . ,(mk/define&set-keymap
              "C-c B e" keymap/ebm
              '(("c" . ebm-create)
                ("v" . ebm-view-database))))
     ("b" . bookmark-jump)
     ("c" . bookmark-set)
     ("d" . bookmark-delete)))

  ;; code(c)
  (mk/define&set-keymap
   "C-c c" keymap/code
   `(;; note: you can select a region and then use this command, which prevents
     ;; you from executing code actions for flymake errors (which often results
     ;; in no actions)
     ("a" . mk/code/action)
     ("B" . ,(mk/define&set-keymap
              "C-c c D" keymap/code-debug
              '(("d" . mk/gf-debug)
                ("D" . mk/gdb-smart)
                ("v" . mk/debug-with-valgrind))))
     ("d" . mk/code/find-definition)
     ("D" . mk/code/find-definition-other-window)
     ;; eldoc: use ? (binding in meow.el)
     ("e" . mk/code/error-list)
     ("f" . ,(mk/define&set-keymap
              "C-c c f" keymap/code-format
              '(("f" . mk/refresh-file)
                ("e" . eglot-format)
                ("F" . apheleia-format-buffer))))
     ("F" . eglot-code-action-quickfix)
     ("h" . ,(mk/define&set-keymap
              "C-c c h" keymap/code-hierarchy
              '(("t" . eglot-hierarchy-type-hierarchy)
                ("c" . eglot-hierarchy-call-hierarchy))))
     ("H" . mk/code/toggle-inlay-hint)
     ("j" . citre-jump)
     ("k" . citre-jump-back)
     ;; ("i" . eglot-code-action-organize-imports)
     ("i" . mk/code/find-implementation)
     ("o" . ,(mk/define&set-keymap
              "C-c c o" keymap/code-other
              '(("c" . citre-create-tags-file)
                ("e" . citre-edit-tags-file-recipe))))
     ("p" . lsp-bridge-peek)
     ("P" . citre-peek)
     ("r" . mk/code/find-references)
     ("s" . mk/xref-stack-current-position)
     ("R" . eglot-rename)
     ("u" . citre-update-this-tags-file)
     ("U" . mk/update-all-tags)))

  ;; emoji(e)
  (mk/define&set-keymap
   "C-c e" keymap/emoji
   '(("e" . emoji-insert)
     ("r" . emoji-recent)))

  ;; file(f)
  (mk/define&set-keymap
   "C-c f" keymap/file
   `(("d" . mk/consult-fd-current-directory)
     ("D" . mk/delete-file)
     ("f" . mk/smart-find-file)
     ("F" . mk/find-file-other-window)
     ("p" . project-find-file)
     ("P" . project-find-dir)
     ("r" . recentf-open)
     ("R" . rename-visited-file)
     ("s" . ,(mk/define&set-keymap
              "C-c f s" keymap/sqlite
              '(("s" . sqlite-mode-open-file)
                ("S" . sql-sqlite))))
     ("S" . others/sudo-find-file)
     ("v" . view-file)
     ("z" . zoxide-find-file)))

  ;; fold(F)
  (mk/define&set-keymap
   "C-c F" keymap/fold
   '(("h". mk/hs-hide-level-samrt)
     ("H" . hs-show-all)
     ("o" . outline-show-only-headings)
     ("O" . outline-show-all)))

  ;; easy GPG assistant
  (mk/define&set-keymap
   "C-c G" keymap/epa
   `(("l" . epa-list-keys)
     ("r" . ,(mk/define&set-keymap
              "C-c G r" mk/epa-region-keymap
              '(("e" . epa-encrypt-region)
                ("d" . epa-decrypt-region)
                ("s" . epa-sign-region)
                ("v" . epa-verify-region))))
     ("f" . ,(mk/define&set-keymap
              "C-c G f" mk/epa-file-keymap
              '(("e" . epa-encrypt-file)
                ("d" . epa-decrypt-file)
                ("s" . epa-sign-file)
                ("v" . epa-verify-file))))))

  ;; Hugo(H)
  (mk/define&set-keymap
   "C-c H" keymap/hugo
   '(("h" . mk/hugo/cd-project)
     ("p" . mk/hugo/toggle-preview)
     ("t" . mk/hugo/find-blog-using-tag-search)
     ("d" . mk/hugo/goto-draft)
     ("b" . mk/hugo/build)
     ("f" . mk/hugo/edit-or-create)))

  ;; narrow(n)
  (mk/define&set-keymap
   "C-c n" keymap/narrow
   '(("n" . narrow-to-region)
     ("p" . narrow-to-page)
     ("d" . narrow-to-defun)
     ("w" . widen)))

  ;; open(o)
  (mk/define&set-keymap
   "C-c o" keymap/open
   '(
     ;; ("-" . vterm)
     ;; ("=" . others/project-vterm)
     ("s" . dired-sidebar-toggle-sidebar)
     ("a" . org-agenda)
     ("A" . (lambda () (interactive) (find-file "~/notes/agenda.org")))
     ("b" . (lambda () (interactive) (find-file "~/Documents/meow_king.srht.site/content")))
     ("e" . eww-list-bookmarks)
     ("E" . mk/browse-emacs-devel)
     ("r" . (lambda () (interactive) (find-file "~/projects/rust/LearningRustOS2023Record/README.org")))
     ("d" . dired-jump)
     ("D" . mk/open-dired-smart)
     ("r" . mk/draw-diagram)
     ("t" . mk/open-terminal-smart)
     ("T" . mk/open-terminal-here)))

  ;; project(p)
  (mk/define&set-keymap
   "C-c p" keymap/project
   '(("A" . project-remember-projects-under)
     ("c" . mk/project-compile)
     ("p" . project-switch-project)
     ("P" . project-forget-project)
     ("e" . mk/open-emacs.d)
     ("E" . flymake-show-project-diagnostics)
     ("v" . project-vc-dir)
     ;; ("s" . others/project-vterm)
     ("s" . project-eshell)
     ("S" . project-async-shell-command)
     ("k" . project-kill-buffers)))

  ;; peek (P)
  (mk/define&set-keymap
   "C-c P" keymap/peek
   '(("p" . peek-overlay-dwim)
     ("x" . peek-xref-definition)
     ("m" . peek-overlay-eldoc-message-toggle-stauts)
     ("d" . peek-collection-dict)))

  ;; replace (r)
  ;; note: use emacs narrow function to to things better
  (mk/define&set-keymap
   "C-c r" mk/replace-keymap
   `(("r" . substitute-target-in-buffer)
     ("d" . substitute-target-in-defun)
     ("R" . query-replace-regexp)
     ("g" . ,(mk/define&set-keymap
              "C-c r g" mk/replace-colorrg-keymap
              '(("i" . color-rg-search-input-in-current-file)
                ("I" . color-rg-search-input-in-project)
                ("b" . color-rg-search-symbol-in-current-file) ;; buffer
                ("d" . color-rg-search-symbol-with-type)       ;; directory
                ("p" . color-rg-search-project-with-type)  ;; project
                )))))

  ;; search (s)
  (mk/define&set-keymap
   "C-c s" keymap/search
   `(("a" . ,(mk/define&set-keymap
              ;; note `shortdoc' is also great for searching functions (I set it to `C-h d')
              "C-c s a" mk/search-apropos-keymap
              '(("a" . apropos)
                ("c" . apropos-command)
                ("C" . customize-apropos)
                ("d" . apropos-documentation)
                ("v" . apropos-variable)
                ("f" . apropos-function)
                ("l" . apropos-library)
                ("i" . mk/better-info-apropos)
                ("I" . info-apropos))))
     ("s" . mk/better-consult-line)
     ("S" . mk/better-consult-line-multi)
     ("f" . consult-focus-lines)
     ("h" . isearch-highlight-regexp)
     ("H" . unhighlight-regexp)
     ("c" . list-colors-display)
     ;; note you can input keys like `v' and add a space after it to filter
     ("i" . consult-imenu) 
     ("I" . consult-imenu-multi)
     ("m" . mk/better-consult-man)
     ("M" . consult-global-mark)
     ;; NOTE: to post filter to filter group (i.e. filename in this case)
     ;; https://github.com/minad/consult/issues/799
     ("p" . mk/consult-ripgrep-file-type)
     ("b" . bookmark-jump)
     ("d" . dictionary-search)
     ("o" . consult-outline)
     ("O" . mk/search-online)
     ("r" . consult-register)
     ("R" . consult-register-store)
     ("t" . ,(mk/define&set-keymap
              "C-c s t" keymap/search-todo
              '(("t" . consult-todo)
                ("T" . consult-todo-all)
                ("p" . consult-todo-project)
                ("d" . consult-todo-dir)
                ("T" . hl-todo-rgrep))))
     ("y" . consult-yank-from-kill-ring)))

  ;; toggle (t)
  (mk/define&set-keymap
   "C-c t" keymap/toggle
   '(("w" . whitespace-mode)
     ("r" . read-only-mode)
     ("R" . mk/global-read-only-mode)
     ("v" . view-mode)
     ("m" . meow-temp-normal)
     ("c" . rainbow-mode)
     ("t" . consult-theme)))

  ;; window(w)
  (mk/define&set-keymap
   "C-c w" keymap/window
   '(("c" . mk/ace-copy-window)
     ("f" . other-frame)
     ("w" . ace-window)
     ("W" . mk/ace-window-balance-window)
     ("t" . others/window-split-toggle)
     ("f" . fit-window-to-buffer)
     ("q" . delete-window)
     ("o" . ace-delete-window)
     ("O" . delete-other-windows)
     ("m" . maximize-window)
     ("M" . minimize-window)
     ("s" . ace-swap-window)
     ("b" . balance-windows)
     ("+" . maximize-window)
     ("-" . minimize-window)
     ("=" . balance-windows)
     ("v" . mk/split-window-vertically)
     ("h" . mk/split-window-horizontally)))

  ;; utility (x)
  (mk/define&set-keymap
   "C-c x" keymap/command
   `(("h" . proxy-http-toggle)
     ("H" . proxy-http-show)
     ("s" . proxy-socks-toggle)
     ("S" . proxy-socks-show)
     ("p" . ,(mk/define&set-keymap
              "C-c x p" keymap/package-manager
              '(("d" . elpaca-delete)
                ("p" . elpaca-manager)
                ("b" . elpaca-browse)
                ("f" . elpaca-fetch)
                ("F" . elpaca-fetch-all)
                ("v" . elpaca-visit)
                ("l" . elpaca-log)
                ("t" . elpaca-try)
                ("s" . elpaca-status)
                ("u" . mk/elpaca-update)
                ("U" . mk/elpaca-update-all))))))

  ;; trivial (z)
  (mk/define&set-keymap
   "C-c z" keymap/trivial
   '(("t" . mk/translate)
     ("c" . jinx-correct)
     ("C" . list-colors-display)
     ("d" . ediff-buffers)
     ("q" . save-buffers-kill-emacs)
     ("Q" . kill-emacs)
     ("r" . restart-emacs)
     ;; ("s" . desktop-save-in-desktop-dir)
     ;; ("l" . desktop-load-file)
     ("n". string-inflection-cycle)
     ("p" . mk/copy-path-smart)
     ("z" . mk/adb-send-messages)
     ("RET" . mk/adb-wechat-press-send-button)
     ("x" . mk/adb--delete-char))))

;; for tapping key which begins with a character other than SPC
;; so `meow-keypad' won't appear
(use-package which-key
  :delight
  :init
  (setq which-key-idle-delay 0.5)
  (setq which-key-side-window-max-height 0.3)
  :config
  (which-key-mode)
  (mk/keyBindingSetup))

;;; Trivial Settings ========================================
(defun mk/Custom-mode-keybinding-setup()
  (keymap-local-set "<return>" #'widget-button-press))

(add-hook 'Custom-mode-hook 'mk/Custom-mode-keybinding-setup)

;;; Trivial Functions =======================================
(defun others/indent-buffer ()
  "Automatic format current buffer."
  (interactive)
  (if (derived-mode-p 'python-mode)
    (message "Don't indent python buffer, it will mess up the code syntax.")
    (save-excursion
      (indent-region (point-min) (point-max) nil)
      (delete-trailing-whitespace)
      (untabify (point-min) (point-max)))))

(defun others/indent-comment-buffer ()
  "Indent comment of buffer."
  (interactive)
  (others/indent-comment-region (point-min) (point-max)))

(defun others/indent-comment-region (start end)
  "Indent region.
START END."
  (interactive "r")
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (while (< (point) end)
      (if (comment-search-forward end t)
        (comment-indent)
        (goto-char end)))))

(defun mk/kill-all-buffers ()
  "Kill all buffers except *dashboard*."
  (interactive)
  (mapc 'kill-buffer (delq (get-buffer "*dashboard*") (buffer-list))))

(defun mk/open-terminal-smart()
  "Open terminal at project root if in a project, otherwise current folder."
  (interactive)
  (let (
         ;; (command-prefix "hyprctl dispatch exec '[workspace 1 slien; float; size 90% 40%; move 5% 58%]  kitty -d ")
         (command-prefix "foot -a floating -D ")) ;; right parenthesis is needed to be added after concatance
    (if (project-current)
      (start-process-shell-command "open terminal" "*terminal*"
        (concat command-prefix (project-root (project-current))))
      (start-process-shell-command "open terminal" "*terminal*"
        ;; (concat command-prefix (file-name-directory buffer-file-name) "'")
        (concat command-prefix (file-name-directory buffer-file-name))))))

(defun mk/open-terminal-here()
  "Open terminal at the current folder."
  (interactive)
  (let (
         ;; (command-prefix "hyprctl dispatch exec '[workspace 1 slien; float; size 90% 40%; move 5% 58%]  kitty -d ")
         (command-prefix "foot -a floating -D ")) ;; right parenthesis is needed to be added after concatance
    (start-process-shell-command "open terminal" "*terminal*"
      ;; (concat command-prefix (file-name-directory buffer-file-name) "'")
      (concat command-prefix (file-name-directory buffer-file-name)))))

(defun mk/translate()
  "Translate words at the point by using ydicy in the external terminal alacritty."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
          (pos1 (car bounds))
          (pos2 (cdr bounds))
          (word (buffer-substring-no-properties pos1 pos2))
          (command (concat "echo " word " ; source $HOME/.config/fish/functions/t.fish && t " word " ; echo ------------------------------ ; echo [Use Ctrl-Shift-Space to toggle vi mode] ; read -P '[Press ENTER key to exit]'"))
          )
    (start-process-shell-command "my-translator" "*my-buffer*" (concat "alacritty --class floating -e /usr/bin/fish -c \"" command "\""))
    ))

(defun mk/smart-buffer-switch-no-hidden (&optional arg)
  "Buffer switch according to project existence without showing hidden buffers.
ARG: prefix argument.  When ARG is non-nil, then force use
`mk/consult-buffer-no-hidden' command instead."
  (interactive "P")
  (if (and (not arg) (project-current))
    (mk/consult-project-buffer-no-hidden)
    (mk/consult-buffer-no-hidden)))

(defun mk/smart-buffer-switch (&optional arg)
  "Smart buffer switch according to project existence.
ARG: prefix argument.  When ARG is non-nil, then force use
`consult-buffer' command instead."
  (interactive "P")
  (if (and (not arg) (project-current))
      (consult-project-buffer)
    (consult-buffer)))

(defun mk/completion-at-point-with-tempel ()
  "`Completion-at-point' function with tempel support.
When tempel-trigger-prefix is before the point, then use temple, else `completion-at-point'."
  (interactive)
  (if tempel--active
      (call-interactively 'tempel-next)
    (if (and tempel-trigger-prefix
             (length> tempel-trigger-prefix 0)
             ;; (rx-to-string `(seq ,tempel-trigger-prefix (* (not (or space punct)))))
             (looking-back "\\(?:<[^[:punct:][:space:]]*\\)" nil))
        (condition-case nil
            (call-interactively 'tempel-complete)
          (user-error
           (if lsp-bridge-mode
               (lsp-bridge-popup-complete-menu)
             (completion-at-point))))
      (if lsp-bridge-mode
          (lsp-bridge-popup-complete-menu)
        (completion-at-point)))))

(defun mk/split-window-horizontally ()
  "Split window horizontally & Move to spawned window."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun mk/split-window-vertically ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun mk/delete-file ()
  "Delete the current buffer file."
  (interactive)
  (if (not buffer-file-name)
    (message "[Error] This buffer havn't been saved to file.")
    (let ((whether-to-delete (yes-or-no-p "Whether to delete this file?")))
      (if whether-to-delete
        (progn
          (move-file-to-trash buffer-file-name)
          (kill-buffer))
        nil)
      )))

;; NOTE: deprecated since 29.1 because of the builtin function rename-visited-file
(defun mk/rename-file ()
  "Rename the current buffer file."
  (interactive)
  (if (not buffer-file-name)
    (message "[Error] This buffer havn't been saved to file.")
    (let ((new-file-name (read-string "Enter a new name:" (file-name-nondirectory (buffer-file-name))))
           (old-buffer (current-buffer)))
      (rename-file buffer-file-name new-file-name)
      (find-file new-file-name)
      (kill-buffer old-buffer))))

(defun others/window-split-toggle ()
  "Toggle window layout: vertical <-> horizontal."
  (interactive)
  (if (eq (length (window-list)) 2)
    (let ((func (if (window-full-height-p)
                  #'split-window-vertically
                  #'split-window-horizontally)))
      ;; to make sure the other buffer has been selected once
      (other-window 1)
      (other-window 1)
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))
    (error "Can't toggle with more than 2 windows!")))

(defun mk/reload-buffer ()
  "Use find-file to reload buffer if the file is changed by other programs."
  (interactive)
  ;; (find-file (buffer-file-name))
  (revert-buffer nil t))

(defun mk/better-emacs-lisp-byte-compile-and-load ()
  (interactive)
  ;; erase byte compile buffer
  (when (get-buffer byte-compile-log-buffer)
    (kill-buffer byte-compile-log-buffer))
  (emacs-lisp-byte-compile-and-load)
  ;; remove byte-compile file
  (delete-file (byte-compile-dest-file buffer-file-name)))

(defun mk/refresh-file (arg)
  "Refresh current file.  Indentation / Save / Load and other stuffs.
ARG: prefix argument."
  (interactive "P")
  (unless (fboundp 'apheleia--get-formatters)
    (require 'apheleia-core))
  (let ((formatter (apheleia--get-formatters)))
    (if formatter
      (apheleia-format-buffer formatter)
      (others/indent-buffer)
      (others/indent-comment-buffer)))
  (save-buffer)
  (cond
    ((eq major-mode 'emacs-lisp-mode)
      (mk/better-emacs-lisp-byte-compile-and-load))
    (t)))

(defun mk/project-compile (&optional confirm)
  "Save & Compile Project.
CONFIRM: universal argument. Whether a confirm is needed."
  (interactive "P")
  (save-buffer)
  (if confirm
    (project-compile)
    (let ((compilation-read-command nil))
      (project-compile))))

(defvar-local mk/search-engines
  '(("direct" . "%s")
     ("github" . "https://github.com/search?q=%s")
     ("google" . "https://www.google.com/search?q=%s")
     ("bing" . "https://www.bing.com/search?q=%s"))
  "Search engines used for function mk/search-online.")

(defun mk/search-online()
  "Search online, using word at point as default."
  (interactive)
  (let* ((word (current-word))
          (word (read-string "Search: " word))
          (engine-names (mapcar #'car mk/search-engines))
          (engine (completing-read "Choose a search engine:" engine-names))
          (search-url (cdr (assoc engine mk/search-engines)))
          (url (format search-url word)))
    (if url
      (progn
        ;; directly call Firefox instead of using browse-url to make parameters("?a=b") can be passed to local url
        ;; (browse-url url)
        (call-process "firefox-developer-edition" nil 0 nil url)
        (call-process "swaymsg" nil 0 nil "workspace" "3")
        (message "open url: %s" url))
      (message "Invalid search engine!"))))

(defun mk/copy-path-smart()
  "Copy current path/project path into clipboard."
  (interactive)
  (if (project-current)
    (mk/base/copy-string-to-clipboard (project-root (project-current)))
    (mk/base/copy-string-to-clipboard (file-name-directory buffer-file-name))))

(defvar mk/draw-diagram-path "~/Documents/diagrams/new.mmd"
  "Diagram file path for mk/draw-diagram.")

(defun mk/draw-diagram ()
  "Draw Mermaid Diagram."
  (interactive)
  (find-file mk/draw-diagram-path))

(defun mk/ace-window-balance-window ()
  "Balance window after `ace-window`"
  (interactive)
  (ace-window 0)
  (if (solaire-mode-real-buffer-p)
    (balance-windows)
    (maximize-window)))

(defun mk/switch-to-compilation-buffer()
  "Switch to compilation buffer"
  (interactive)
  (switch-to-buffer "*compilation*"))

(defun mk/delete-symbol-at-point ()
  "Delete the symbol at point."
  (interactive)
  (let ((symbol-bounds (bounds-of-thing-at-point 'symbol)))
    (if symbol-bounds
      (delete-region (car symbol-bounds) (cdr symbol-bounds))
      (message "No symbol at point."))))

(defun mk/project-git()
  "Open gitui at project root.(Due to magit's poor performance)"
  (interactive)
  (let ((command-prefix "kitty --class fullscreen -d ")) ;; right parenthesis is needed to be added after concatance
    (if (project-current)
      (start-process-shell-command "open terminal" "*terminal*"
        (concat command-prefix (project-root (project-current)) " ~/.emacs.d/gitui_start.sh"))
      (message "Not in a project!"))))

(defun mk/browse-emacs-devel()
  "Use eww to browse the emacs devel thread."
  (interactive)
  (eww (format "https://lists.gnu.org/archive/html/emacs-devel/%s/threads.html"
         (format-time-string "%Y-%0m"))))

(defun mk/better-info-apropos (arg)
  "Use symbol at point as the default input of `affe-grep'.
ARG: prefix argument.  Use prefix argument when you want no default input."
  (interactive "P")
  (if arg
    (call-interactively #'info-apropos)
    (info-apropos (thing-at-point 'symbol))))

;; (defun mk/better-query-replace (from to)
;;   "Perform a query-replace with default FROM and TO strings as the symbol at
;; point."
;;   (interactive
;;     (let* ((symbol (thing-at-point 'symbol))
;;             (reg (rx-to-string `(seq symbol-start ,symbol symbol-end)))
;;             (from (read-string "Replace: " reg))
;;             (to (read-string "With: " symbol)))
;;       (list from to)))
;;   ;; (save-excursion
;;   ;;   (beginning-of-buffer)
;;   ;;   (query-replace-regexp from to))
;;   (message-box "Use meow-edit functionality instead!"))

(defun mk/update-all-tags()
  "Update both ctags and gtags file (for citre)."
  (interactive)
  (citre-update-this-tags-file)
  (citre-global-update-database))

(defun mk/elpaca-update()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively #'elpaca-merge)))

(defun mk/elpaca-update-all()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively #'elpaca-merge-all)))

(defun mk/highlight-symbol-buffer()
  "Highlight all the symbols is that is the same of the one at point."
  (interactive)
  (let ((target (read-string "Highlight Symbol: "
                  (thing-at-point 'symbol))))
    (highlight-phrase target)))

(defun others/project-vterm ()
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
          (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
          (vterm-buffer (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
      (pop-to-buffer vterm-buffer  (bound-and-true-p display-comint-buffer-action))
      (vterm))))

(defun others/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
               (concat "/" (file-remote-p file 'method) ":"
                 (file-remote-p file 'user) "@" (file-remote-p file 'host)
                 "|sudo:root@"
                 (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

(defun mk/find-file-other-window ()
  "Open a window at right for file selected by `find-file'."
  (interactive)
  (split-window-right)
  (other-window 1)
  (call-interactively #'find-file))

(defun mk/open-dired-smart()
  "Open smart considering whether it's in a project."
  (interactive)
  (if (project-current)
    (project-dired)
    (dired-jump)))

(defun mk/smart-find-file ()
  "Context intelligent Find file."
  (interactive)
  (if (derived-mode-p 'dired-mode)
    (let ((default-directory (dired-current-directory)))
      (call-interactively #'find-file))
    (call-interactively #'find-file)))

(defun mk/backward-delete-word (&optional arg)
  "Like `backward-kill-word', but don't modify kill-ring.
ARG: number of words to kill"
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun mk/print-current-branch-name ()
  "Print current branch name."
  (interactive)
  (message "Current Branch: %s" (car (vc-git-branches))))

(defun mk/open-emacs.d ()
  "Open emacs.d project."
  (interactive)
  (project-switch-project user-emacs-directory))

(defun mk/hs-hide-level-samrt()
  "Calling hs-hide-level based on line numbers."
  (interactive)
  (when hs-minor-mode
    (let ((n (car (buffer-line-statistics)))
           (l3 500)
           (l2 600)
           (l1 700)
           (l0 1000))
      (cond
        ((> n l0)
          (hs-hide-all)
          ;; (outline-show-only-headings)
          )
        ((> n l1) (hs-hide-all))     ;; also hide long comment
        ((> n l2) (hs-hide-level 1)) ;; show root function
        ((> n l3) (hs-hide-level 2))))))

(defun mk/ace-copy-window ()
  "Ace delete window."
  (interactive)
  (aw-select " Ace - Copy Window"
    #'aw-copy-window))

(defun mk/global-read-only-mode ()
  "`find-file-hook'."
  (interactive)
  (if (memq 'read-only-mode (default-value 'find-file-hook)) ;; diff-hl change the local value
    (progn
      ;; will remove all buffer's read-only-mode including those originally supposed to be
      (dolist (buf (buffer-list))
        (unless (string-match-p (rx (or " " "*") (* anychar)) (buffer-name buf))
          (with-current-buffer buf
            (read-only-mode -1))))
      (remove-hook 'find-file-hook #'read-only-mode))
    
    ;; add read-only-mode to all already existing normal buffers
    (dolist (buf (buffer-list))
      (unless (string-match-p (rx (or " " "*") (* anychar)) (buffer-name buf))
        (with-current-buffer buf
          (read-only-mode 1))))
    (add-hook 'find-file-hook #'read-only-mode)))

(defun mk/switch-to-eww-buffer ()
  (interactive)
  (switch-to-buffer "*eww*"))

(defun mk/toggle-documentation-buffer-display ()
  (interactive)
  (let ((buf (if lsp-bridge-mode
                 lsp-bridge-buffer-documentation-buffer
               eldoc--doc-buffer)))
    (let ((window (get-buffer-window buf)))
      (if window
          (quit-window nil window)
        (display-buffer buf)))))

(defun mk/code/find-definition ()
  (interactive)
  (if lsp-bridge-mode
      (lsp-bridge-find-def)
    (let ((this-command 'xref-find-definitions))
      (call-interactively #'xref-find-definitions))))

(defun mk/code/find-definition-other-window ()
  (interactive)
  (if lsp-bridge-mode
      (lsp-bridge-find-def-other-window)
    (let ((this-command 'xref-find-definitions-other-window))
      (call-interactively #'xref-find-definitions-other-window))))

(defun mk/code/find-references ()
  (interactive)
  (if lsp-bridge-mode
      (lsp-bridge-find-references)
    (let ((this-command 'xref-find-references))
      (call-interactively #'xref-find-references))))

(defun mk/code/find-implementation ()
  (interactive)
  (if lsp-bridge-mode
      (lsp-bridge-find-impl)
    (call-interactively #'eglot-find-implementation)))

(defun mk/code/toggle-inlay-hint ()
  (interactive)
  (if lsp-bridge-mode
      (setq lsp-bridge-enable-inlay-hint (not lsp-bridge-enable-inlay-hint))
    (call-interactively #'eglot-inlay-hints-mode)))

(defun mk/code/error-list ()
  (interactive)
  (if lsp-bridge-mode
      (lsp-bridge-diagnostic-list)
    (call-interactively #'lsp-bridge-diagnostic-list)))

(defun mk/code/action ()
  (interactive)
  (if lsp-bridge-mode
      (lsp-bridge-code-action)
    (call-interactively #'eglot-code-actions)))

(provide 'init-key)

;;; init-key.el ends here
