;;; init-key.el --- keyBindings -*- lexical-binding: t -*-
;;; Commentary:
;; NOTE: global key bindings will be shadowed by local keybindings
;; TIPS:

;; meow-cheatsheet  (I've customed this command to also display my own notes)
;; also note that C-, is left for my local mode bindings in editing (like web-mode)

;;; Code:

;; NOTE don't define same key since local keybinding will override global keybinding
(defconst MK/LOCAL_KEY_MAP "C-c _")
(defconst MK/LOCAL_KEY_MAP_GLOBAL_KEYBINDING "C-c .")

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

(defun mk/key-changed-placeholder ()
  (interactive)
  (message-box "use M-<backspace> instead"))

(progn
  ;; common behavior for terminal applications
  (keymap-global-set "C-w" #'mk/backward-delete-word)
  (keymap-global-set "C-M-#" #'kill-region)
  (setq meow--kbd-kill-region "C-M-#")

  ;; to familiar with nano editor
  (keymap-global-set "M-<backspace>" #'mk/backward-delete-word)
  (keymap-global-set "M-S-<backspace>" #'backward-kill-sexp)
  
  (keymap-global-set "C-u" #'scroll-down-command)
  (keymap-global-set "C-d" #'scroll-up-command)
  (keymap-global-set "C-M-!" #'delete-char)
  (setq meow--kbd-delete-char "C-M-!")

  (keymap-global-set "M-:" #'comment-box)
  (keymap-global-set "C-M-u" #'universal-argument))

(progn
  (keymap-global-set "C-S-<mouse-1>" #'mc/add-cursor-on-click)
  
  (keymap-global-set "C--" #'global-text-scale-adjust)
  (keymap-global-set "C-=" #'global-text-scale-adjust)
  (keymap-global-set "M-h" #'windmove-left)
  (keymap-global-set "M-j" #'windmove-down)
  (keymap-global-set "M-k" #'windmove-up)
  (keymap-global-set "M-l" #'windmove-right)

  (keymap-global-set "<backtab>" #'outline-cycle)
  
  (keymap-global-set "M-<tab>" #'tab-bar-switch-to-recent-tab)

  (keymap-global-set "M-?" #'lspx-show-documentation-eldoc)

  (keymap-global-set "C-s" #'isearch-forward-regexp)
  (keymap-global-set "C-r" #'isearch-backward-regexp)
  (keymap-global-set "C-/" #'avy-goto-char-timer)
  (keymap-global-set "C-M-/" #'avy-isearch)
  
  
  (keymap-global-set "S-<return>" #'meow-open-below)
  (keymap-global-set "M-S-<return>" #'meow-open-above)
  
  (keymap-global-set "C-M-@" #'forward-char)
  (setq meow--kbd-forward-char "C-M-@")
  (keymap-global-set "C-M-$" #'kill-line)
  (setq meow--kbd-kill-line "C-M-$")

  (keymap-global-set "C-S-v" #'clipboard-yank)
  (keymap-global-set "C-<return>" #'mk/completion-at-point-with-tempel)
  (keymap-global-set "M-<return>" #'eldoc-box-help-at-point)
  ;; cape-dabbrev has been integrated into completion-at-point function already
  (keymap-global-set "M-/" #'dabbrev-completion)
  (keymap-global-set "C-S-f" #'cape-file))

(defun mk/keyBindingSetup ()
  ;; trivial
  (keymap-global-set "C-c :" #'eval-expression)
  (keymap-global-set "C-c \`" #'tab-switch)
  (keymap-global-set "C-c ;" #'async-shell-command)
  ;; (keymap-global-set "C-c SPC" #'which-key-show-major-mode)
  ;; (keymap-global-set "C-c SPC" #'consult-mode-command) ;; use l/g/m to filter
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
  
  (keymap-global-set "C-x v D" #'vc-dir)
  
  (mk/define&set-keymap
   "C-x v d" keymap/vc-diff 
   '(("d" . vc-diff)
     ("D" . vc-root-diff)
     ("e" . vc-ediff)
     ("v" . vc-version-diff)
     ("V" . vc-root-version-diff)
     ("E" . vc-version-ediff)))

  (mk/define&set-keymap
   "C-x v s" keymap/vc-share
   '(("s" . mk/git-link-clipboard)
     ("c" . mk/git-link-commit-clipboard)
     ("h" . mk/git-link-homepage-clipboard)))
  
  ;; diff (SPC x SPC d)
  (keymap-global-set "C-x d" #'diff)
  ;; smerge
  ;; use C-c ^
  ;; C-M- (SPC g) ===============================================================
  (keymap-global-set "C-M-l" #'recenter-top-bottom) ;; gl
  (keymap-global-set "C-M-s" #'scratch-buffer)      ;; gs

  ;; C-c SPC is preserved for `mk/set-shared-local-keymap' function

  ;; buffer(b)
  (mk/define&set-keymap
   "C-c b" keymap/buffer
   '(("b" . consult-buffer)
     ("o" . switch-to-buffer-other-window)
     ("c" . mk/switch-to-compilation-buffer)
     ("e" . mk/switch-to-eww-buffer)
     ("i" . ibuffer)
     ("r" . mk/reload-buffer)
     ("d" . kill-current-buffer)
     ("k" . mk/better-kill-buffer)
     ("K" . kill-buffer-and-window)
     ("R" . kill-matching-buffers-no-ask)))

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
     ("a" . lspx-execute-code-action)
     ("B" . ,(mk/define&set-keymap
              "C-c c D" keymap/code-debug
              '(("d" . mk/gf-debug)
                ("D" . mk/gdb-smart)
                ("v" . mk/debug-with-valgrind))))
     ("d" . lspx-find-definition-xref)
     ("D" . lspx-find-definition-other-window-xref)
     ;; eldoc: use ? (binding in meow.el)
     ("e" . lspx-show-buffer-errors)
     ("E" . lspx-show-project-errors)
     ("f" . ,(mk/define&set-keymap
              "C-c c f" keymap/code-format
              '(("f" . mk/refresh-file)
                ("b" . lspx-format-buffer)
                ("r" . lspx-format-region)
                ("F" . apheleia-format-buffer))))
     ("H" . lspx-toggle-inlay-hint)
     ;; ("i" . eglot-code-action-organize-imports)
     ("i" . lspx-find-implementation)
     ("l" . ,(mk/define&set-keymap
              "C-c c l" keymap/code-lsp
              '(("s" . lspx)
                ("k" . lspx-shutdown))))
     ("r" . lspx-find-references-xref)
     ("R" . lspx-rename)
     ("s" . mk/xref-stack-current-position)
     ("t" . lspx-find-type-definition)
     ("T" . lspx-find-type-definition-other-window)))

  ;; emoji(e)
  (mk/define&set-keymap
   "C-c e" keymap/emoji
   '(("e" . emoji-insert)
     ("r" . emoji-recent)))

  ;; file(f)
  (mk/define&set-keymap
   "C-c f" keymap/file
   `(("D" . mk/delete-file)
     ("f" . mk/smart-find-file)
     ("F" . mk/find-file-other-window)
     ("p" . project-find-file)
     ("P" . project-root-find-file)
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
     ("s" . dired-sidebar-toggle-sidebar)
     ("a" . org-agenda)
     ("A" . (lambda () (interactive) (find-file "~/notes/agenda.org")))
     ("b" . (lambda () (interactive) (find-file "~/Documents/meow_king.srht.site/content")))
     ("e" . mk/ace-ielm)
     ("E" . (lambda (arg) (interactive "P") (if arg (mk/browse-emacs-devel) (eww-list-bookmarks))))
     ("d" . mk/dired-jump)
     ("y" . ee-yazi)
     ("t" . eat)
     ("T" . mk/open-terminal)))

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
     ("s" . project-eshell)
     ("S" . project-async-shell-command)
     ("t" . tasks-project-run-last-cmd)
     ("T" . tasks-project-run)
     ("k" . project-kill-buffers)
     ("y" . ee-yazi-project)))

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
     ("p" . project-query-replace-regexp)
     ("R" . query-replace-regexp)
     ("g" . ,(mk/define&set-keymap
              "C-c r g" mk/replace-colorrg-keymap
              '(("i" . color-rg-search-input-in-current-file)
                ("I" . color-rg-search-input-in-project)
                ("b" . color-rg-search-symbol-in-current-file)  ; buffer
                ("d" . color-rg-search-symbol-with-type)  ; directory
                ("p" . color-rg-search-project-with-type)  ; project
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
                ("I" . info-apropos))))
     ("s" . isearch-forward-thing-at-point)
     ("l" . mk/better-consult-line)
     ("L" . mk/consult-line-other-window-no-jump)
     ;; ("S" . mk/better-consult-line-multi)
     ("f" . consult-focus-lines)
     ("h" . isearch-highlight-regexp)
     ("H" . unhighlight-regexp)
     ("c" . list-colors-display)
     ;; note you can input keys like `v' and add a space after it to filter
     ("i" . mk/better-consult-imenu)
     ("I" . imenu) 
     ("m" . mk/better-consult-man)
     ("M" . consult-global-mark)
     ;; NOTE: to post filter to filter group (i.e. filename in this case)
     ;; https://github.com/minad/consult/issues/799
     ("p" . mk/consult-ripgrep-file-type)
     ("b" . bookmark-jump)
     ("d" . dictionary-search)
     ("o" . consult-outline)
     ("r" . consult-register)
     ("R" . consult-register-store)
     ("t" . ,(mk/define&set-keymap
              "C-c s t" keymap/search-todo
              '(("t" . consult-todo)
                ("T" . consult-todo-all)
                ("d" . consult-todo-dir)
                ("T" . hl-todo-rgrep))))
     ("y" . consult-yank-from-kill-ring)))

  ;; toggle & tasks (t)
  (mk/define&set-keymap
   "C-c t" keymap/toggle
   '(("f" . mk/toggle-follow-mode)
     ("W" . whitespace-mode)
     ("r" . read-only-mode)
     ("R" . mk/global-read-only-mode)
     ("v" . view-mode)
     ("m" . meow-temp-normal)
     ("c" . rainbow-mode)
     ("s" . consult-theme)
     ("t" . tasks-run)))

  ;; window(w)
  (mk/define&set-keymap
   "C-c w" keymap/window
   '(("b" . mk/quit-other-window)
     ("c" . mk/ace-copy-window)
     ("f" . other-frame)
     ("w" . ace-window)
     ("W" . mk/ace-window-balance-window)
     ("t" . window-toggle-side-windows)
     ("T" . others/window-split-toggle)
     ("f" . fit-window-to-buffer)
     ("q" . delete-window)
     ("o" . ace-delete-window)
     ("O" . delete-other-windows)
     ("m" . maximize-window)
     ("M" . minimize-window)
     ("s" . ace-swap-window)
     ("B" . balance-windows)
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
                ("L" . mk/elpaca-write-lock-file)
                ("t" . elpaca-try)
                ("s" . elpaca-status)
                ("u" . mk/elpaca-update)
                ("U" . mk/elpaca-update-all))))))

  ;; trivial (z)
  (mk/define&set-keymap
   "C-c z" keymap/trivial
   `(("0".  mk/share-0x0)
     ("a" . aidermacs-transient-menu)
     ("g" . mk/better-gptel)
     ("A" . mk/better-align-regexp)
     ("c" . jinx-correct)
     ("C" . compile)
     ("e" . mk/trans-map/ekg-dispatch)
     ("L" . list-colors-display)
     ("j" . mk/run-just-command)
     ("k" . consult-kmacro)
     ("K" . kmacro-name-last-macro)
     ("d" . ediff-buffers)
     ("r" . restart-emacs)
     ;; ("s" . desktop-save-in-desktop-dir)
     ;; ("l" . desktop-load-file)
     ("n" . string-inflection-cycle)
     ("z" . ,(mk/define&set-keymap
              "C-c z z" keymap/trival/trival
              '(("q" . save-buffers-kill-emacs)
                ("Q" . kill-emacs)))))))

(with-eval-after-load 'emacs
  (mk/keyBindingSetup))

(add-hook
 'after-init-hook
 (lambda ()
   (keymap-global-set MK/LOCAL_KEY_MAP_GLOBAL_KEYBINDING #'mk/invoke-local-keymap)))

;; for tapping key which begins with a character other than SPC
;; so `meow-keypad' won't appear
(use-package which-key
  :ensure nil
  :config
  (setq which-key-idle-delay 0.5
        which-key-side-window-max-height 0.3)
  (which-key-mode))

;; Local Key Maps ==============================================================

(defun mk/invoke-local-keymap()
  "Used in global keymap for keybinding."
  (interactive)
  (when-let* ((ret (local-key-binding (kbd MK/LOCAL_KEY_MAP))))
    (cond
     ((commandp ret)
      (call-interactively ret))

     ((keymapp ret)
      (meow-keypad-start-with MK/LOCAL_KEY_MAP)))))

(defvar-keymap keymap/local/elisp
  "d" #'others/byte-compile-and-load-directory
  "e" #'others/eval-buffer)

(defvar-keymap keymap/local/cc
  "o" #'ff-find-other-file)

(defvar-keymap keymap/local/web
  "S" #'mk/live-web-start
  "T" #'mk/live-web-toggle
  "K" #'mk/live-web-kill
  "c" #'twind-insert-css-from-cheatsheet
  "t" #'twind-insert-class-from-cheatsheet)

(defvar-keymap keymap/local/eat
  "c" #'eat-semi-char-mode
  "C" #'eat-char-mode
  "l" #'eat-line-mode
  "e" #'eat-emacs-mode)

(defun mk/setup-local-keymap ()
  (add-hook
   'emacs-lisp-mode-hook
   (lambda ()
     (keymap-local-set MK/LOCAL_KEY_MAP keymap/local/elisp)))

  (mk/lib/batch-add-hook
   '(c-mode-hook c++-mode-hook)
   (lambda ()
     (keymap-local-set MK/LOCAL_KEY_MAP keymap/local/cc)))

  (add-hook
   'dired-mode-hook
   (lambda ()
     (keymap-local-set MK/LOCAL_KEY_MAP #'casual-dired-tmenu)))

  (add-hook
   'calc-mode-hook
   (lambda ()
     (keymap-local-set MK/LOCAL_KEY_MAP #'casual-calc-tmenu)))

  (add-hook
   'Info-mode-hook
   (lambda ()
     (keymap-local-set MK/LOCAL_KEY_MAP #'casual-info-tmenu)))

  (add-hook
   'ibuffer-mode-hook
   (lambda ()
     (keymap-local-set MK/LOCAL_KEY_MAP #'casual-ibuffer-tmenu)))

  (mk/lib/batch-add-hook
   '(js-mode-hook
     js-ts-mode-hook tsx-ts-mode-hook typescript-ts-mode-hook
     typescript-mode-hook web-mode-hook html-mode-hook mhtml-mode-hook
     vue-mode-hook css-mode-hook css-ts-mode)
   (lambda ()
     (keymap-local-set MK/LOCAL_KEY_MAP keymap/local/web)))

  (add-hook
   'eat-mode-hook
   (lambda ()
     (keymap-local-set MK/LOCAL_KEY_MAP keymap/local/eat))))

(mk/setup-local-keymap)

(provide 'init-key)

;;; init-key.el ends here
