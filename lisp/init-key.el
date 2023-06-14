;;; init-key.el --- keyBindings -*- lexical-binding: t -*-
;;; Commentary:
;; NOTE: global key bindings will be shadowwed by local keybindings
;;; Code:

;; remove existing keybindings
;; (defun remove-elisp-mode-local-keybindings ()
;;   (keymap-local-unset "C-c C-b" t))
;; 
;; (add-hook 'emacs-lisp-mode-hook 'remove-elisp-mode-local-keybindings)

;; native approach to surround text
(define-key global-map (kbd "M-[") 'insert-pair)
(define-key global-map (kbd "M-{") 'insert-pair)
(define-key global-map (kbd "M-\"") 'insert-pair)
(define-key global-map (kbd "M-<") 'insert-pair)
(define-key global-map (kbd "M-'") 'insert-pair)

;; Vim-like  keybinding
(progn
  (keymap-global-set "C-w" #'backward-kill-word)
  (keymap-global-set "C-M-#" #'kill-region)
  (setq meow--kbd-kill-region "C-M-#")
  
  (keymap-global-set "C-d" #'scroll-up)
  (keymap-global-set "C-M-!" #'delete-char)
  (setq meow--kbd-delete-char "C-M-!")
  
  (keymap-global-set "C-u" #'scroll-down))

(progn ;; insert mode (actually all mode)
  (keymap-global-set "C-M-@" #'forward-char)
  (setq meow--kbd-forward-char "C-M-@")
  (keymap-global-set "C-M-$" #'kill-line)
  (setq meow--kbd-kill-line "C-M-$")
  
  (keymap-global-set "M-<backspace>" #'mk/delete-symbol-at-point)
  (keymap-global-set "C-S-v" #'clipboard-yank)
  (keymap-global-set "C-<return>" #'mk/tempel-complete-or-next)
  (keymap-global-set "C-S-<return>" #'tempel-insert)
  (keymap-global-set "C-j" #'complete-symbol)
  (keymap-global-set "C-k" #'cape-dabbrev)
  (keymap-global-set "C-f" #'cape-file)
  (keymap-global-set "C-l" #'cape-line)
  (keymap-global-set "C-S-l" #'mk/cape-line-previous-buffer))

;; trivial
(defun mk/keyBindingSetup ()
  (keymap-global-set "C-c :" #'eval-expression)
  (keymap-global-set "C-c \`" #'eyebrowse-last-window-config)
  (keymap-global-set "C-c ;" #'with-editor-async-shell-command)
  (keymap-global-set "C-c ~" #'list-processes)
  (keymap-global-set "C-c SPC" #'execute-extended-command)
  ;; gl
  (keymap-global-set "C-M-l" #'recenter-top-bottom)
  ;; gs
  (keymap-global-set "C-M-s" #'scratch-buffer)
  ;; git(gg)
  (keymap-global-set "C-M-g" #'mk/project-git)
  ;; quit(q)
  (keymap-global-set "C-c q" #'kill-emacs)
  ;; z
  (which-key-add-key-based-replacements "C-c z" "trivial")
  (keymap-global-set "C-c z t" #'mk/translate)
  (keymap-global-set "C-c z c" #'jinx-correct)
  (keymap-global-set "C-c z p" #'mk/copy-path-smart)
  (keymap-global-set "C-c z s" #'desktop-save-in-desktop-dir)
  (keymap-global-set "C-c z l" #'desktop-load-file)

  ;; buffer(b)
  (which-key-add-key-based-replacements "C-c b" "buffer")
  (keymap-global-set "C-c b a" #'consult-buffer)
  (keymap-global-set "C-c b b" #'mk/consult-buffer-no-hidden)
  (keymap-global-set "C-c b c" #'mk/switch-to-compilation-buffer)
  (keymap-global-set "C-c b r" #'mk/reload-buffer)
  (keymap-global-set "C-c b B" #'consult-buffer)
  (keymap-global-set "C-c b p" #'mk/smart-buffer-switch-no-hidden)
  (keymap-global-set "C-c b P" #'mk/smart-buffer-switch)
  (keymap-global-set "C-c b d" #'mk/kill-buffer)
  (keymap-global-set "C-c b k" #'mk/kill-buffer)
  (keymap-global-set "C-c b K" #'mk/kill-all-buffers)

  ;; bookmark(B)
  (which-key-add-key-based-replacements "C-c B" "bookmark")
  (keymap-global-set "C-c B b" #'bookmark-jump)
  (keymap-global-set "C-c B c" #'bookmark-set)
  (keymap-global-set "C-c B d" #'bookmark-delete)

  ;; code(c)
  (which-key-add-key-based-replacements "C-c c" "code")
  (keymap-global-set "C-c c E" #'combobulate-envelop)
  (keymap-global-set "C-c c a" #'eglot-code-actions)
  (keymap-global-set "C-c c R" #'eglot-rename)
  (keymap-global-set "C-c c i" #'eglot-code-action-organize-imports)

  ;; (keymap-global-set "C-c c f" #'editorconfig-format-buffer)
  (which-key-add-key-based-replacements "C-c c f" "format buffer")
  (keymap-global-set "C-c c f F" #'apheleia-format-buffer)
  (keymap-global-set "C-c c f f" #'eglot-format)
  (keymap-global-set "C-c c p" #'citre-ace-peek)
  (keymap-global-set "C-c c P" #'citre-peek)

  (which-key-add-key-based-replacements "C-c c c" "color-rg")
  (keymap-global-set "C-c c c p" #'color-rg-search-project-with-type)
  (keymap-global-set "C-c c c b" #'color-rg-search-symbol-in-current-file)

  (keymap-global-set "C-c c D" #'eldoc)
  (keymap-global-set "C-c c d" #'xref-find-definitions)
  (keymap-global-set "C-c c r" #'xref-find-references)
  (keymap-global-set "C-c c j" #'citre-jump)
  (keymap-global-set "C-c c k" #'citre-jump-back)
  (keymap-global-set "C-c c u" #'citre-update-this-tags-file)
  (keymap-global-set "C-c c U" #'mk/update-all-tags)
  (keymap-global-set "C-c c e" #'consult-flymake) ;; show all errors
  (keymap-global-set "C-c c F" #'eglot-code-action-quickfix)

  (which-key-add-key-based-replacements "C-c c o" "other")
  (keymap-global-set "C-c c o c" #'citre-create-tags-file)
  (keymap-global-set "C-c c o e" #'citre-edit-tags-file-recipe)

  ;; Combobulate(C)
  ;;  (which-key-add-key-based-replacements "C-c C" "Combobulate")
  ;;  (keymap-global-set "C-c C" #'combobulate)

  ;; emoji(e)
  (which-key-add-key-based-replacements "C-h e" "emoji")
  (keymap-global-set "C-c e" #'emoji-insert)
  (keymap-global-set "C-h r" #'emoji-recent)

  ;; file(f)
  (which-key-add-key-based-replacements "C-c f" "file")
  (keymap-global-set "C-c f f" #'find-file)
  (keymap-global-set "C-c f D" #'mk/delete-file)
  (keymap-global-set "C-c f R" #'rename-visited-file)
  (keymap-global-set "C-c f r" #'recentf-open)
  (keymap-global-set "C-c f p" #'project-find-file)
  (keymap-global-set "C-c f z" #'zoxide-find-file)

  ;; fold(F)
  (which-key-add-key-based-replacements "C-c F" "fold")
  (keymap-global-set "C-c F o" #'hs-show-all)
  (keymap-global-set "C-c F O" #'outline-show-all)
  (keymap-global-set "C-c F c" #'hs-hide-all)
  (keymap-global-set "C-c F C" #'outline-show-only-headings)

  ;; easy GPG assistant
  (which-key-add-key-based-replacements "C-c G" "epa")
  (which-key-add-key-based-replacements "C-c G r" "region")
  (keymap-global-set "C-c G l" #'epa-list-keys)
  (keymap-global-set "C-c G r e" #'epa-encrypt-region)
  (keymap-global-set "C-c G r d" #'epa-decrypt-region)
  (keymap-global-set "C-c G r v" #'epa-verify-region)
  (which-key-add-key-based-replacements "C-c G f" "file")
  (keymap-global-set "C-c G f e" #'epa-encrypt-file)
  (keymap-global-set "C-c G f d" #'epa-decrypt-file)
  (keymap-global-set "C-c G f v" #'epa-verify-file)
  
  ;; help(h)
  ;; SPC h SPC <character>
  (keymap-global-set "C-h M" #'woman)

  ;; Hugo(H)
  (which-key-add-key-based-replacements "C-c H" "help")
  (keymap-global-set "C-c H h" #'mk/hugo/cd-project)
  (keymap-global-set "C-c H p" #'mk/hugo/toggle-preview)
  (keymap-global-set "C-c H t" #'mk/hugo/find-blog-using-tag-search)
  (keymap-global-set "C-c H d" #'mk/hugo/goto-draft)
  (keymap-global-set "C-c H b" #'mk/hugo/build)
  (keymap-global-set "C-c H f" #'mk/hugo/edit-or-create)

  ;; narrow(n)
  (which-key-add-key-based-replacements "C-c n" "narrow")
  (keymap-global-set "C-c n n" #'narrow-to-region)
  (keymap-global-set "C-c n p" #'narrow-to-page)
  (keymap-global-set "C-c n d" #'narrow-to-defun)
  (keymap-global-set "C-c n w" #'widen)

  ;; open(o)
  (which-key-add-key-based-replacements "C-c o" "open")
  (keymap-global-set "C-c o -" #'dired-jump)
  (keymap-global-set "C-c o =" #'project-dired)
  (keymap-global-set "C-c o s" #'dired-sidebar-toggle-sidebar)
  (keymap-global-set "C-c o a" '(lambda () (interactive) (find-file "~/notes/agenda.org")))
  (keymap-global-set "C-c o e" #'project-eshell)
  (keymap-global-set "C-c o r" '(lambda () (interactive) (find-file "~/projects/rust/LearningRustOS2023Record/README.org")))
  (keymap-global-set "C-c o d" #'dashboard-open)
  (keymap-global-set "C-c o D" #'mk/draw-diagram)
  (keymap-global-set "C-c o A" '(lambda () (interactive) (find-file "~/Documents/dotfiles/docs/unclassified.org")))
  (keymap-global-set "C-c o t" #'mk/open-terminal-smart)
  (keymap-global-set "C-c o T" #'mk/open-terminal-here)

  ;; project(p)
  (which-key-add-key-based-replacements "C-c p" "project")
  (keymap-global-set "C-c p A" #'project-remember-projects-under)
  (keymap-global-set "C-c p p" #'project-switch-project)
  (keymap-global-set "C-c p t" #'magit-todos-list)
  (keymap-global-set "C-c p e" #'flymake-show-project-diagnostics)
  (keymap-global-set "C-c p s" #'project-eshell)
  (keymap-global-set "C-c p S" #'project-async-shell-command)
  (keymap-global-set "C-c p k" #'project-kill-buffers)
  (keymap-global-set "C-c p c" #'mk/project-compile)
  (keymap-global-set "C-c p P" #'project-forget-project)

  ;; proxy(P)
  (which-key-add-key-based-replacements "C-c P" "proxy")
  (keymap-global-set "C-c P h" #'proxy-http-toggle)
  (keymap-global-set "C-c P H" #'proxy-http-show)
  (keymap-global-set "C-c P s" #'proxy-socks-toggle)
  (keymap-global-set "C-c P S" #'proxy-http-show)
  (keymap-global-set "C-c P p" #'org-tree-slide-mode)

  ;; search & replace (s)
  (which-key-add-key-based-replacements "C-c s" "search & replace")
  (which-key-add-key-based-replacements "C-c s a" "apropos")
  (keymap-global-set "C-c s a a" #'apropos)
  (keymap-global-set "C-c s a c" #'apropos-command)
  (keymap-global-set "C-c s a d" #'apropos-documentation)
  (keymap-global-set "C-c s a s" #'apropos-symbol)
  (keymap-global-set "C-c s a v" #'apropos-variable)
  (keymap-global-set "C-c s a f" #'apropos-function)
  (keymap-global-set "C-c s s" #'mk/better-consult-line)
  (keymap-global-set "C-c s c" #'list-colors-display)
  (keymap-global-set "C-c s i" #'consult-imenu)
  (keymap-global-set "C-c s I" #'consult-imenu-multi) ;; project wide
  (keymap-global-set "C-c s m" #'consult-global-mark)
  (keymap-global-set "C-c s M" #'consult-mark)
  (keymap-global-set "C-c s I" #'consult-info)
  (keymap-global-set "C-c s p" #'mk/better-consult-ripgrep)
  (keymap-global-set "C-c s P" #'mk/better-consult-git-grep)
  (keymap-global-set "C-c s b" #'consult-bookmark)
  (keymap-global-set "C-c s d" #'dictionary-search)
  (keymap-global-set "C-c s o" #'consult-outline)
  (keymap-global-set "C-c s O" #'mk/search-online)
  (keymap-global-set "C-c s t" #'hl-todo-occur)
  (keymap-global-set "C-c s T" #'hl-todo-rgrep)
  (keymap-global-set "C-c s r" #'query-replace)

  ;; straight (S)
  (which-key-add-key-based-replacements "C-c S" "straight")
  (keymap-global-set "C-c S r" #'straight-remove-unused-repos)
  (keymap-global-set "C-c S b" #'straight-rebuild-package)
  (keymap-global-set "C-c S B" #'straight-rebuild-all)
  (keymap-global-set "C-c S p" #'straight-pull-all)
  (keymap-global-set "C-c S f" #'straight-freeze-versions)

  ;; toggle (t)
  (which-key-add-key-based-replacements "C-c t" "toggle")
  (keymap-global-set "C-c t w" #'whitespace-mode)
  (keymap-global-set "C-c t h" #'mk/unhighlight-search)
  (keymap-global-set "C-c t t" #'consult-theme)

  ;; window(w)
  (which-key-add-key-based-replacements "C-c w" "window")
  (keymap-global-set "C-c w w" #'mk/ace-window-balance-window)
  (keymap-global-set "C-c w W" #'ace-window)
  (keymap-global-set "C-c w t" #'others/window-split-toggle)
  (keymap-global-set "C-c w m" #'delete-window)
  (keymap-global-set "C-c w o" #'delete-other-windows)
  (keymap-global-set "C-c w m" #'maximize-window)
  (keymap-global-set "C-c w M" #'minimize-window)
  (keymap-global-set "C-c w b" #'balance-windows)
  (keymap-global-set "C-c w +" #'maximize-window)
  (keymap-global-set "C-c w -" #'minimize-window)
  (keymap-global-set "C-c w =" #'balance-windows)
  (keymap-global-set "C-c w v" #'mk/split-window-vertically)
  (keymap-global-set "C-c w h" #'mk/split-window-horizontally)
  (keymap-global-set "C-c w q" #'evil-window-delete)
  (keymap-global-set "C-c w d" #'evil-window-delete)
  (keymap-global-set "C-c w L" #'buf-move-right)
  (keymap-global-set "C-c w H" #'buf-move-left)
  (keymap-global-set "C-c w J" #'buf-move-down)
  (keymap-global-set "C-c w K" #'buf-move-up))

(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  (setq which-key-side-window-max-height 0.3)
  :config
  (which-key-mode)
  (mk/keyBindingSetup))

;;; Trivial Functions =======================================
(defun mk/kill-buffer()
  "Kill buffer without deleting its window. (unlike evil-delete-buffer)"
  (interactive)
  (kill-buffer))

(defun mk/kill-all-buffers ()
  "Kill all buffers except *dashboard*."
  (interactive)
  (mapc 'kill-buffer (delq (get-buffer "*dashboard*") (buffer-list))))

(defun mk/open-terminal-smart()
  "Open terminal at project root if in a project, otherwise current folder."
  (interactive)
  (let (
         ;; (command-prefix "hyprctl dispatch exec '[workspace 1 slien; float; size 90% 40%; move 5% 58%]  kitty -d ")
         (command-prefix "kitty --class floating -d ")) ;; right parenthesis is needed to be added after concatance
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
         (command-prefix "kitty --class floating -d ")) ;; right parenthesis is needed to be added after concatance
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

(defun mk/consult-buffer-no-hidden()
  "Consult buffer without displaying hidden buffers."
  (interactive)
  (let* ((filters consult-buffer-filter)
          (consult-buffer-filter (push "\\`\\*.*\\*\\'" filters))) ;; local consult-buffer-filter
    (consult-buffer)))

(defun mk/consult-project-buffer-no-hidden()
  "Consult project buffer without displaying hidden buffers."
  (interactive)
  (let* ((filters consult-buffer-filter)
          (consult-buffer-filter (push "\\`\\*.*\\*\\'" filters))) ;; local consult-buffer-filter
    (consult-project-buffer)))

(defun mk/smart-buffer-switch-no-hidden ()
  "Smart buffer switch according to project existence without showing hidden buffers."
  (interactive)
  (if (project-current)
    (mk/consult-project-buffer-no-hidden)
    (mk/consult-buffer-no-hidden)))

(defun mk/smart-buffer-switch ()
  "Smart buffer switch according to project existence."
  (interactive)
  (if (project-current)
    (consult-project-buffer)
    (consult-buffer)))

(defun mk/tempel-complete-or-next ()
  "This function combines tempel-complete and tempel-next. Though it can also be achieved by
it can also be achieved by binding tempel-next in tempel-map to the same key as tempel-complete."
  (interactive)
  (if (not tempel--active)
    (call-interactively 'tempel-complete)
    (call-interactively 'tempel-next)))

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
  "Toggle window layout: vertical <-> horizontal"
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

(defun mk/project-compile()
  "Save & Compile Project."
  (interactive)
  (save-buffer)
  (project-compile))

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
        (call-process "firefox" nil 0 nil url)
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

(defun mk/better-consult-ripgrep()
  "Use symbol at point as the default input of `affe-grep'."
  (interactive)
  (consult-ripgrep nil (thing-at-point 'symbol)))

(defun mk/better-consult-git-grep()
  "Use symbol at point as the default input of `affe-grep'."
  (interactive)
  (consult-git-grep nil (thing-at-point 'symbol)))

(defun mk/better-consult-line()
  "Use symbol at point as the default input of `consult-line'."
  (interactive)
  (consult-line (thing-at-point 'symbol) nil))

(defun mk/update-all-tags()
  "Update both ctags and gtags file (for citre)."
  (interactive)
  (citre-update-this-tags-file)
  (citre-global-update-database))

;; Evil Related
;;
;; (defun mk/evil-search-symbol-forward ()
;;   "Symbol instead of word in normal state. This function aims to replace the default '*' binding in evil."
;;   (interactive)
;;   (cond
;;     ;; visual mode use package `evil-visualstar`
;;     ((evil-normal-state-p)
;;       (highlight-symbol-at-point)
;;       (evil-search-word-forward 1 (symbol-at-point)))))
;; 
;; (defun mk/evil-search-symbol-backward ()
;;   "Symbol instead of word in normal state. This function aims to replace the default '*' binding in evil."
;;   (interactive)
;;   (cond
;;     ;; visual mode use package `evil-visualstar`
;;     ((evil-normal-state-p)
;;       (highlight-symbol-at-point)
;;       (evil-search-word-backward 1 (symbol-at-point)))))
;; 
;; (defun mk/unhighlight-search()
;;   "Unhighlight all symbols highlighted by `highlight-symbol-at-point' in `mk/evil-search-symbol-*'"
;;   (interactive)
;;   (unhighlight-regexp t))
;; (defun djoyner/evil-shift-left-visual ()
;;   "Continuous evil shift-left."
;;   (interactive)
;;   (evil-shift-left (region-beginning) (region-end))
;;   (evil-normal-state)
;;   (evil-visual-restore))
;; 
;; (defun djoyner/evil-shift-right-visual ()
;;   "Continuous evil shift-right."
;;   (interactive)
;;   (evil-shift-right (region-beginning) (region-end))
;;   (evil-normal-state)
;;   (evil-visual-restore))

(provide 'init-key)
