;;; init-key.el --- Key Mappings Here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; universal-argument conflict with evil mode(c-u, scroll up half screen), so we change it
(global-set-key (kbd "C-M-u") 'universal-argument)

;;; Key Definers and Definer Macros =========================
;; define keybindings with ease
(use-package general
  ;; :demand t ;; use mapBegin! instead
  :config
  (general-evil-setup)
	
  (general-create-definer mk/leader-def
    :prefix "SPC"
		:non-normal-prefix "S-SPC")
	
  (general-create-definer mk/local-leader-def
		:prefix "SPC m"
		:non-normal-prefix "M-SPC m"))

(defmacro mapBegin! (&rest expression)
  "Used for defining keys. Keys are defined after package 'general' load up, so we
don't need to add ':demand t' keyword to 'use-package' declearation."
  `(use-package general
    :config
    ,@expression))


;;; Evil Collection ========================================= 
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit)) ;; <escape> to act like <C-g>
  :preface
  (setq
   evil-normal-state-cursor 'box
   evil-emacs-state-cursor  'box
   evil-insert-state-cursor 'bar
   evil-visual-state-cursor 'hollow)
  :init
  (setq
   evil-want-keybinding nil ;; evil-collection required
   evil-want-C-u-scroll t
   evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

;; @ integrate evil to wide other mode
(use-package evil-collection
  :after evil
  :custom (evil-collection-outline-bind-tab-p t)
  :config
  (evil-collection-init))

;; @ custom key to escape
(use-package evil-escape
  :after evil
  :config
  (evil-escape-mode t)
  (setq-default evil-escape-key-sequence "kk"))

;; @ add / deleta/ change surrounding notations
;; also works well with adding xml tags(can use class) and add funtion
;; like print("something")
;; visual state: S, and Sf (for add function), < (for add xml tags)
;; normal state:
;;   add: ysiw<new>
;;   change: cs<old><new>
;;   delete: cs<old>
(use-package evil-surround
  :after evil)

;; @ comment
(use-package evil-nerd-commenter
	:general ([remap comment-line] #'evilnc-comment-or-uncomment-lines))

;; @ indent
;; default keybindings: gl and gL
(use-package evil-lion
  :config
  (evil-lion-mode))

;; @ textobj
;; + arguments (function arguments, default style: (arg1, arg2) 
;; keybindings(incluing other useful function of this package are in the main keymaps
;; textobj: a
(use-package evil-args
	:config
	;; bind evil-args text objects
	(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
	(define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

;; + indentation
;; textobj: i, I, J
;; TODO: Current shorttage: cannot select objects inside arguments
;; maybe I can combine it with evil-args
(use-package evil-indent-plus
	:config
	(evil-indent-plus-default-bindings))

;; + xml attribute
;; textobj: x
;; example: uncomment the line below and relocate in 'class="meow"', type 'vix'
;; <html class="meow"></html>
(use-package exato)


;; @ textobj using tree-sitter
;; TODO more hack can be here
;; TODO wait for supporting emacs29 buildin tree-sitter
;; (use-package evil-textobj-tree-sitter
;; 	:config
;; 	;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
;; 	(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;; 	;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
;; 	(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))

;; @ navigatoin
;; Recommendation:
;;   - vertical movement: evil-easymotion
;;   - horizontal movement: evil-snipe
;;   - *, # visual area replacement: evil-easymotion
;; + gj,gk,ge,gb,g*,g#...
(use-package evil-easymotion
	:config
	(evilem-default-keybindings "g"))

;; + line: s(two characters search), f, t(makes f, t repectable)
(use-package evil-snipe
	:config
	(evil-snipe-mode +1)
	(evil-snipe-override-mode +1))

;;; Avy =====================================================
;; evil package has build avy
;; (use-package avy)

;;; Which-key ===============================================
(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  (setq which-key-side-window-max-height 0.3)
  :config
	(which-key-mode))

;;; select region ===========================================
(use-package expand-region)

;;; Main Key Mapping ========================================
(mapBegin!
 ;; continuous shift-right/left, cannot be defined in general map
 (define-key evil-visual-state-map (kbd ">") 'djoyner/evil-shift-right-visual)
 (define-key evil-visual-state-map (kbd "<") 'djoyner/evil-shift-left-visual)


 (general-unbind 'normal
	 "<"
	 ">"
	 "v")

 ;; @ normal map (no leader key
 (general-nmap 
	 ;; "." evil-repeat can be your friend
	 "TAB" #'evil-avy-goto-char-2
	 "gcc" #'evilnc-comment-or-uncomment-lines
	 "gg" #'evil-goto-first-line ;; deal with evil-easymotion keymap 
	 "L" #'sp-forward-sexp
	 "H" #'sp-backward-sexp
	 "M-v" #'er/expand-region
	 "C-." #'embark-act
	 "go"   #'evil-jump-out-args

	 ;; TODO this is temporary, wait for news from evil-textobj-tree-sitter
	 "[f" '(treesit-beginning-of-defun :which-key "func begin")
	 "]f" '(treesit-end-of-defun :which-key "func end")
	 "]a"   #'evil-forward-arg
	 "[a"   #'evil-backward-arg
	 "]s" #'sp-end-of-sexp
	 "[s" #'sp-beginning-of-sexp

	 ;; @ text-scale via init-base/default-text-scale
	 ;; C-- and C-= to change font size

	 "<<" #'evil-shift-left-line
	 ">>" #'evil-shift-right-line

	 "zk" #'sp-kill-hybrid-sexp
	 ;; the two works for the closest expression, for more specific control,
	 ;; use evil-surround instead (cs<paren> ds<paren>)
	 "zd" #'sp-splice-sexp
	 "zc" #'sp-rewrap-sexp
	 "zC" #'evil-close-fold

	 ">)" #'sp-forward-slurp-sexp
	 "<)" #'sp-forward-barf-sexp

	 ">(" #'sp-backward-barf-sexp
	 "<(" #'sp-backward-slurp-sexp)

 (general-mmap
	 "L" #'evil-forward-arg
	 "H" #'evil-backward-arg)

 ;; @ visual map (no leader key
 (general-vmap ;; visual
   "S" #'evil-surround-region
   "C-S-c" #'clipboard-kill-ring-save
	 "gc" #'evilnc-comment-or-uncomment-lines
	 "TAB" #'evil-avy-goto-char-2)

 ;; ((hllo world) meowking)
 ;; @ insert( map (no leader key
 (general-imap ;; insert
	 ;; eivl(vim) default C-o can be useful when moving in evil insert mode
   "C-S-v" #'clipboard-yank

	 ;; smartparen
	 "M-<backspace>" #'sp-backward-kill-sexp
	 "M-<return>" #'sp-up-sexp
	 "M-S-<return>" #'sp-backward-up-sexp
	 )

 ;; @ operation map (no leader key
 (general-omap
   "s" #'evil-surround-edit
   "S" #'evil-Surround-edit)


 ;; @ normal anad insert map (leader
 (mk/leader-def
	 :states '(normal visual)
	 :keymaps 'override
	 ":" #'(eval-expression :which-key "Eval")
	 "`" #'(eyebrowse-last-window-config :which-key "previous workspace")
	 ";" #'(with-editor-async-shell-command :which-key "run command")
	 "SPC" #'(execute-extended-command :which-key "M-x")

	 ;; @ workspace
	 ;; eyebrowse switch functions are also bounded to "M-<num>" in its use-package scope
	 "TAB" '(:ignore t :which-key "workspace")
	 "TAB TAB" '(eyebrowse-switch-to-window-config :which-key "switch")
	 "TAB 0" '(eyebrowse-switch-to-window-config-0 :which-key "w0")
	 "TAB 1" '(eyebrowse-switch-to-window-config-1 :which-key "w1")
	 "TAB 2" '(eyebrowse-switch-to-window-config-2 :which-key "w2")
	 "TAB 3" '(eyebrowse-switch-to-window-config-3 :which-key "w3")
	 "TAB 4" '(eyebrowse-switch-to-window-config-4 :which-key "w4")
	 "TAB 5" '(eyebrowse-switch-to-window-config-5 :which-key "w5")	 
	 "TAB s" '(desktop-save-in-desktop-dir :which-key "save session")
	 "TAB l" '(desktop-load-file :which-key "load session")

   ;; @ buffer
   "b"  '(:ignore t :which-key "Buffer & Bookmark")
   "bb" '(consult-project-buffer :which-key "switch")
   "bB" '(consult-buffer :which-key "all buffer")
	 "bd" '(evil-delete-buffer :which-key "delete")
	 "bk" '(evil-delete-buffer :which-key "delete")
	 "bK" '(mk/kill-all-buffers :which-key "delete")
	 
	 ;; @ bookmark
	 "B" '(:ignore t :which-key "Bookmark")
	 "Bb" '(bookmark-jump :which-key "switch")
	 "BB" '(bookmark-jump :which-key "switch")
	 "Bc" '(bookmark-set :which-key "create")
	 "Bd" '(bookmark-delete :which-key "delete")
	 "Bk" '(bookmark-delete :which-key "delete")
	 "BD" '(bookmark-delete :which-key "delete all")

	 ;; @ Code
	 "c" '(:ignore t :which-key "Code")
	 "ca" '(eglot-code-actions :which-key "action")
	 "cr" '(eglot-rename :which-key "rename")
	 "ci" '(eglot-code-action-organize-imports :which-key "format-buffer")
	 "cf" '(eglot-code-action-quickfix :which-key "format-buffer")
	 "cF" '(eglot-format-buffer :which-key "format-buffer")
	 "ce" '(consult-flymake :which-key "errors(b)")
	 "cd" '(xref-find-definitions :which-key "definitions")
	 "cr" '(xref-find-references :which-key "references")
	 "cD" '(eldoc-doc-buffer :which-key "doc") ;; also available as "K" in evil mode

   ;; @ file
   "f" '(:ignore t :which-key "File")
   "ff" '(find-file :which-key "find")
	 "fp" '(project-find-file :which-key "find@project")
	 "fr" '(consult-recent-file :which-key "recent")
	 "fz" '(zoxide-find-file :which-key "zoxide")

   ;; @ help
   "h" '(:ignore t :which-key "Help")
   "hf" #'describe-function
	 "hc" #'describe-char
	 "hF" #'describe-face
   "hk" #'describe-key
	 "hK" #'describe-keymap
   "ho" #'describe-symbol
   "hm" #'describe-mode
	 "hM" '(woman :which-key "man page")

	 ;; @ git
	 "g" '(:ignore t :which-key "Git")
	 "gg"  'magit-status
	 "gs"  'magit-status
   "gd"  'magit-diff-unstaged
   "gc"  'magit-branch-or-checkout
   "gl"  '(:ignore t :which-key "log")
   "glc" 'magit-log-current
   "glf" 'magit-log-buffer-file
   "gb"  'magit-branch
   "gP"  'magit-push-current
   "gp"  'magit-pull-branch
   "gf"  'magit-fetch
   "gF"  'magit-fetch-all
   "gr"  'magit-rebase

	 "o"  '(:ignore t :which-key "open")
	 "o-" '(dired-jump :which-key "dired here")
	 "o=" '(project-dired :which-key "project dired")

	 ;; @ project
	 "p" '(:ignore t :which-key "Project")
	 "pp" '(project-switch-project :which-key "switch")
	 "pt" '(magit-todos-list :which-key "todos")
	 "pe" '(flymake-show-project-diagnostics :which-key "errors(p)")
	 "pk" '(project-kill-buffers :which-key "kill buffers(p)")
	 "pc" #'(project-compile :whici-key "compile")
	 "ps" #'(project-async-shell-command :which-key "run command")
	 "pr" #'(project-forget-project :which-key "select to remove project")

	 ;; @ quit
	 "q" '(:ignore t :which-key "quit")
	 "qq" '(kill-emacs :which-key "kill emacs")

	 ;; @ search @ replace
	 "s" '(:ignore t :which-key "Search & Replace")
	 "ss" '(consult-line :which-key "content")
	 "si" '(consult-imenu :which-key "imenu")
	 "sp" '(consult-ripgrep :which-key "project content")
	 "sb" '(consult-bookmark :which-key "bookmark")
	 "so" '(consult-outline :which-key "outline")
	 "sr" '(:ignore t :which-key "color-rg") ; + color-rg
	 "srd" '(:ignore t :which-key "current directory")
	 "srdi" '(color-rg-search-input :which-key "input")
	 "srdp" '(color-rg-search-symbol :which-key "point")
	 "srde" '(color-rg-search-symbol-with-type :which-key "with file-extension")
	 "srp" '(:ignore t :which-key "current project")
	 "srpi" '(color-rg-search-input-in-project :which-key "input")
	 "srpp" '(color-rg-search-symbol-in-project :which-key "point")
	 "srpe" '(color-rg-search-project-with-type :which-key "with file-extension")
	 "srb" '(:ignore t :which-key "current buffer")
	 "srbi" '(color-rg-search-input-in-current-file :which-key "input")
	 "srbp" '(color-rg-search-symbol-in-current-file :which-key "point")


   ;; @ toggle
   "t" '(:ignore t :which-key "Toggle")
   "tw" 'whitespace-mode
   "tt" '(consult-theme :which-key "choose theme")
	 
   ;; @ window
   "w"  '(:ignore t :which-key "Window")
   "ww" #'(ace-window :which-key "ace-window")
	 "wo" #'(delete-other-windows :which-key "delte other window")
	 "wm" #'(maximize-window :which-key "maximize")
	 "wM" #'(minimize-window :which-key "minimize")
	 "wb" #'(balance-windows :which-key "balance")
	 "w+" #'(maximize-window :which-key "maximize")
	 "w-" #'(minimize-window :which-key "minimize")
	 "w=" #'(balance-windows :which-key "balance")
	 "wv" #'(split-window-vertically :which-key "split(v)")
	 "wh" #'(split-window-horizontally :which-key "split(h)")
	 "wq" #'(evil-window-delete :which-key "delete")
	 "wd" #'(evil-window-delete :which-key "delete")
	 "wL" #'(evil-window-right :which-key "go right")
	 "wH" #'(evil-window-left :which-key "go left")
	 "wJ" #'(evil-window-down :which-key "go down") 
	 "wK" #'(evil-window-up :which-key "go up")
	 )
 )

;;; Trivial Functions =======================================

(defun djoyner/evil-shift-left-visual ()
	"Continuous evil shift-left."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun djoyner/evil-shift-right-visual ()
	"Continuous evil shift-right."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun mk/kill-all-buffers ()
		"Kill all buffers except."
		(interactive)
		(mapc 'kill-buffer (delq (get-buffer "*dashboard*") (buffer-list))))

(provide 'init-key)
