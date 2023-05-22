;;; init-key.el --- Key Mappings Here -*- lexical-binding: t -*-
;;; Commentary:
;; Main keys here. Specific mode keybindings are not included.
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
  (setq  evil-normal-state-cursor  '("DodgerBlue" box)
    evil-insert-state-cursor  '("IndianRed1" (bar . 2))
    evil-emacs-state-cursor   '("SkyBlue2" box)
    evil-replace-state-cursor '("Chocolate" (hbar . 2))
    evil-visual-state-cursor  '("DarkViolet" (hollow . 2))
    evil-motion-state-cursor  '("Plum3" box))
  :init
  (setq
    evil-want-keybinding nil ;; evil-collection required
    evil-want-C-u-scroll t
    evil-move-beyond-eol t
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
  (setq-default evil-escape-key-sequence "kk"
    evil-escape-excluded-states '(visual)))

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
;; default keybindings: gl and gl
(use-package evil-lion
  :config
  (evil-lion-mode))

(use-package evil-visualstar
  :hook ((after-init . global-evil-visualstar-mode)))

;; @ textobj
;; + cleverparens
;; textobjs:
;;   f: From, nearest delimeter
;;   c: comment, vic(delimeter detect), vac(all)
;;   d: defun
;;   W: smart word(aware delimeter)
;; Keys:
;;   H, L: move backward/forward by sexp
;;   M-h/l: Move to the beginning/end of a top-level form
;;   ()[]{} many more
;;   clever D, Y, C, x, ... many more
;; https://github.com/emacs-evil/evil-cleverparens
;; (use-package evil-cleverparens
;;   :hook ((emacs-lisp-mode) . evil-cleverparens-mode)
;;   :init
;;   (setq evil-cleverparens-use-s-and-S nil) ;; use evil-snipe instead
;;   :config
;;   (setq evil-cleverparens-use-additional-bindings nil))

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
;; (use-package evil-easymotion
;; 	:config
;; 	(evilem-default-keybindings "g"))

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

;;; Mini buffer enhance =====================================
(defun mk/insert-clipboard-in-minibuffer ()
  (interactive)
  (insert (gui-get-selection 'CLIPBOARD)))
(define-key minibuffer-local-map (kbd "C-y") 'mk/insert-clipboard-in-minibuffer)
(define-key minibuffer-local-map (kbd "C-S-v") 'mk/insert-clipboard-in-minibuffer)

;;; Main Key Mapping ========================================
(mapBegin!
  ;; continuous shift-right/left, cannot be defined in general map
  (define-key evil-visual-state-map (kbd ">") 'djoyner/evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'djoyner/evil-shift-left-visual)

  ;; mainly for rebinding these keys
  (general-unbind 'normal
	  "<"
	  ">"
	  "v"
    "TAB")

  (general-mmap
	  "L" '(lambda () (interactive) (call-interactively #'forward-sexp))
	  "H" '(lambda () (interactive) (call-interactively #'backward-sexp)))

  ;; @ normal map (no leader key
  (general-nmap 
    :keymaps 'override
    ;; to better move around in long wrapped lines
    "j" '(lambda () (interactive) (call-interactively #'next-line)) 
    "k" '(lambda () (interactive) (call-interactively #'previous-line)) 

	  ;; "." evil-repeat can be your friend
	  ;; "TAB" #'evil-avy-goto-char-2 ;; FIXME no use?
	  "gcc" #'evilnc-comment-or-uncomment-lines
	  "gg" #'evil-goto-first-line ;; deal with evil-easymotion keymap 
    "," #'evil-avy-goto-word-0
    "." #'evil-avy-goto-word-1
	  "M-h" #'sp-beginning-of-sexp
    "M-l" #'sp-end-of-sexp
	  "M-v" #'er/expand-region
	  "C-." #'embark-act
    "C-i" #'evil-jump-forward
    "C-o" #'evil-jump-backward
	  "go"   #'evil-jump-out-args
    "C-S-v" #'clipboard-yank

    "*" #'mk/evil-search-symbol-forward
    "C-*" #'mk/unhighlight-search
    "#" #'mk/evil-search-symbol-backward

    ;; TODO this is temporary, wait for news from evil-textobj-tree-sitter
	  "[f" '(treesit-beginning-of-defun :which-key "func begin")
	  "]f" '(treesit-end-of-defun :which-key "func end")
	  "]a"  #'evil-forward-arg
	  "[a"  #'evil-backward-arg
	  "]s"  #'forward-sexp
	  "[s"  #'backward-sexp
	  "M-<backspace>" #'(lambda () (interactive) (progn (mk/delete-symbol-at-point) (evil-insert-state)) )

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

	  ;; ">)" #'sp-forward-slurp-sexp
	  ;; "<)" #'sp-forward-barf-sexp

	  ;; ">(" #'sp-backward-barf-sexp
	  ;; "<(" #'sp-backward-slurp-sexp
    )

  ;; @ visual map (no leader key
  (general-vmap ;; visual
    ;; to better move around in long wrapped lines
    "C-j" '(lambda () (interactive) (call-interactively #'next-line)) 
    "C-k" '(lambda () (interactive) (call-interactively #'previous-line)) 

    "S" #'evil-surround-region
    "C-S-c" #'clipboard-kill-ring-save
	  "gc" #'evilnc-comment-or-uncomment-lines
	  "gg" #'evil-goto-first-line ;; deal with evil-easymotion keymap 
	  "TAB" #'evil-avy-goto-char-2)

  ;; ((hllo world) meowking)
  ;; @ insert( map (no leader key
  (general-imap ;; insert
	  ;; eivl(vim) default C-o can be useful when moving in evil insert mode
    ;; emacs keybindings like C-f, C-b, M-f, M-b are also useful.
    "C-S-v" #'clipboard-yank

	  ;; smartparen
	  "M-<backspace>" #'mk/delete-symbol-at-point
    ;; "S-<return>" #'ispell-complete-word
    ;; "C-j" #'lsp-bridge-popup-complete-menu
    "C-j" #'complete-symbol
    "C-k" #'cape-dabbrev
    "C-f" #'cape-file
    "C-l" #'cape-line
    "C-S-l" #'mk/cape-line-previous-buffer
	  ;; "M-<return>" #'sp-up-sexp
	  ;; "M-S-<return>" #'sp-backward-up-sexp

	  "C-<return>" #'mk/tempel-complete-or-next
    "C-S-<return>" #'tempel-insert)

  ;; @ operation map (no leader key
  (general-omap
    "s" #'evil-surround-edit
    "S" #'evil-Surround-edit)

  (general-define-key
    :keymaps 'isearch-mode-map
    "C-n" #'isearch-repeat-forward
    "C-j" #'isearch-repeat-forward
    "C-p" #'isearch-repeat-backward
    "C-k" #'isearch-repeat-backward)

  ;; @ normal anad insert map (leader
  (mk/leader-def
	  :states '(normal visual)
    :keymaps 'override
	  ":" #'(eval-expression :which-key "Eval")
	  "`" #'(eyebrowse-last-window-config :which-key "previous workspace")
	  ";" #'(with-editor-async-shell-command :which-key "run command")
    "~" #'(list-processes :which-key "list processes")
    "/" #'(evil-avy-goto-word-0 :which-key "avy")
	  "SPC" #'(execute-extended-command :which-key "M-x")
	  "0" '(eyebrowse-last-window-config :which-key "last workspace")
	  "1" '(eyebrowse-switch-to-window-config-1 :which-key "w1")
	  "2" '(eyebrowse-switch-to-window-config-2 :which-key "w2")
	  "3" '(eyebrowse-switch-to-window-config-3 :which-key "w3")
	  "4" '(eyebrowse-switch-to-window-config-4 :which-key "w4")
	  "5" '(eyebrowse-switch-to-window-config-5 :which-key "w5")

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

    ;; @ ai
    "a" '(:ignore t :which-key "AI")
    "at" #'(mind-wave-translate-to-english :which-key "translate") ;; note: can also translate Chinese
    "ap" #'(mind-wave-proofreading-doc :which-key "proofreading")
    "ae" #'(:ignore t :which-key "explain")
    "aew" #'(mind-wave-explain-word :which-key "word")
    "aec" #'(mind-wave-explain-code :which-key "code")
    "aeC" #'(mind-wave-comment-code :which-key "comment to explain")
    "ac" #'(:ignore t :which-key "refractory")
    "aca" #'(mind-wave-refactory-code :which-key "refractory")
    "acc" #'(mind-wave-generate-commit-name :whici-key "generate commit name")

    ;; @ buffer
    "b"  '(:ignore t :which-key "Buffer & Bookmark")
    "bb" '(mk/consult-buffer-no-hidden :which-key "all buffer")
    "bc" '(mk/switch-to-compilation-buffer :which-key "*compilation*")
    "br" #'(mk/reload-buffer :which-key "reload")
    "bB" '(consult-buffer :which-key "buffer(all)")
    "bp" '(mk/smart-buffer-switch-no-hidden :which-key "switch(p)")
    "bP" '(mk/smart-buffer-switch :which-key "switch(p,all)")
    "ba" '(consult-buffer :which-key "all buffer")
	  "bd" '(mk/kill-buffer :which-key "delete")
	  "bk" '(mk/kill-buffer :which-key "delete")
	  "bK" '(mk/kill-all-buffers :which-key "kill all")
	  
	  ;; @ bookmark
	  "B" '(:ignore t :which-key "Bookmark")
	  "Bb" '(bookmark-jump :which-key "switch")
	  "BB" '(bookmark-jump :which-key "switch")
	  "Bc" '(bookmark-set :which-key "create")
	  "Bd" '(bookmark-delete :which-key "delete")
	  "Bk" '(bookmark-delete :which-key "delete")
	  "BD" '(bookmark-delete :which-key "delete all")

	  ;; @ Code
	  "c"  '(:ignore t :which-key "Code")
    ;; citre & eglot
	  "ca" '(eglot-code-actions :which-key "lsp action")
	  "cR" '(eglot-rename :which-key "lsp rename")
	  "ci" '(eglot-code-action-organize-imports :which-key "lsp organize import")

	  "cf" '(editorconfig-format-buffer :which-key "format buffer")
	  ;; "cD" '(eldoc-doc-buffer :which-key "doc") ;; also available as "K" in evil mode
    "cp" #'(citre-ace-peek :which-key "ace peek") ;; M-n M-p to scroll
	  "cP" #'(citre-peek :which-key "peek")

    "cc" '(:ignore t :which-key "color-rg")
	  "ccp" #'(color-rg-search-project-with-type :which-key "rg(p)")
    "ccb" #'(color-rg-search-symbol-in-current-file :which-key "rg(b)")

    "cd" #'(xref-find-definitions :which-key "lsp definitions") 
	  "cr" '(xref-find-references :which-key "references");; xref-find-references may be occupied by citre's backend
    ;; "cr" #'(citre-jump-to-reference :which-key "reference")
    "cj" #'(citre-jump :which-key "jump")
    "ck" #'(citre-jump-back :which-key "jump back")
    "cu" #'(citre-update-this-tags-file :which-key "update tags")
    "cU" #'(mk/update-all-tags :which-key "update gtags")

	  "ce" '(consult-flymake :which-key "errors(b)")
	  "cF" '(eglot-code-action-quickfix :which-key "quick fix")

    "co" '(:ignore t :which-key "other") ;; not frequently used functions
    "coc" #'(citre-create-tags-file :which-key "create tags")
    "coe" #'(citre-edit-tags-file-recipe :which-key "edit tags recipe")

    ;; lsp-bridge
	  ;; "ca" '(lsp-bridge-code-action :which-key "action")
	  ;; "cr" '(lsp-bridge-rename :which-key "rename")
	  ;; "cR" '(color-rg-search-project-with-type :which-key "references") ;; lsp-bridge-find-references cannot popup
	  ;; "ci" '(lsp-bridge-workspace-list-symbols :which-key "import organization")
	  ;; "cf" '(editorconfig-format-buffer :which-key "format buffer")
	  ;; ;; "cF" '(eglot-code-action-quickfix :which-key "quick fix")
	  ;; "ce" '(lsp-bridge-diagnostic-jump-next :which-key "errors(b)")
	  ;; "cd" '(lsp-bridge-find-def :which-key "definitions")
	  ;; "cD" '(lsp-bridge-popup-documentation :which-key "doc") ;; also available as "K" in evil mode

    ;; @ file
    "f" '(:ignore t :which-key "File")
    "ff" #'(find-file :which-key "find file")
    "fF" #'(affe-find :which-key "fuzzy find")
    "fD" #'(mk/delete-file :which-key "delete")
    "fR" #'(rename-visited-file :which-key "rename")
	  "fp" #'(project-find-file :which-key "find@project")
	  "fr" #'(consult-recent-file :which-key "recent")
	  "fz" #'(zoxide-find-file :which-key "zoxide")

    ;; @ fold
    ;; other fold method is integrated into evil's buildin 'z'
    "F" '(:ignore t :which-key "Fold & Outline")
    "Fo" #'(hs-show-all :which-key "open all folds")
    "FO" #'(outline-show-all :which-key "open all headings")
    "Fc" #'(hs-hide-all :which-key "hide all folds")
    "FC" #'(outline-show-only-headings :which-key "show only headings")

    ;; @ help
    "h" '(:ignore t :which-key "Help")
    "hf" #'(describe-function :which-key "function")
    "hi" #'(info :which-key "info")
    "hn" #'(view-emacs-news :which-key "news")
	  "hc" #'(describe-char :which-key "char")
	  "hF" #'(describe-face :which-key "face")
    "hk" #'(describe-key :which-key "key")
	  "hK" #'(describe-keymap :which-key "keymap")
    "ho" #'(describe-symbol :which-key "symbol")
    "hv" #'(describe-variable :which-key "variable")
    "hm" #'(describe-mode :which-key "mode")
	  "hM" #'(woman :which-key "man page")

    ;; @ insert
    "i" '(:ignore t :which-key "insert")
    "ie" '(:ignore t :which-key "emoji")
    "iei" '(emoji-insert :which-key "insert")
    "ier" '(emoji-insert :which-key "recent")

    ;; @ hugo
    "H" '(:ignore t :which-key "hugo")
    "Hh" #'(mk/hugo/cd-project :which-key "switch to blog project")
    "Hp" #'(mk/hugo/toggle-preview :which-key "toggle preview")
    "Ht" #'(mk/hugo/find-blog-using-tag-search :which-key "tag search") 
    "Hd" #'(mk/hugo/goto-draft :which-key "goto draft")
    "Hb" #'(mk/hugo/build :which-key "build")
    "Hf" #'(mk/hugo/edit-or-create :which-key "edit or create")

	  ;; @ git
	  "g" '(:ignore t :which-key "Git")
	  ;; "gg"  #'(magit-status :which-key "status")
	  "gg"  #'(mk/project-git :which-key "status")
    "gi" #'(magit-init :which-key "init")
	  "gs"  #'(magit-status :which-key "status")
    "gd"  #'(magit-diff :which-key "diff(staged)")
    "gc"  #'(magit-branch-or-checkout :which-key "branch or checkout")
    "gl"  '(:ignore t :which-key "log")
    "glc" #'(magit-log-current :which-key "log current")
    "glf" #'(magit-log-buffer-file :which-key "log buffer file")
    "gll" #'(magit-log :which-key "log")
    "gb"  #'(magit-branch :which-key "branch")
    "gB"  #'(magit-blame :which-key "blame")
    "gP"  #'(magit-push-current :which-key "push")
    "gp"  #'(magit-pull-branch :which-key "pull")
    "gf"  #'(magit-fetch :which-key "fetch")
    "gF"  #'(magit-fetch-all :which-key "fet all")
    "gr"  #'(magit-rebase :which-key "rebase")

    ;; @ open
	  "o"  '(:ignore t :which-key "open")
	  "o-" #'(dired-jump :which-key "dired here")
	  "o=" #'(project-dired :which-key "project dired")
    "os" #'(dired-sidebar-toggle-sidebar :which-key "toggle sidebar")
	  "oa" #'((lambda () (interactive) (find-file "~/notes/agenda.org")) :which-key "todos")
    "or" #'((lambda () (interactive) (find-file "~/projects/rust/LearningRustOS2023Record/README.org" :which-key "os learning record")))
    "od" #'(dashboard-open :which-key "open dashboard")
    "oD" #'(mk/draw-diagram :which-key "draw diagram")
    "oA" #'((lambda () (interactive) (find-file "~/Documents/dotfiles/docs/unclassified.org")) :which-key "application record")
	  "ot" #'(mk/open-terminal-smart :which-key "open terminal(p)")
	  "oT" #'(mk/open-terminal-here :which-key "open terminal(b)")

	  ;; @ project
	  "p" '(:ignore t :which-key "Project")
    "pA" #'(project-remember-projects-under :which-key "add p")
	  "pp" '(project-switch-project :which-key "switch")
	  "pt" '(magit-todos-list :which-key "todos")
	  "pe" '(flymake-show-project-diagnostics :which-key "errors(p)")
    "ps" #'(project-shell :which-key "project-shell")
	  "pk" '(project-kill-buffers :which-key "kill buffers(p)")
	  "pc" #'(mk/project-compile :whici-key "compile")
	  "pr" #'(project-async-shell-command :which-key "run command")
	  "pR" #'(project-forget-project :which-key "remove p")

    ;; @ Proxy & Presentation
	  "P" '(:ignore t :which-key: "Proxy & Presentation")
    "Ph" #'(proxy-http-toggle :which-key "toggle HTTP proxy")
    "PH" #'(proxy-http-show :which-key "show HTTP proxy")
    "Ps" #'(proxy-socks-toggle :which-key "toggle socks proxy")
    "PS" #'(proxy-http-show :which-key "show socks proxy")
	  "Pp" #'(org-tree-slide-mode :which-key "org-tree-slide")
	  "Pk" #'(keycast-header-line-mode :which-key "key(header line)")
	  "Pl" #'(keycast-header-line-mode :which-key "key(other frame)")

	  ;; @ quit
	  "q" '(:ignore t :which-key "quit")
	  "qq" '(kill-emacs :which-key "kill emacs")

	  ;; @ search @ replace
	  "s" '(:ignore t :which-key "Search & Replace")
    "sa" '(:igore t :which-key "apropos")
    "saa" '(apropos :which-key "apropos")
    "sac" '(apropos-command :which-key "command")
    "sad" '(apropos-documentation :which-key "documentation")
    "sas" '(apropos-symbol :which-key "symbol")
    "sav" '(apropos-variable :which-key "variable")
    "saf" '(apropos-function :which-key "function")
	  "ss" '(mk/better-consult-line :which-key "content")
    "sc" '(list-colors-display :which-key "colors")
	  "si" '(consult-imenu :which-key "imenu")
    "sI" '(consult-info :which-key "info")
	  "sp" '(mk/better-affe-grep :which-key "affe-grep(p)")
	  "sP" '(consult-ripgrep :which-key "consult-ripgrep(p)")
	  "sb" '(consult-bookmark :which-key "bookmark")
    "sd" '(dictionary-search :which-key "dictionary")
	  "so" '(consult-outline :which-key "outline")
    "sO" '(mk/search-online :which-key "online search")
    "st" #'(hl-todo-occur :which-key "todo(b)")
    "sT" #'(hl-todo-rgrep :which-key "todo(p)")
    "sr" '(query-replace :which-key "query replace")
	  ;; "sr" '(:ignore t :which-key "color-rg") ; + color-rg
	  ;; "srd" '(:ignore t :which-key "current directory")
	  ;; "srdi" '(color-rg-search-input :which-key "input")
	  ;; "srdp" '(color-rg-search-symbol :which-key "point")
	  ;; "srde" '(color-rg-search-symbol-with-type :which-key "with file-extension")
	  ;; "srdd" '(color-rg-search-symbol :which-key "point")
	  ;; "srp" '(:ignore t :which-key "current project")
	  ;; "srpi" '(color-rg-search-input-in-project :which-key "input")
	  ;; "srpp" '(color-rg-search-symbol-in-project :which-key "point")
	  ;; "srpe" '(color-rg-search-project-with-type :which-key "with file-extension")
	  ;; "srb" '(:ignore t :which-key "current buffer")
	  ;; "srbi" '(color-rg-search-input-in-current-file :which-key "input")
	  ;; "srbp" '(color-rg-search-symbol-in-current-file :which-key "point")
	  ;; "srbb" '(color-rg-search-symbol-in-current-file :which-key "point")

	  "S" '(:ignore t :which-key "straight")
	  "Sr" #'(straight-remove-unused-repos :which-key "remove unused")
	  "Sp" #'(straight-pull-all :which-key "pull all")
    ;; lockfile: .local/straight/versions/default.el
    "Sl" #'(straight-freeze-versions :which-key "lock version")

    ;; @ toggle
    "t" '(:ignore t :which-key "Toggle")
    "tw" 'whitespace-mode
    "th" '(mk/unhighlight-search :which-key "unhighlight search")
    "tt" '(consult-theme :which-key "choose theme")
	  
    ;; @ window
    "w"  '(:ignore t :which-key "Window")
    "ww" #'(mk/ace-window-balance-window :which-key "ace-window")
    "wW" #'(ace-window :which-key "ace-window")
    "wt" #'(others/window-split-toggle :which-key "split layout toggle")
	  "wo" #'(delete-other-windows :which-key "delte other window")
	  "wm" #'(maximize-window :which-key "maximize")
	  "wM" #'(minimize-window :which-key "minimize")
	  "wb" #'(balance-windows :which-key "balance")
	  "w+" #'(maximize-window :which-key "maximize")
	  "w-" #'(minimize-window :which-key "minimize")
	  "w=" #'(balance-windows :which-key "balance")
	  "wv" #'(mk/split-window-vertically :which-key "split(v)")
	  "wh" #'(mk/split-window-horizontally :which-key "split(h)")
	  "wq" #'(evil-window-delete :which-key "delete")
	  "wd" #'(evil-window-delete :which-key "delete")
	  ;; "wl" #'(evil-window-right :which-key "go right")
	  ;; "wh" #'(evil-window-left :which-key "go left")
	  ;; "wj" #'(evil-window-down :which-key "go down") 
	  ;; "wk" #'(evil-window-up :which-key "go up")
	  "wL" #'(buf-move-right :which-key "move right")
	  "wH" #'(buf-move-left :which-key "move left")
	  "wJ" #'(buf-move-down :which-key "move down")
	  "wK" #'(buf-move-up :which-key "move up")

	  "x" #'(scratch-buffer :which-key "scratch")

	  "z" #'(:ignore t :which-key "trivial")
    "zj" #'(evil-collection-consult-jump-list :which-key "jump his")
	  "zt" #'(mk/translate :which-key "translate")
    ;; "zc" #'(ispell-word :which-key "correct word")
    ;; "zc" #'(jit-spell-correct-word :which-key "correct misspelling")
    "zc" #'(jinx-correct :which-key "correct word")
    "zp" #'(mk/copy-path-smart :which-key "copy path")

	  "m" #'(:ignore t :which-key "local"))

  ;; info mode map
  (general-nmap
    :keymaps 'Info-mode-map 
    "P" #'(Info-prev :which-key "prev")
    "N" #'(Info-next :which-key "next")
    "u" #'(Info-up :which-key "up")
    "t" #'(Info-toc :which-key "toc")))

;;; Trivial Functions =======================================
(defun mk/kill-buffer()
  "Kill buffer without deleting its window. (unlike evil-delete-buffer)"
  (interactive)
  (kill-buffer))

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

(defun mk/evil-search-symbol-forward ()
  "Symbol instead of word in normal state. This function aims to replace the default '*' binding in evil."
  (interactive)
  (cond
    ;; visual mode use package `evil-visualstar`
    ((evil-normal-state-p)
      (highlight-symbol-at-point)
      (evil-search-word-forward 1 (symbol-at-point)))))

(defun mk/evil-search-symbol-backward ()
  "Symbol instead of word in normal state. This function aims to replace the default '*' binding in evil."
  (interactive)
  (cond
    ;; visual mode use package `evil-visualstar`
    ((evil-normal-state-p)
      (highlight-symbol-at-point)
      (evil-search-word-backward 1 (symbol-at-point)))))

(defun mk/unhighlight-search()
  "Unhighlight all symbols highlighted by `highlight-symbol-at-point' in `mk/evil-search-symbol-*'"
  (interactive)
  (unhighlight-regexp t))

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

(defun mk/better-affe-grep()
  "Use symbol at point as the default input of `affe-grep'."
  (interactive)
  (affe-grep nil (thing-at-point 'symbol)))

(defun mk/better-consult-line()
  "Use symbol at point as the default input of `consult-line'."
  (interactive)
  (consult-line (thing-at-point 'symbol) nil))

(defun mk/update-all-tags()
  "Update both ctags and gtags file (for citre)."
  (interactive)
  (citre-update-this-tags-file)
  (citre-global-update-database))

(provide 'init-key)
