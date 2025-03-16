;;; my-misc.el --- miscellaneous configuration -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Meow King <mr.meowking@anche.no>

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; miscellaneous configuration

;;; Code:

;; show key, for presentation usage
;; (use-package keycast
;;   :after (doom-modeline dashboard))

;; license
(use-package lice
  :ensure (:type git :host github :repo "buzztaiki/lice-el"))

(with-eval-after-load 'eww
  (setq shr-use-fonts nil)
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

(with-eval-after-load 'info
  (keymap-set Info-mode-map "h" #'backward-char)
  (keymap-set Info-mode-map "l" #'forward-char)
  (keymap-set Info-mode-map "H" #'Info-history-back)
  (keymap-set Info-mode-map "L" #'Info-history-forward))

(with-eval-after-load 'cus-edit
  (keymap-set Custom-mode-map "<return>" #'widget-button-press))

;; @ colorful compilation
(use-package ansi-color  ; Emacs 28 builtin
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Note that the `copmile' command is adviced in `my-advice'.
(defun mk/set-compile-command (&optional major-mode-first)
  "Define compile command for every mode.
MAJOR-MODE-FIRST: respect more about major mode configuration than project
configuration (like Makefile)."
  (interactive (list t))
  ;; to make sure buffer has corresponding file, and prevent
  ;; error when loading lisp-interaction-mode at emacs startup
  (when buffer-file-name
    (setq-local
     compile-command
     (let* ((base-path ;; project root when in a project; current directory when not
             (if (project-current)
                 (project-root (project-current)) ;; have problem with git submodule
               (file-name-directory buffer-file-name)))
            (file-extension (file-name-extension buffer-file-name))
            (file-name (file-name-nondirectory buffer-file-name))
            (relative-file-name (file-relative-name buffer-file-name base-path))
            (relative-bare-file-name (file-name-sans-extension relative-file-name))
            (makefile-exist (file-exists-p (expand-file-name "Makefile" base-path)))
            (justfile-exist (file-exists-p (expand-file-name "justfile" base-path)))
            (gradlew-project (file-exists-p (expand-file-name "gradlew" base-path))))
       (cond
        ((and (not major-mode-first) makefile-exist)
         "make run")
        ((and (not major-mode-first) justfile-exist)
         "just run")  ; just --list
        (gradlew-project
         "./gradlew run")
        ;; rust
        ((or (eq major-mode 'rust-mode) (eq major-mode 'rustic-mode) (eq major-mode 'rust-ts-mode)) 
         "cargo run")
        ;; cpp
        ((or (eq major-mode 'c++-mode) (eq major-mode 'c++-ts-mode))
         (concat "g++ -Wall -std=c++17 " relative-file-name " -o " relative-bare-file-name " && ./" relative-bare-file-name))
        ;; c
        ((or (eq major-mode 'c-mode) (eq major-mode 'c-ts-mode))
         ;; (concat "make " relative-bare-file-name " && ./" relative-bare-file-name)
         (concat "gcc -Wall -Wpedantic " relative-file-name " -o " relative-bare-file-name " && ./" relative-bare-file-name))
        ;; java
        ((or (eq major-mode 'java-mode) (eq major-mode 'java-ts-mode))
         (concat "./gradlew run"))
        ;; kotlin
        ((eq major-mode 'kotlin-ts-mode)
         (concat "kotlinc " relative-file-name " -include-runtime -d app.jar && kotlin ./app.jar"))
        ;; zig
        ((derived-mode-p '(zig-mode zig-ts-mode))
         (concat "zig build run"))
        ;; typescript
        ((derived-mode-p '(typescript-ts-base-mode js-base-mode))
         (concat "bun run " relative-file-name))
        ;; d2
        ((eq major-mode 'd2-mode)
         (concat "d2 -p 8888 -l elk -w " relative-file-name))
        ;; python
        ((or (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode))
         (concat "python " relative-file-name))
        ;; README.typ -> README.md
        ((and (eq major-mode 'typst-ts-mode) (equal file-name "README.typ"))
         (concat "pandoc -o README.md README.typ"))
        ;; web
        ((eq major-mode 'web-mode)
         ;; format file
         (pcase file-extension
           ("j2" (concat "djlint " relative-file-name " --extension=html.j2 --reformat"))))
        ;; ((eq major-mode 'mermaid-ts-mode)
        ;;   ;; see https://github.com/mermaid-js/mermaid-cli/issues/112#issuecomment-869401507
        ;;   (concat "mmdc -c ~/.config/mermaid/config.json -i " relative-file-name " -o " relative-bare-file-name ".svg && swayimg " relative-bare-file-name ".svg"))
        ((eq major-mode 'mermaid-ts-mode)
         ;; https://sr.ht/~meow_king/mermaid-open/
         (concat "mermaid-open -v " relative-file-name " --no-open | xargs firefox-developer-edition "))
        ((or (equal file-extension "puml") (equal file-extension "plantuml"))
         (concat "env PLANTUML_LIMIT_SIZE=327680 plantuml " relative-file-name " && imv " relative-bare-file-name ".png"))
        ((or (equal file-extension "pikchr"))
         (concat "pikchr --svg-only " relative-file-name " > " relative-bare-file-name ".svg"))
        ((and major-mode-first makefile-exist)
         "make run")
        ;; other
        (t "make "))))))

(add-hook 'after-change-major-mode-hook #'mk/set-compile-command)

;;; Extra Project Root Markers ==============================
(setq project-vc-extra-root-markers
      '(".project-root"  ; my custom
        ".jj"))  ; jujutsu

(provide 'my-misc)

;;; my-misc.el ends here
