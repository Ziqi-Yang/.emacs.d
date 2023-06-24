;;; l-draw.el --- Key Mappings Here -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;;; PlantUML ================================================
;; TODO try plantuml-emacs: https://github.com/ginqi7/plantuml-emacs
(use-package plantuml-mode
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :init
  ;; enable plantuml babel support
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
    (append org-babel-load-languages
      '((plantuml . t))))
  :config
  (setq org-plantuml-exec-mode 'plantuml)
  (setq org-plantuml-executable-path "plantuml")
  (setq plantuml-executable-path "plantuml")
  (setq plantuml-default-exec-mode 'executable)
  ;; set default babel header arguments
  (setq org-babel-default-header-args:plantuml
    '((:exports . "results")
       (:results . "file")
       )))

;;; Mermaid =================================================
;; github support rendering mermaid diagrams
(use-package mermaid-mode
  :custom
  (mermaid-flags "-w 1568 -H 1168"))

(defun mk/draw-local-keybinding-setup()
  (keymap-local-set "C-c C-c c" #'mermaid-compile)
  (keymap-local-set "C-c C-c b" #'mermaid-open-browser)
  (keymap-local-set "C-c C-c d" #'mermaid-open-doc))

(add-hook 'mermaid-mode-hook 'mk/draw-local-keybinding-setup)

(provide 'l-draw)
