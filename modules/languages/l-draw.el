;;; l-draw.el --- Key Mappings Here -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

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

(use-package mermaid-mode
  :custom
  (mermaid-flags "-w 1568 -H 1168")
  :config
  (mapBegin!
    (mk/local-leader-def
	    :states 'normal
	    :keymaps 'mermaid-mode-map
      "c" #'(mermaid-compile :which-key "compile current-file")
      "o" #'(mermaid-open-browser :which-key "open in browser editor")
      "d" #'(mermaid-open-doc :which-key "documentation"))))


(provide 'l-draw)
