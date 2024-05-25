;;; my-note.el --- Taking Notes  -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Meow King <mr.meowking@anche.no>

;;; Commentary:

;;; Code:

(use-package ekg
  :custom
  (ekg-db-file
   ;; (expand-file-name "ekg.db" user-emacs-directory)
   (expand-file-name "ekg.db" "~/personal/notes"))
  (ekg-acceptable-modes '(org-mode markdown-mode text-mode typst-ts-mode))
  (ekg-capture-default-mode 'typst-ts-mode)
  (ekg-inline-custom-tag-completion-symbols '((?! . "idea"))))

(provide 'my-note)

;;; my-note.el ends here
