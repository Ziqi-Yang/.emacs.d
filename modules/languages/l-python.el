;;; l-python.el --- python  -*- lexical-binding: t; -*-
;;; Commentary:
;; Note pyright needs `pyrightconfig.json' file in the project root to detect virtual
;; environment. See
;;  1. https://microsoft.github.io/pyright/#/configuration
;;  2. https://microsoft.github.io/pyright/#/configuration
;; Example pyrightconfig.json:
;; {
;;   "venvPath": ".",
;;   "venv": ".venv"
;; }
;;; Code:

(setq python-shell-interpreter "ipython"
  python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

(provide 'l-python)
;;; l-python.el ends here
