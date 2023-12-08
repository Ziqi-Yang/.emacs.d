;;; l-python.el --- python  -*- lexical-binding: t; -*-
;; Commentary:
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

;; (setq python-shell-interpreter "ipython"
;;   python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

(with-eval-after-load
  (setq python-shell-interpreter "python"
    python-shell-interpreter-args "-i"))

(with-eval-after-load 'python
  (keymap-unset python-mode-map "C-c C-c")
  (keymap-set python-mode-map "C-c C-c o" #'python-fix-imports) ;; NOTE!!

  (keymap-unset python-ts-mode-map "C-c C-c")
  (keymap-set python-ts-mode-map "C-c C-c o" #'python-fix-imports) ;; NOTE!!
  )


(provide 'l-python)
;;; l-python.el ends here
