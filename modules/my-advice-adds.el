;;; my-advice-adds.el -- My advice adds  -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Meow King <mr.meowking@anche.no>

;;; Commentary:

;;; Code:
(require 'my-utils)

;; note that `kill-ring-deindent-mode' is useful in Python mode. So I enabled it.
;; When debug, don't ignore it.
;; (defun mk/advice/clipboard-yank (&rest _r)
;;   "Advice (type: after) for command `yank'."
;;   (indent-region (region-beginning) (region-end)))

(defun mk/advice/global-text-scale-change (&rest _r)
  "Changes "
  (set-fontset-font t 'han (font-spec :family CJK-font :size font-size))
  (set-face-attribute 'han nil :height (face-attribute 'default :height)))

;; not a advice add function
;; (defun mk/yank-without-indent()
;;   (interactive)
;;   (funcall (advice--cdr (symbol-function #'yank))))

(defun mk/advice/compile (oldfun command &rest r)
  "Advice (type: around) for command `compile'.
OLDFUN COMMAND R."
  (let* ((command-tidy (string-clean-whitespace (string-trim command))))
    (when-let* ((p (project-current))
                (pt (project-root p)))
      (unless (string-prefix-p "nix develop" command-tidy)
        (let ((python-venv (concat pt ".venv")))
          (cond
           ((and (file-exists-p python-venv) (not (string-prefix-p "source" command-tidy)))
            (setq command (concat "source " python-venv "/bin/activate; " command)))))
        (when (and mk/vars/in-nixos (file-exists-p (concat pt "/flake.nix")))
          (setq command (format "nix develop -c bash -c \"%s\"" (mk/util/quote-string command))))))
    (apply oldfun command r)))

(defun mk/my-advice-add-initialize()
  "Add all my custom advices.
This function should be called after init, so that other initialization can work properly."
  ;; (advice-add #'clipboard-yank :after #'mk/advice/clipboard-yank)
  (advice-add #'compile :around #'mk/advice/compile)

  (with-eval-after-load 'typescript-ts-mode
    (advice-add #'typescript-ts-mode--indent-rules :filter-return #'mk/typescript-ts-mode--indent-rules)))


;; indent region being yanked
(add-hook 'after-init-hook #'mk/my-advice-add-initialize)



(defun mk/typescript-ts-mode--indent-rules (rules)
  (let ((ts-rules (car rules)))
    (nbutlast ts-rules 1)
    (nconc
     ts-rules
     '(((lambda (_node parent _bol)
          (treesit-parent-until parent "\\`template_string\\'" t))
        no-indent)
       (no-node parent-bol 0))))
  rules)

(provide 'my-advice-adds)

;;; my_addvice_adds.el ends here
