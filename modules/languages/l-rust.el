;;; l-rust.el --- Rust -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(use-package rust-mode)
(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot
    lsp-rust-server 'rust-analyzer))

(mapBegin!
  (mk/leader-def
	  :states '(normal visual)
    :keymaps 'override
	  "cF" #'(rust-format-buffer  :which-key "format")))

(provide 'l-rust)
