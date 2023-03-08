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
	  "cf" #'(rust-format-buffer  :which-key "format")))

(mapBegin!
  (mk/local-leader-def
	  :states 'normal
    :keymaps '(toml-ts-mode-map rust-ts-mode-map)
    "r" #'(rustic-cargo-run :which-key "run")
    "b" #'(rustic-cargo-build :which-key "build")
    "c" '(:ignore t :which-key "dependencies")
    "ca" #'(rustic-cargo-add :which-key "add")
    "cu" #'(rustic-cargo-update :which-key "upgrade")
    "cm" #'(rustic-cargo-add-missing-dependencies :which-key "add missing")
    "t" '(:ignore t :which-key "test")
    "tt" #'(rustic-cargo-test-run :which-key "run")
    "tc" #'(rustic-cargo-current-test :which-key "current")
    "d" #'(rustic-cargo-doc :which-key "doc")))

(defun mk/add-rust-search-engine()
  "Add search engine in addition to mk/search-engines when in rust."
  (push '("crate.io" . "https://crates.io/crates/%s") mk/search-engines)
  (push '("docs.rs" . "https://docs.rs/%s") mk/search-engines))
(add-hook 'toml-ts-mode-hook 'mk/add-rust-search-engine)
(add-hook 'rust-ts-mode-hook 'mk/add-rust-search-engine)

(provide 'l-rust)
