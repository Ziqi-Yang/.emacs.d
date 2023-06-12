;;; l-rust.el --- Rust -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(use-package rust-mode)
(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot
    lsp-rust-server 'rust-analyzer))

(defun mk/rust-local-keybinding-setup()
  (keymap-local-set "M-f" #'rust-format-buffer)
  (keymap-local-set "M-r" #'rustic-cargo-run)
  (keymap-local-set "M-b" #'rustic-cargo-build)
  (keymap-local-set "M-c c" #'rust-run-clippy)
  (keymap-local-set "M-c f" #'rustic-cargo-clippy-fix)
  (keymap-local-set "M-d a" #'rustic-cargo-add)
  (keymap-local-set "M-d u" #'rustic-cargo-update)
  (keymap-local-set "M-d m" #'rustic-cargo-add-missing-dependencies)
  (keymap-local-set "M-t t" #'rustic-cargo-test-run)
  (keymap-local-set "M-t c" #'rustic-cargo-current-test)
  (keymap-local-set "M-D" #'rustic-cargo-doc))

(add-hook 'rust-mode-hook 'mk/rust-local-keybinding-setup)
(add-hook 'rust-ts-mode-hook 'mk/rust-local-keybinding-setup)

(defun mk/add-rust-search-engine()
  "Add search engine in addition to mk/search-engines when in rust."
  (push '("crate.io" . "https://crates.io/crates/%s") mk/search-engines)
  (push '("docs.rs" . "file:///home/zarkli/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/share/doc/rust/html/std/index.html?search=%s") mk/search-engines))
(add-hook 'toml-ts-mode-hook 'mk/add-rust-search-engine)
(add-hook 'rust-ts-mode-hook 'mk/add-rust-search-engine)

(provide 'l-rust)
