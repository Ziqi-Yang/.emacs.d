;;; l-rust.el --- Rust -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(use-package rust-mode)
(use-package rustic
  ;; provides useful keybindings beginning with 'C-c C-c'
  :config
  (setq rustic-lsp-client 'eglot
    lsp-rust-server 'rust-analyzer))

(defun mk/add-rust-search-engine()
  "Add search engine in addition to mk/search-engines when in rust."
  (push '("crate.io" . "https://crates.io/crates/%s") mk/search-engines)
  (push '("docs.rs" . "file:///home/zarkli/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/share/doc/rust/html/std/index.html?search=%s") mk/search-engines))
(add-hook 'toml-ts-mode-hook 'mk/add-rust-search-engine)
(add-hook 'rust-ts-mode-hook 'mk/add-rust-search-engine)

(provide 'l-rust)
