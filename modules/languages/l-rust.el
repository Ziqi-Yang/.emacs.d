;;; l-rust.el --- Rust -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;; NOTE: currently rustic doesn't support rust-ts-mode
;; (use-package rust-mode)
;; (use-package rustic
;;   ;; provides useful keybindings beginning with 'C-c C-c'
;;   :config
;;   (setq rustic-lsp-client 'eglot
;;     lsp-rust-server 'rust-analyzer))

(defun mk/cargo-crate-info (crate-name)
  "Get the basic information of a crate. The underlying command is =cargo add --dry-run crate="
  (interactive (list (read-string "crate name: " (thing-at-point 'symbol))))
  (async-shell-command (concat "cargo add --dry-run " crate-name)))

(defun mk/rust-local-keybinding-setup()
  (keymap-local-set "C-c C-c I" #'mk/cargo-crate-info))

(defun mk/add-rust-search-engine()
  "Add search engine in addition to mk/search-engines when in rust."
  (push '("crate.io" . "https://crates.io/crates/%s") mk/search-engines)
  (push '("docs.rs" . "https://docs.rs/%s/") mk/search-engines)
  (push '("rust-std" . "file:///home/zarkli/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/share/doc/rust/html/std/index.html?search=%s") mk/search-engines))

(add-hook 'toml-ts-mode-hook 'mk/add-rust-search-engine)
(add-hook 'rust-ts-mode-hook 'mk/add-rust-search-engine)
;; (add-hook 'rust-mode-hook 'mk/add-rust-search-engine)

(add-hook 'toml-ts-mode-hook 'mk/rust-local-keybinding-setup)
(add-hook 'rust-ts-mode-hook 'mk/rust-local-keybinding-setup)

(provide 'l-rust)

;;; l-rust.el ends here
