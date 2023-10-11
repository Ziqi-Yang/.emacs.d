;;; l-java.el --- Java -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(use-package zig-mode
  :elpaca (:type git :host github :repo "ziglang/zig-mode")
  :config
  (setq zig-format-on-save nil))

(defun mk/zig-local-keybinding-setup()
  (keymap-local-set "C-c C-c c" #'zig-compile)
  (keymap-local-set "C-c C-c C-b" #'zig-build-exe)
  (keymap-local-set "C-c C-c C-l" #'zig-build-lib)
  (keymap-local-set "C-c C-c C-o" #'zig-build-obj)
  (keymap-local-set "C-c C-c t" #'zig-test-buffer)
  (keymap-local-set "C-c C-c r" #'zig-run)
  (keymap-local-set "C-c C-c f" #'zig-format-buffer))

(add-hook 'zig-mode-hook 'mk/zig-local-keybinding-setup)

(provide 'l-zig)
