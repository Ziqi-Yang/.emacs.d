;;; l-java.el --- Java -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(use-package zig-mode
  :straight (:type git :host github :repo "ziglang/zig-mode")
  :config
  (setq zig-format-on-save nil))

(defun mk/zig-local-keybinding-setup()
  (keymap-local-set "M-c" #'zig-compile)
  (keymap-local-set "M-e" #'zig-build-exe)
  (keymap-local-set "M-l" #'zig-build-lib)
  (keymap-local-set "M-o" #'zig-build-obj)
  (keymap-local-set "M-t" #'zig-test-buffer)
  (keymap-local-set "M-r" #'zig-run)
  (keymap-local-set "M-f" #'zig-format-buffer))

(add-hook 'zig-mode-hook 'mk/zig-local-keybinding-setup)

(provide 'l-zig)
