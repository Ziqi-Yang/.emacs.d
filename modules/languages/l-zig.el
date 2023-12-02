;;; l-java.el --- Java -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(use-package zig-mode
  :elpaca (:type git :host github :repo "ziglang/zig-mode")
  :config
  (setq zig-format-on-save nil)
  
  (keymap-set zig-mode-map "C-c C-c c" #'zig-compile)
  (keymap-set zig-mode-map "C-c C-c C-b" #'zig-build-exe)
  (keymap-set zig-mode-map "C-c C-c C-l" #'zig-build-lib)
  (keymap-set zig-mode-map "C-c C-c C-o" #'zig-build-obj)
  (keymap-set zig-mode-map "C-c C-c t" #'zig-test-buffer)
  (keymap-set zig-mode-map "C-c C-c r" #'zig-run)
  (keymap-set zig-mode-map "C-c C-c f" #'zig-format-buffer))

(provide 'l-zig)

;;; l-zig.el ends here
