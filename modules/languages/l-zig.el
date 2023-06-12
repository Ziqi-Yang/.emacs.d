;;; l-java.el --- Java -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

(use-package zig-mode
  :straight (:type git :host github :repo "ziglang/zig-mode")
  :config
  (setq zig-format-on-save nil))

;; (mapBegin!
;;   (mk/local-leader-def
;;     :states 'normal
;;     :keymaps 'zig-mode-map
;;     "c" #'zig-compile
;;     "e" #'zig-build-exe
;;     "l" #'zig-build-lib
;;     "o" #'zig-build-obj
;;     "t" #'zig-test-buffer
;;     "r" #'zig-run
;;     "f" #'zig-format-buffer))

(provide 'l-zig)
