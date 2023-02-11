;;; l-latex.el --- Latex Settings -*- lexical-binding: t -*-
;;; Commentary:
;; Accroding to TexMode (https://www.gnu.org/software/emacs/manual/html_node/emacs/TeX-Mode.html)
;; emacs has buildin BibTeX and RefTeX package, but AUCTeX pakcage is not included
;; However, the buildin support is very enough for latex editing, the somewhat lackging for single command
;; completion support can be dealt with tempel/yasnippet (and use other's work :) ).
;;; Code:

;; @ auctex DON'T actually needed
;; the installation is pesky, please refer to others installations:
;; https://github.com/radian-software/straight.el/issues/1042
;; (use-package auctex
;; 	:straight (:host github :repo "emacs-straight/auctex"
;; 									 :pre-build (("./autogen.sh")
;; 															 ("./configure" "--without-texmf-dir" "--with-lispdir=.")
;; 															 ("make"))
;; 									 ;; https://github.com/raxod502/straight.el/issues/899#issuecomment-989436958
;; 									 :files (:defaults "doc/*"))
;; 	:config
;; 	(setq-default TeX-master nil)
;; 	(setq TeX-auto-save t
;; 				TeX-parse-self t))

(provide 'l-latex)
