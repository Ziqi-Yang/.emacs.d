;;; my-ai.el --- AI Powered Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (defconst openai-api-key-path (expand-file-name "openai-api-key.txt" user-emacs-directory))
;; (defvar openai-api-key)

;; (defun mk/get-openai-api-key ()
;;   "Get openai-api-key from ~/.emacs.d/openai-api-key.txt"
;;   (interactive)
;;   (if (bound-and-true-p openai-api-key)
;;     openai-api-key
;;     (let ((key (with-temp-buffer
;;                  (insert-file-contents openai-api-key-path)
;;                  (substring (buffer-string) 0 -1))))
;;       (setq openai-api-key key)
;;       key)))

;; (use-package gptel
;;   :straight (:host github :repo "karthink/gptel" :files ("*.el"))
;;   :config
;;   (setq gptel-api-key #'mk/get-openai-api-key
;;     gptel-use-curl nil))

;; (use-package mind-wave
;;   :straight (:host github :repo "manateelazycat/mind-wave" :files ("*.el" "*.py"))
;;   :config
;;   (setq mind-wave-api-key-path openai-api-key-path
;;     mind-wave-api-base "https://openai-proxy.animer.live/v1"))

(provide 'my-ai)

;;; my-ai.el ends here
