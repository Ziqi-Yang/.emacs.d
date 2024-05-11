;;; my-ai.el --- AI Powered Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :config
  (setq
   gptel-model "gpt-4o"
   gptel-backend
   (gptel-make-openai "openai-api"
     :host "api.gptsapi.net"
     :key 'mk/private-vars/gptel-openai-key
     :stream t
     :models '("gpt-3.5-turbo" "gpt-4-turbo-preview" "gpt-4o"))))

(defun mk/better-gptel (&optional arg)
  (interactive "P")
  (if (or arg (region-active-p))
      (call-interactively #'gptel-menu)
    (call-interactively #'gptel-send)))

(provide 'my-ai)

;;; my-ai.el ends here
