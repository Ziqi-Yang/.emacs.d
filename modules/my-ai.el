;;; my-ai.el --- AI Powered Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :config
  (setq
   gptel-model "gpt-4-turbo-preview"
   gptel-backend
   (gptel-make-openai "openai-api"
     :host "api.gptsapi.net"
     :key 'mk/private-vars/gptel-openai-key
     :models '("gpt-3.5-turbo" "gpt-4-turbo-preview"))))

(provide 'my-ai)

;;; my-ai.el ends here
