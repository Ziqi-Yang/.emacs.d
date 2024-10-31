;;; my-ai.el --- AI Powered Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package aider
  :ensure (:host github :repo "tninja/aider.el")
  :custom
  ;; (aider-args '("--model" "openrouter/deepseek/deepseek-coder"))
  (aider-args '("--model" "openrouter/anthropic/claude-3.5-sonnet"))
  :config
  (setenv "OPENAI_API_KEY" mk/private-vars/gptel-openrouter-key)
  (setenv "OPENROUTER_API_KEY" mk/private-vars/gptel-openrouter-key))

(use-package gptel
  :config
  (setq gptel-model 'deepseek/deepseek-chat)
  (setq gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key 'mk/private-vars/gptel-openrouter-key
          :models '(deepseek/deepseek-chat
                    anthropic/claude-3.5-sonnet
                    openai/chatgpt-4o-latest))))

(defun mk/better-gptel (&optional arg)
  (interactive "P")
  (if (or arg (region-active-p))
      (call-interactively #'gptel-menu)
    (call-interactively #'gptel)))

(provide 'my-ai)

;;; my-ai.el ends here
