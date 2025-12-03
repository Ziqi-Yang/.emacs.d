;;; my-ai.el --- AI-powered features for Emacs -*- lexical-binding: t -*-

;; (use-package aider
;;   :disabled
;;   :ensure (:host github :repo "tninja/aider.el")
;;   :custom
;;   (aider-args '("--no-auto-commits" "--model" "openrouter/anthropic/claude-3.5-sonnet"))
;;   :config
;;   (setenv "OPENAI_API_KEY" mk/private-vars/gptel-openrouter-key)
;;   (setenv "OPENROUTER_API_KEY" mk/private-vars/gptel-openrouter-key))


;; For better aidermacs experience. Aidermacs hasn't supported `eat' yet.
(use-package vterm
  ;; installed via Nix
  :ensure nil)

;; (use-package aidermacs
;;   :ensure (:host github :repo "MatthewZMD/aidermacs")
;;   :bind (("C-c a" . aidermacs-transient-menu))
;;   :custom
;;   (aidermacs-use-architect-mode t)
;;   (aidermacs-backend 'vterm)
;;   (aidermacs-default-model "openrouter/anthropic/claude-3.7-sonnet")
;;   (aidermacs-architect-model "openrouter:anthropic/claude-3.7-sonnet:thinking")
;;   :config
;;   (aidermacs-setup-minor-mode))

(use-package gptel
  :disabled
  :config
  (setq gptel-model 'google/gemini-3-pro-preview)
  (setq gptel-backend
        (gptel-make-openai
            "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key 'mk/private-vars/gptel-openrouter-key
          :models '(google/gemini-3-pro-preview
                    anthropic/claude-sonnet-4.5))))

(defun mk/better-gptel (&optional arg)
  "Enhanced GPTel interaction command.
With prefix ARG or active region, open GPTel menu.
Otherwise, start a new GPTel chat session."
  (interactive "P")
  (if (or arg (region-active-p))
      (call-interactively #'gptel-menu)
    (call-interactively #'gptel)))

(provide 'my-ai)
;;; my-ai.el ends here
