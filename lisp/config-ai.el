;;; config-ai.el --- AI assistants -*- lexical-binding: t; -*-

;;; Commentary:
;; LLM integration for coding assistance.

;;; Code:

;;;; Agent Shell
(use-package agent-shell :ensure t)

;;;; Ellama
(use-package ellama
  :straight (:host github :repo "s-kostyaev/ellama")
  :init
  (setopt ellama-keymap-prefix "C-c e")
  (require 'llm-ollama)
  (setopt ellama-provider
	        (make-llm-ollama :chat-model "llama3.1:latest")))

;;;; Minuet
(use-package minuet
  :init
  ;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  :config
  (setq minuet-provider 'openai-fim-compatible)
  (setq minuet-n-completions 1)
  (setq minuet-context-window 512)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "qwen3:8b")
  (minuet-set-optional-options minuet-openai-fim-compatible-options :suffix nil :template)
  (minuet-set-optional-options
   minuet-openai-fim-compatible-options
   :prompt
   (defun minuet-llama-cpp-fim-qwen-prompt-function (ctx)
     (format "<|fim_prefix|>%s\n%s<|fim_suffix|>%s<|fim_middle|>"
             (plist-get ctx :language-and-tab)
             (plist-get ctx :before-cursor)
             (plist-get ctx :after-cursor)))
   :template)

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64))

;;;; Aidermacs
(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "qwen2.5-coder:1.5b")
  (aidermacs-architect-model "deepcoder:latest"))

(provide 'config-ai)
;;; config-ai.el ends here
