;;; config-lsp.el --- LSP and code intelligence -*- lexical-binding: t; -*-

;;; Commentary:
;; Eglot-based LSP support with fallback to dumb-jump.

;;; Code:

;;;; Project
(use-package project
  :ensure nil)

;;;; Xref
(use-package xref
  :ensure nil
  :bind (("M-." . xref-find-definitions)
         ("M-," . xref-go-back)
         ("M-?" . xref-find-references)))

;;;; Eldoc
(use-package eldoc
  :config
  ;; Only enable in programming modes (not globally) for performance
  (global-eldoc-mode -1)
  (add-hook 'prog-mode-hook #'eldoc-mode)
  (setq eldoc-echo-area-use-multiline-p nil
        eldoc-idle-delay 0.75)           ; Wait 0.75s before showing (was 0.5)
  :custom-face
  (tooltip ((t (:inherit default :box nil)))))

;;;; Eglot
(use-package eglot
  :hook
  ((python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   (ruby-mode . eglot-ensure)
   (ruby-ts-mode . eglot-ensure)
   (js-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (clojure-mode . eglot-ensure)
   (tuareg-mode . eglot-ensure)
   (go-mode . eglot-ensure)
   (go-ts-mode . eglot-ensure))
  :ensure nil
  :config
  ;; Use pyright for Python (provides completions, unlike ruff)
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (setq eglot-connect-timeout 60
        eglot-sync-connect nil            ; Non-blocking connection
        eglot-events-buffer-size 0        ; Disable event logging
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5  ; Debounce changes
        eglot-extend-to-xref t            ; Better xref integration
        ;; Disable features that run on every cursor move
        eglot-ignored-server-capabilities
        '(:documentHighlightProvider      ; Don't highlight all occurrences of symbol
          :inlayHintProvider)             ; Don't show inline type hints
        eglot-report-progress nil))       ; Don't show LSP progress spinner

;;;; Eglot Booster
(use-package eglot-booster
  :straight (eglot-booster :type git
                           :host github
                           :repo "jdtsmith/eglot-booster"
                           :branch "main")
  :after eglot
  :config
  ;; Emacs 30 has faster JSON parser, use IO-only mode
  (setq eglot-booster-io-only t)
  (eglot-booster-mode))

;;;; Dumb Jump (fallback)
(use-package dumb-jump
  :after xref
  :config
  (setq dumb-jump-force-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;;; Eldoc Box - DISABLED for performance
;;;; Documentation shows in echo area instead (posframes are expensive)
;; (use-package eldoc-box
;;   :hook
;;   (eglot-managed-mode . eldoc-box-hover-at-point-mode)
;;   :config
;;   (setq eldoc-box-clear-with-C-g t
;;         eldoc-box-max-pixel-height 600
;;         eldoc-box-max-pixel-width 800)
;;   :custom-face
;;   (eldoc-box-border ((t (:background unspecified :foreground unspecified))))
;;   (eldoc-box-body ((t (:inherit default)))))

(provide 'config-lsp)
;;; config-lsp.el ends here
