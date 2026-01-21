;;; config-lsp.el --- LSP and code intelligence -*- lexical-binding: t; -*-

;;; Commentary:
;; Eglot-based LSP support with fallback to dumb-jump.

;;; Code:

;;;; Project
(use-package project
  :ensure nil
  :straight nil)

;;;; Xref
(use-package xref
  :ensure nil
  :bind (("M-." . xref-find-definitions)
         ("M-," . xref-go-back)
         ("M-?" . xref-find-references)))

;;;; Eldoc
(use-package eldoc
  :config
  (global-eldoc-mode 1)
  (setq eldoc-echo-area-use-multiline-p nil)
  :custom-face
  (tooltip ((t (:inherit default :box nil)))))

;;;; Eglot
(use-package eglot
  :defer t
  :hook
  ((python-mode . eglot-ensure)
   (ruby-mode . eglot-ensure)
   (js-mode . eglot-ensure)
   (clojure-mode . eglot-ensure)
   (tuareg-mode . eglot-ensure)
   (go-mode . eglot-ensure))
  :after project
  :after xref
  :ensure nil
  :straight nil
  :config
  (setq eglot-connect-timeout 60
        eglot-sync-connect nil
        eglot-events-buffer-size 0
        eglot-autoshutdown t))

;;;; Eglot Booster
(use-package eglot-booster
  :straight (eglot-booster :type git
                           :host github
                           :repo "jdtsmith/eglot-booster"
                           :branch "main")
  :after eglot
  :config
  (eglot-booster-mode))

;;;; Dumb Jump (fallback)
(use-package dumb-jump
  :after xref
  :config
  (setq dumb-jump-force-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;;; Eldoc Box
(use-package eldoc-box
  :hook
  (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  :config
  (setq eldoc-box-clear-with-C-g t
        eldoc-box-max-pixel-height 600
        eldoc-box-max-pixel-width 800)
  :custom-face
  (eldoc-box-border ((t (:background unspecified :foreground unspecified))))
  (eldoc-box-body ((t (:inherit default)))))

(provide 'config-lsp)
;;; config-lsp.el ends here
