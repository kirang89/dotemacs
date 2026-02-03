;;; config-outline.el --- Symbol outline sidebar -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a persistent sidebar displaying code symbols organized by category.
;; Uses Eglot integration for accurate symbol information.

;;; Code:

(use-package symbols-outline
  :straight (symbols-outline :host github :repo "liushihao456/symbols-outline.el")
  :bind (("C-c o" . symbols-outline-toggle))
  :commands (symbols-outline-show symbols-outline-toggle)
  :config
  ;; Window positioning
  (setq symbols-outline-window-position 'right
        symbols-outline-window-width 35)

  ;; Use Eglot as primary backend when available
  (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch)

  ;; Collapse deeper nesting on startup for cleaner view
  (setq symbols-outline-collapse-functions-on-startup t)

  ;; Icon display (uses nerd-icons if available)
  (setq symbols-outline-use-nerd-icon-in-gui t))

(provide 'config-outline)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; End:
;;; config-outline.el ends here
