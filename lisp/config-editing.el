;;; config-editing.el --- Editing enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; Structural editing, multiple cursors, and text manipulation.

;;; Code:

;;;; Basic Editing Settings

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil
              tab-width 2
              c-basic-offset 2)
(setq js-indent-level 2
      css-indent-offset 2)

;; Type over selected text
(delete-selection-mode 1)

;; Remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; Paren Matching
(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 0.125)
(set-face-background 'show-paren-match (face-background 'default))
(if (eq (frame-parameter nil 'background-mode) 'dark)
    (set-face-foreground 'show-paren-match "purple")
  (set-face-foreground 'show-paren-match "black"))
(set-face-attribute 'show-paren-match nil :weight 'bold)

;;;; Paredit
(use-package paredit
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (paredit-mode)
              (local-set-key (kbd "<C-s-right>") #'paredit-forward-slurp-sexp)
              (local-set-key (kbd "<C-s-left>") #'paredit-forward-barf-sexp))))

;;;; Rainbow Delimiters
(use-package rainbow-delimiters
  :defer t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;; Smartparens
(use-package smartparens
  :init
  (add-hook 'text-mode-hook 'smartparens-mode)
  (add-hook 'markdown-mode-hook 'smartparens-mode)
  (add-hook 'prog-mode-hook 'smartparens-mode))

;;;; Multiple Cursors
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

;;;; Expand Region
(use-package expand-region
  :config
  (global-set-key (kbd "C-;") 'er/expand-region)
  (global-set-key (kbd "C-:") 'er/mark-inside-pairs))

;;;; Aggressive Indent
(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

;;;; Change Inner (vim's ci command)
(use-package change-inner)

;;;; Easy Kill
(use-package easy-kill
  :bind ([remap kill-ring-save] . easy-kill))

;;;; Smart Comment
(use-package smart-comment
  :bind ("M-;" . smart-comment))

;;;; Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(provide 'config-editing)
;;; config-editing.el ends here
