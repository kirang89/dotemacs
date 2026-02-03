;;; config-ui.el --- UI and appearance -*- lexical-binding: t; -*-

;;; Commentary:
;; Themes, modeline, fonts, frame settings, and visual configuration.

;;; Code:

;;;; Font Configuration
(defconst kg/mono-font "Ioskeley Mono")
(defconst kg/org-code-font "JetBrains Mono")
(defconst kg/variable-pitch-font "SF Pro")

(set-face-attribute 'default nil
                    :family kg/mono-font
                    :height 150
                    :weight 'normal
                    :width 'normal)

(setq-default line-spacing 2)

;;;; GUI Elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;;; Frame Settings
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Make the title bar blend with the background color
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Conservative scrolling (faster than pixel-scroll-precision-mode)
(setq scroll-conservatively 101
      scroll-margin 3
      scroll-preserve-screen-position t)

;; Line numbers
(setq linum-format "%5d ")
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;;; hl-line (buffer-local for performance)
(add-hook 'prog-mode-hook #'hl-line-mode)
(with-eval-after-load 'hl-line
  (set-face-background 'hl-line "#2a2a3e")
  (set-face-attribute 'hl-line nil :underline nil))

;;;; Transparency (disabled for performance)
;; (set-frame-parameter (selected-frame) 'alpha '(93 . 93))
;; (add-to-list 'default-frame-alist '(alpha . (93 . 93)))

;;;; Window Divider
(use-package frame
  :straight (:type built-in)
  :hook (before-make-frame-hook . window-divider-mode)
  :custom
  (window-divider-default-right-width 2)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  :config
  (window-divider-mode t))

;;;; Spacious Padding
(use-package spacious-padding
  :straight (spacious-padding :type git :host github :repo "protesilaos/spacious-padding")
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '( :internal-border-width 20
      :header-line-width 2
      :mode-line-width 0
      :tab-width 4
      :right-divider-width 30
      :scroll-bar-width 8
      :left-fringe-width 2
      :right-fringe-width 2)))

;;;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq custom-safe-themes t)

(use-package autothemer :defer t)

;; Disable all active themes before loading a new one
(defun my/disable-themes-before-load (&rest _args)
  "Disable all active themes before loading a new theme."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(advice-add 'load-theme :before #'my/disable-themes-before-load)

;; Load the active theme
(load-theme 'challenger-deep t)

;;;; Doom Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (set-face-attribute 'mode-line nil :family kg/mono-font :height 150)
  (set-face-attribute 'mode-line-inactive nil :family kg/mono-font :height 150)
  :config
  ;; Don't compact font caches during GC.
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon nil
        doom-modeline-major-mode-color-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-buffer-modification-icon nil
        doom-modeline-minor-modes nil
        doom-modeline-github nil
        doom-modeline-version nil
        doom-modeline-height 25
        doom-modeline-bar-width 0
        doom-modeline-buffer-encoding nil
        doom-modeline-vcs-max-length 50
        doom-modeline-vcs-icon nil
        doom-modeline-gnus nil
        doom-modeline-irc nil
        doom-modeline-persp-name t
        doom-modeline-display-default-persp-name t
        doom-modeline-persp-icon nil
        doom-modeline-window-width-limit fill-column
        doom-modeline-lsp nil               ; Disabled for performance - updates on every LSP message
        doom-modeline-env-version nil))

;;;; Context Menu (Emacs 28+)
(when (display-graphic-p)
  (context-menu-mode 1))

;; Recover accidentally closed frames with C-x 5 u (Emacs 29+)
(undelete-frame-mode 1)

;;;; Miscellaneous UI Settings
(setq ns-use-proxy-icon nil)
(setq ns-pop-up-frames nil)

(provide 'config-ui)
;;; config-ui.el ends here
