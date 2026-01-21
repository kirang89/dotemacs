;;; config-tools.el --- Utility packages and tools -*- lexical-binding: t; -*-

;;; Commentary:
;; exec-path-from-shell, which-key, flycheck, helpful, and other utilities.

;;; Code:

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. This library works around this problem by copying important
;; environment variables from the user's shell.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package which-key
  :defer 1
  :config
  (which-key-mode))

(use-package flycheck
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  ;; Make flycheck indicators more subtle
  (custom-set-faces
   '(flycheck-error ((t (:underline (:color "#742828" :style line)))))
   '(flycheck-warning ((t (:underline (:color "#742828" :style line)))))
   '(flycheck-info ((t (:underline (:color "#415141" :style line))))))

  (with-eval-after-load 'flycheck
    ;; Specifically handle free variable warnings with byte-compiler options
    (setq-default flycheck-emacs-lisp-load-path 'inherit)
    (setq-default flycheck-emacs-lisp-check-form
                  "(progn (setq byte-compile-warnings '(not free-vars unresolved)) (batch-byte-compile))"))

  (with-eval-after-load 'flycheck
    ;; Disable checkdoc - this checks documentation style
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (setq-default flycheck-emacs-lisp-variables-indent-listed t))

  (setq flycheck-indication-mode 'left-fringe)
  ;; Use smaller, more subtle fringe bitmaps
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [0 0 0 0 0 16 48 112 112 48 16 0 0 0 0 0])
  ;; Make error highlighting more subtle
  (setq flycheck-highlighting-mode 'lines)
  ;; Check syntax on save and mode-enabled
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-highlighting-mode 'symbols)
  (setq flycheck-error-list-minimum-level 'error)
  (setq flycheck-idle-change-delay 0.5))

(use-package helpful
  :defer t
  :bind (("C-h f" . 'helpful-callable)
         ("C-h v" . 'helpful-variable)
         ("C-h k" . 'helpful-key)
         ("C-h x" . 'helpful-command)))

(use-package saveplace
  :straight (:type built-in)
  :init
  (save-place-mode 1)
  :config
  (setq-default save-place t))

(use-package ace-window
  :straight t
  :bind
  ("M-o" . ace-window))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package dape
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-info-hide-mode-line nil)
  (dape-cwd-function #'projectile-project-root))

(use-package repeat
  :custom
  (repeat-mode +1))

(use-package midnight
  :defer 30
  :custom
  (midnight-delay 18000)
  (clean-buffer-list-kill-never-buffer-names '("*scratch*" "*Messages*"))
  (clean-buffer-list-kill-never-regexps
   '("^ \\*Minibuf-.*\\*$"))
  (clean-buffer-list-kill-regexps '(".*"))
  :config
  (midnight-mode t))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package copy-as-format
  :custom
  (copy-as-format-default "slack"))

(use-package harpoon)

(use-package key-chord
  :hook
  (prog-mode . key-chord-mode)
  :config
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-define-global "j1" (lambda () (interactive) (harpoon-go-to 1)))
  (key-chord-define-global "j2" (lambda () (interactive) (harpoon-go-to 2)))
  (key-chord-define-global "j3" (lambda () (interactive) (harpoon-go-to 3)))
  (key-chord-define-global "j4" (lambda () (interactive) (harpoon-go-to 4))))

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :custom
  (fill-column-enable-sensible-window-split t))

(use-package benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package apheleia
  :straight t
  :hook (prog-mode . apheleia-mode)
  :init
  (setq apheleia-npx-executable (or (executable-find "npx")
                                    "/Users/kiran/.asdf/shims/npx"))
  :config
  (add-to-list 'apheleia-mode-alist '(python-mode . (ruff-isort ruff)))
  (apheleia-global-mode t))

(use-package editorconfig)
(use-package jsonrpc)
(use-package transient)

(provide 'config-tools)
;;; config-tools.el ends here
