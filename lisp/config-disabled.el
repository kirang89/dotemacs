;;; config-disabled.el --- Disabled/experimental packages -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains packages that are disabled or experimental.
;; To enable any package, move it to the appropriate config-*.el file
;; and add the corresponding (require 'config-*) to init.el.
;;
;; This file is NOT loaded at startup.

;;; Code:

;;;; =========================================================
;;;;                     MODAL EDITING
;;;; =========================================================

;; (use-package evil
;;   :defer t
;;   :config
;;   (use-package evil-surround)
;;
;;   (with-eval-after-load 'evil
;;     (define-prefix-command 'my-leader-map)
;;     (evil-define-key 'normal 'global (kbd "SPC") 'my-leader-map)
;;
;;     (define-key my-leader-map (kbd "f") 'find-file)
;;     (define-key my-leader-map (kbd "b") 'switch-to-buffer)))

;; (use-package boon-qwerty
;;   :straight (:type git :host github :repo "jyp/boon")
;;   :config
;;   (boon-mode -1))

;;;; =========================================================
;;;;                        ICONS
;;;; =========================================================

;; (use-package all-the-icons
;;   :if (display-graphic-p)
;;   :config
;;   (setq all-the-icons-scale-factor 1.1))

;; (use-package all-the-icons-completion
;;   :after (marginalia all-the-icons)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init
;;   (all-the-icons-completion-mode))

;;;; =========================================================
;;;;                      AI/COPILOT
;;;; =========================================================

;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
;;   :config
;;   (add-hook 'emacs-mode-hook 'copilot-mode)
;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

;; Copilot helper functions (if copilot is enabled)
;; (defun kg/no-copilot-mode ()
;;   "Helper for `kg/no-copilot-modes'."
;;   (copilot-mode -1))
;;
;; (defvar kg/no-copilot-modes '(shell-mode
;;                               inferior-python-mode
;;                               eshell-mode
;;                               term-mode
;;                               vterm-mode
;;                               comint-mode
;;                               compilation-mode
;;                               debugger-mode
;;                               dired-mode-hook
;;                               compilation-mode-hook
;;                               flutter-mode-hook
;;                               minibuffer-mode-hook
;;                               shell-script-modes)
;;   "Modes in which copilot is inconvenient.")
;;
;; (defun kg/copilot-disable-predicate ()
;;   "When copilot should not automatically show completions."
;;   (or (member major-mode kg/no-copilot-modes)))
;;
;; (add-to-list 'copilot-disable-predicates #'kg/copilot-disable-predicate)

;;;; =========================================================
;;;;                     MODELINES
;;;; =========================================================

;; (use-package nano-modeline
;;   :custom
;;   (nano-modeline-position #'nano-modeline-footer)
;;   :config
;;   (setq-default mode-line-format nil)
;;   (nano-modeline-text-mode nil)
;;   (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
;;   (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
;;   (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
;;   (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
;;   (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
;;   (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
;;   (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
;;   (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode))

;;;; =========================================================
;;;;                    COMPLETION UI
;;;; =========================================================

;; Does not work with search
;; (use-package nano-vertico
;;   :straight (:host github :repo "rougier/nano-vertico")
;;   :config (nano-vertico-mode t))

;;;; =========================================================
;;;;                    POPUP MANAGEMENT
;;;; =========================================================

;; (use-package popper
;;   :straight t
;;   :bind (("C-`"   . popper-toggle)
;;          ("M-`"   . popper-cycle)
;;          ("C-M-`" . popper-toggle-type))
;;   :custom
;;   (popper-group-function #'popper-group-by-project)
;;   :init
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           "Output\\*$"
;;           "\\*Async Shell Command\\*"
;;           help-mode
;;           helpful-mode
;;           compilation-mode))
;;   (popper-mode +1)
;;   (popper-echo-mode +1))

;;;; =========================================================
;;;;                      INDENTATION
;;;; =========================================================

;; (use-package outline-indent
;;   :straight (outline-indent
;;              :type git
;;              :host github
;;              :repo "jamescherti/outline-indent.el")
;;   :custom
;;   (outline-indent-ellipsis " ▼ "))

;; (use-package indent-bars
;;   :straight t
;;   :commands (indent-bars-mode)
;;   :hook (prog-mode . indent-bars-mode)
;;   :custom
;;   (indent-bars-pattern ".")
;;   (indent-bars-width-frac 0.1)
;;   (indent-bars-pad-frac 0.1)
;;   (indent-bars-no-descend-lists t)
;;   (indent-bars-treesit-support t)
;;   (indent-bars-treesit-ignore-blank-lines-types '("module"))
;;   (indent-bars-prefer-character t)
;;   (indent-bars-color '(highlight :face-bg t :blend 0.25))
;;   (indent-bars-highlight-current-depth '(:face default :blend 0.4))
;;   (indent-bars-color-by-depth nil))

;;;; =========================================================
;;;;                        XREF
;;;; =========================================================

;; Custom xref with ripgrep backend
;; (use-package xref
;;   :straight (:type built-in)
;;   :custom
;;   (xref-search-program 'ripgrep)
;;   ;; Disable tags-based backend entirely
;;   (xref-backend-functions (remove 'etags--xref-backend xref-backend-functions))
;;   :config
;;   (setq-default xref-search-program 'ripgrep)
;;   (setq xref-search-program-extra-args
;;         '("--null" "--line-buffered" "--color=never" "--max-columns=1000"))
;;   ;; Create a custom xref backend that will fall back to ripgrep
;;   (defun ripgrep-xref-backend ()
;;     "Custom xref backend using ripgrep."
;;     (lambda (pattern files dir ignores)
;;       (let ((xref-ripgrep-command
;;              (format "rg --no-heading --with-filename --line-number -n %s"
;;                      pattern)))
;;         (xref-matches-in-directory pattern dir))))
;;
;;   ;; Add our backend to the list
;;   (add-to-list 'xref-backend-functions 'ripgrep-xref-backend))

;;;; =========================================================
;;;;                       PYTHON
;;;; =========================================================

;; (use-package pet
;;   :commands (pet-mode)
;;   :init
;;   (add-hook 'python-base-mode-hook 'pet-mode -10)
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (setq-local python-shell-interpreter (pet-executable-find "python")
;;                           python-shell-virtualenv-root (pet-virtualenv-root))
;;               (pet-eglot-setup)
;;               (pet-flycheck-setup))))

;;;; =========================================================
;;;;                    HIGHLIGHTING
;;;; =========================================================

;; (use-package symbol-overlay
;;   :straight t
;;   :hook
;;   ((prog-mode text-mode) . symbol-overlay-mode))

;; (use-package hl-todo
;;   :defer t
;;   :config
;;   (global-hl-todo-mode t))

;;;; =========================================================
;;;;                    EDITING HELPERS
;;;; =========================================================

;; (use-package hungry-delete
;;   :config
;;   (setq hungry-delete-join-reluctantly t)
;;   (global-hungry-delete-mode))

;;;; =========================================================
;;;;                     FILE BROWSER
;;;; =========================================================

;; (use-package neotree
;;   :config
;;   (global-set-key (kbd "<f5>") 'neotree-toggle))

;;;; =========================================================
;;;;                     TYPESCRIPT
;;;; =========================================================

;; (use-package typescript-mode :defer t :mode ("\\.ts\\'" "\\.tsx\\'"))

;; (use-package tide
;;   :after (typescript-mode)
;;   :config
;;   (tide-hl-identifier-mode 1)
;;   (eldoc-mode 1))

;;;; =========================================================
;;;;                      DOCKER
;;;; =========================================================

;; (use-package dockerfile-mode
;;   :defer t
;;   :config
;;   (require 'dockerfile-mode)
;;   (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;;;; =========================================================
;;;;                       OCAML
;;;; =========================================================

;; (add-to-list 'load-path "/Users/kiran/.opam/default/share/emacs/site-lisp")
;; (use-package ocp-indent
;;   :defer t
;;   :load-path "/Users/kiran/.opam/default/share/emacs/site-lisp"
;;   :hook (tuareg-mode . ocp-indent-caml-mode-setup)
;;   :config
;;   (setq tuareg-indent-function 'ocp-indent-line))

;;;; =========================================================
;;;;                        GIT
;;;; =========================================================

;; (use-package magit-todos
;;   :after magit
;;   :custom
;;   (magit-todos-keyword-suffix " ?([^)]+):")
;;   (magit-todos-keywords '("jww" "FIXME"))
;;   (magit-todos-exclude-globs '(".git/" "/archive/"))
;;   :config
;;   (magit-todos-mode 1))

;;;; =========================================================
;;;;                     KEY CHORDS
;;;; =========================================================

;; Alternative key-chord configuration
;; (use-package key-chord
;;   :ensure t
;;   :config
;;   (key-chord-mode +1)
;;   (setq key-chord-one-key-delay 0.185)
;;   (setq key-chord-two-keys-delay 0.1)
;;   (setq key-chord-safety-interval-backward 0.2)
;;   (setq key-chord-safety-interval-forward 0.3)
;;
;;   (key-chord-define-global "VV" 'split-window-right)
;;   (key-chord-define-global "HH" 'split-window-below)
;;   (key-chord-define-global "BB" 'switch-to-buffer)
;;   (key-chord-define-global "QQ" 'delete-window)
;;   (key-chord-define-global "11" 'delete-other-windows))

;;;; =========================================================
;;;;                      CLOJURE
;;;; =========================================================

;; Better syntax highlighting for Clojure
;; (use-package clojure-mode-extra-font-locking)

;;;; =========================================================
;;;;                    LEGACY/MISC
;;;; =========================================================

;; Ancient package - provides nothing modern Emacs doesn't have
;; (use-package font-lock+)

(provide 'config-disabled)
;;; config-disabled.el ends here
