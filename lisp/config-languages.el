;;; config-languages.el --- Programming languages -*- lexical-binding: t; -*-

;;; Commentary:
;; Language-specific modes and configurations.

;;; Code:

;;;; =========================================================
;;;;                        LISP FAMILY
;;;; =========================================================

;;;; Clojure Mode
(use-package clojure-mode
  :defer t
  :hook ((clojure-mode . subword-mode))
  :config
  ;; Treat hyphens as a word character when transposing words
  (defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
    (let ((st (make-syntax-table clojure-mode-syntax-table)))
      (modify-syntax-entry ?- "w" st)
      st))

  (defun transpose-words-with-hyphens (arg)
    "Treat hyphens as a word character when transposing words"
    (interactive "*p")
    (with-syntax-table clojure-mode-with-hyphens-as-word-sep-syntax-table
      (transpose-words arg)))

  (define-key clojure-mode-map (kbd "M-t") 'transpose-words-with-hyphens))

;;;; Cider
(use-package cider
  :after clojure-mode
  :defer t
  :init
  (setq cider-repl-pop-to-buffer-on-connect nil
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t
        cider-repl-history-size 100
        cider-docview-fill-column 70
        cider-stacktrace-fill-column 76
        nrepl-popup-stacktraces nil
        nrepl-log-messages nil
        nrepl-hide-special-buffers t
        cider-repl-use-pretty-printing t
        cider-repl-result-prefix ";; => "
        cider-repl-display-help-banner nil
        cider-font-lock-dynamically '(macro core function var)
        ;; Use LSP for the following
        cider-font-lock-dynamically nil
        cider-eldoc-display-for-symbol-at-point nil
        cider-prompt-for-symbol nil
        cider-use-xref nil)

  :custom
  (cider-preferred-build-tool 'clj)
  :config
  ;; Use clojure-lsp with Eglot for this
  (remove-hook 'eldoc-documentation-functions #'cider-eldoc)
  (remove-hook 'completion-at-point-functions #'cider-complete-at-point)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-mode-hook
            (lambda ()
              (local-set-key (kbd "C-l") 'cider-repl-clear-buffer)
              (local-set-key (kbd "<C-return>") 'cider-eval-last-sexp)
              (local-set-key (kbd "C-c C-n") 'cider-eval-buffer)
              (local-set-key (kbd "C-x C-i") 'cider-inspect-last-sexp))))

;;;; Clj-refactor
(use-package clj-refactor
  :after clojure-mode
  :config
  (setq cljr-warn-on-eval nil
        cljr-eagerly-build-asts-on-startup nil))

;;;; Flycheck Clj-kondo
(use-package flycheck-clj-kondo
  :defer t
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (require 'flycheck-clj-kondo))))

;;;; =========================================================
;;;;                        GO
;;;; =========================================================

(use-package gotest)

(defun kg/go-mode-hook ()
  (if (executable-find "goimports")
      (setq gofmt-command "goimports"))
  (subword-mode 1)
  (smartparens-mode 1)
  (add-hook 'go-mode-hook (local-set-key (kbd "C-c C-c") 'go-run))
  (add-hook 'go-mode-hook (local-set-key (kbd "C-c C-t") 'go-test-current-project)))

(add-hook 'go-mode-hook 'kg/go-mode-hook)

(use-package go-mode
  :hook ((go-mode . eglot-ensure))
  :after exec-path-from-shell
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  :config
  (exec-path-from-shell-copy-env "GOROOT")
  (exec-path-from-shell-copy-env "GOPATH"))

;;;; =========================================================
;;;;                        RUBY
;;;; =========================================================

(use-package ruby-mode
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :interpreter "ruby"
  :hook ((ruby-mode . smartparens-strict-mode))
  :init
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  (add-hook 'ruby-mode 'superword-mode))

;;;; =========================================================
;;;;                        PYTHON
;;;; =========================================================

(use-package python :straight (:type built-in))

;; python-ts-mode has some syntax highlighting issues.
(add-to-list 'major-mode-remap-alist '(python-ts-mode . python-mode))

;;;; =========================================================
;;;;                        OCAML
;;;; =========================================================

(use-package ocamlformat)

(use-package tuareg
  :defer t
  :after ocamlformat
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)
         ("\\.ocamlinit\\'" . tuareg-mode))
  :hook ((tuareg-mode . (lambda ()
                          (eglot-ensure)
                          (eldoc-mode 1)
                          (eldoc-box-hover-at-point-mode 1)
                          (setq-local eldoc-idle-delay 0.2)))))

(use-package merlin-eldoc
  :ensure t
  :hook ((reason-mode tuareg-mode caml-mode) . merlin-eldoc-setup))

(use-package ocaml-eglot
  :ensure t
  :after tuareg
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))

;;;; =========================================================
;;;;                        ELIXIR
;;;; =========================================================

(use-package elixir-mode
  :hook ((elixir-mode . (lambda ()
                          (local-set-key (kbd "s-l l") 'goto-line)
                          (local-set-key (kbd "C-c C-d") 'elixir-mode-open-docs-stable)))))

;;;; =========================================================
;;;;                        NIX
;;;; =========================================================

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package company-nixos-options
  :ensure t
  :after nix-mode
  :config
  (add-to-list 'company-backends 'company-nixos-options))

;;;; =========================================================
;;;;                        ERLANG
;;;; =========================================================

;; Dynamic path detection for Erlang
(when-let ((erlang-emacs-tools
            (car (file-expand-wildcards
                  (expand-file-name "~/.asdf/installs/erlang/*/lib/tools-*/emacs")))))
  (add-to-list 'load-path erlang-emacs-tools))

(when (locate-library "erlang-start")
  (require 'erlang-start)
  (add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode)))

;;;; =========================================================
;;;;                        TERRAFORM
;;;; =========================================================

(use-package terraform-mode)

;;;; =========================================================
;;;;                        DATA FORMATS
;;;; =========================================================

(use-package yaml-mode :defer t)
(use-package json-mode :defer t)

(use-package protobuf-mode
  :defer t
  :hook (protobuf-mode . flycheck-mode)
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))

  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t))))

;;;; =========================================================
;;;;                        MARKDOWN
;;;; =========================================================

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc -f markdown_github+smart")
  :hook (markdown-mode . visual-line-mode)
  :config
  (setq markdown-header-scaling t)
  (add-hook 'gfm-mode-hook 'display-line-numbers-mode)
  (add-hook 'markdown-mode-hook 'display-line-numbers-mode))

;;;; =========================================================
;;;;                        TREE-SITTER
;;;; =========================================================

(when (treesit-available-p)
  (setq treesit-font-lock-level 3)  ; Level 3 for performance
  (use-package treesit-auto
    :ensure t
    :config
    (setq treesit-auto-install 'prompt)
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode 1)))

(provide 'config-languages)
;;; config-languages.el ends here
