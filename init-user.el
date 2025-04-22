(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; (defconst font "CommitMono Nerd Font Mono")
(defconst font "JetbrainsMono Nerd Font Mono")
(defconst org-code-block-font "JetBrains Mono")
(defconst org-variable-pitch-font "SF Pro")

(set-face-attribute 'default nil
                    :family font
                    :height 160
                    :weight 'normal
                    :width 'normal)

(use-package font-lock+)

(setq-default help-window-select t
              truncate-lines t
              fill-column 100)

(setq-default line-spacing 3)

;; Prevents from accidentally quitting Emacs
;; (global-unset-key (kbd "C-x C-c"))

;; workaround for alt not worqking as meta key
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'super
      mac-option-modifier 'meta
      inhibit-splash-screen t
      inhibit-startup-message t
      ring-bell-function 'ignore
      select-enable-clipboard t
      save-interprogram-paste-before-kill t
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      vc-handled-backends '(Git)
      default-file-name-coding-system 'utf-8
      buffer-file-coding-system 'utf-8
      org-ellipsis " ▶")

(set-default 'cursor-type 'box)
(blink-cursor-mode nil)

(global-hl-line-mode -1)
(set-face-attribute hl-line-face nil :underline t)

(global-set-key (kbd "s-x") 'execute-extended-command)
(global-set-key (kbd "C-g") #'kg/keyboard-quit-dwim)

;; Rewrite selected text
(delete-selection-mode 1)

(use-package spacious-padding
  :straight (spacious-padding :type git :host github :repo "protesilaos/spacious-padding")
  :demand t
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '( :internal-border-width 24
      :header-line-width 4
      :mode-line-width 10
      :tab-width 4
      :right-divider-width 30
      :scroll-bar-width 12
      :fringe-width 12)))

;; =========================================================
;;                          THEMES
;; =========================================================

(defadvice load-theme (before disable-themes-first activate)
  "Disable all active themes before loading a new theme."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(use-package catppuccin-theme
  :straight `(catppuccin-theme :type git
                               :host github
                               :repo "catppuccin/emacs"
                               :branch "main")
  :config
  (load-theme 'catppuccin t)
  (setq catppuccin-flavor 'frappe))

;; =========================================================
;;                OTHER THIRD PARTY PACKAGES
;; =========================================================

(use-package expand-region
  :config
  (global-set-key (kbd "C-;") 'er/expand-region))

(use-package rg
  :config
  (setq rg-command-line-flags '("-w"))
  (setq rg-ignore-case 'smart)

  (rg-define-search kg/grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                         ; search root project dir
             default-directory))          ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !.git")))

(use-package counsel
  :after rg
  :config
  (global-set-key (kbd "s-r") 'counsel-recentf)
  (setq counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")
  (setq recentf-max-saved-items 50)
  (setq recentf-auto-cleanup (* 24 60 60))
  (use-package flx
    :init
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))))

(use-package swiper)

(use-package multiple-cursors
  :config
  (setq-default mc/edit-lines-empty-lines 'ignore
                mc/insert-numbers-default 1)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq all-the-icons-scale-factor 1.1))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (set-face-attribute 'mode-line nil :family font :height 120)
  (set-face-attribute 'mode-line-inactive nil :family font :height 120)
  :config
  ;; Don’t compact font caches during GC.
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-github nil
        doom-modeline-version nil
        doom-modeline-height 25
        doom-modeline-bar-width 0
        doom-modeline-buffer-encoding nil
        doom-modeline-vcs-max-length 50
        doom-modeline-gnus nil
        doom-modeline-irc nil
        doom-modeline-persp-name nil
        doom-modeline-window-width-limit fill-column
        doom-modeline-lsp t))

(use-package undo-tree
  :bind ("s-Z" . 'undo-tree-redo)
  :init
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-history")))
  :config
  (global-undo-tree-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  (add-hook 'gfm-mode-hook 'display-line-numbers-mode)
  (add-hook 'markdown-mode-hook 'display-line-numbers-mode))

(use-package smartparens
  :init
  (add-hook 'text-mode-hook 'smartparens-mode)
  (add-hook 'markdown-mode-hook 'smartparens-mode)
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode t))

(use-package yaml-mode)
(use-package json-mode)

(use-package protobuf-mode
  :hook (protobuf-mode . flycheck-mode)
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))

  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t)))

  ;; Consider integrating buf using the snippet below
  ;; https://github.com/flycheck/flycheck/issues/1453#issuecomment-506598272
  )

(use-package dockerfile-mode
  :config
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package which-key
  :config
  (which-key-mode))

(use-package elixir-mode
  :hook ((elixir-mode . (lambda ()
                          (local-set-key (kbd "s-l l") 'goto-line)
                          (local-set-key (kbd "C-c C-d") 'elixir-mode-open-docs-stable)))))

;; (use-package exunit
;;   :after elixir-mode
;;   :hook ((elixir-mode . exunit-mode))
;;   :bind (("C-c t t" . 'exunit-verify-single)
;;          ("C-c t f" . 'exunit-verify)
;;          ("C-c t a" . 'exunit-verify-all)))

;; IEx REPL
;; (use-package inf-elixir
;;   :bind (("C-c i i" . 'inf-elixir)
;;          ("C-c i p" . 'inf-elixir-project)
;;          ("C-c i l" . 'inf-elixir-send-line)
;;          ("C-c i r" . 'inf-elixir-send-region)
;;          ("C-c i b" . 'inf-elixir-send-buffer)))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  ;; Make flycheck indicators more subtle
  (custom-set-faces
   '(flycheck-error ((t (:underline (:color "#742828" :style line)))))
   '(flycheck-warning ((t (:underline (:color "#742828" :style line)))))
   '(flycheck-info ((t (:underline (:color "#415141" :style line))))))

  ;; For an alternative aesthetic
  ;; (custom-set-faces
  ;;  '(flycheck-error ((t (:background "#742828"))))
  ;;  '(flycheck-warning ((t (:background "#742828"))))
  ;;  '(flycheck-info ((t (:background "#1f231f")))))

  (with-eval-after-load 'flycheck
    ;; Disable checkdoc - this checks documentation style
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

    ;; Specifically handle free variable warnings with byte-compiler options
    (setq-default flycheck-emacs-lisp-load-path 'inherit)
    (setq-default flycheck-emacs-lisp-check-form
                  "(progn (setq byte-compile-warnings '(not free-vars unresolved)) (batch-byte-compile))"))

  ;; (with-eval-after-load 'flycheck
  ;;   (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ;;   (setq-default flycheck-emacs-lisp-variables-indent-listed t)
  ;;   (add-to-list 'flycheck-emacs-lisp-variables-non-standard
  ;;                "^[a-zA-Z0-9-]+-.+$"))

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

;; ++++++++++++
;; Experimental
;; ++++++++++++

(use-package outline-indent
  :straight (outline-indent
             :type git
             :host github
             :repo "jamescherti/outline-indent.el")
  :custom
  (outline-indent-ellipsis " ▼ "))

(use-package ultra-scroll
  :straight (ultra-scroll
             :type git
             :host github
             :repo "jdtsmith/ultra-scroll"
             :branch "main")
  :init
  (setq scroll-conservatively 101       ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package csv-mode
  :straight t
  :mode "\\.csv\\'"
  :hook
  (csv-mode . csv-align-mode)
  :bind
  ("C-c C-h" . csv-header-line))

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

;;   ;; Add our backend to the list
;;   (add-to-list 'xref-backend-functions 'ripgrep-xref-backend)
;;   )

(use-package apheleia
  :straight t
  :hook (prog-mode . apheleia-mode)
  :config
  (add-to-list 'apheleia-mode-alist '(python-mode . (isort ruff)))
  (apheleia-global-mode t))

(use-package diff-hl
  :straight t
  :after magit
  :hook
  (magit-post-refresh . #'diff-hl-magit-post-refresh)
  :custom
  (diff-hl-side 'left)
  (diff-hl-margin-symbols-alist '((insert . "│")
                                  (delete . "-")
                                  (change . "│")
                                  (unknown . "?")
                                  (ignored . "i")))
  :config
  (setq vc-git-diff-switches '("--histogram"))
  (global-diff-hl-mode))

(use-package eglot-booster
  :straight (eglot-booster :type git
                           :host github
                           :repo "jdtsmith/eglot-booster"
                           :branch "main")
  :after eglot
  :config (eglot-booster-mode))

(use-package tree-sitter-langs
  :straight t)

(use-package python
  :straight (:type built-in)
  :config
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (add-hook 'python-mode-hook #'tree-sitter-hl-mode))

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

;; (use-package hl-column-mode
;;   :straight (hl-column-mode
;;              :type git
;;              :host codeberg
;;              :repo "akib/emacs-hl-column")
;;   ;; :after ef-themes
;;   ;; :custom-face
;;   ;; (hl-column ((t (:background ,(ef-themes-get-color-value 'bg-alt)))))
;;   :commands (global-hl-column-mode -1))

(use-package symbol-overlay
  :straight t
  :hook
  ((prog-mode text-mode) . symbol-overlay-mode))

(use-package indent-bars
  :straight t
  :commands (indent-bars-mode)
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-prefer-character t)
  (indent-bars-color '(highlight :face-bg t :blend 0.25))
  (indent-bars-highlight-current-depth '(:face default :blend 0.4))
  (indent-bars-color-by-depth nil))

;; (use-package eldoc-overlay
;;   :ensure t
;;   :delight eldoc-overlay-mode
;;   :custom ((eldoc-overlay-backend 'inline-docs)
;;            (eldoc-overlay-delay 5))
;;   :custom-face (inline-docs-border-face ((t (:family "Input"))))
;;   :hook (eldoc-mode . eldoc-overlay-mode))

;; (use-package eldoc-box
;;   :config
;;   (setq eldoc-box-cleanup-interval 2
;;         eldoc-box-max-pixel-height 600
;;         eldoc-box-max-pixel-widtch 800)
;;   (eldoc-box-hover-at-point-mode)
;;   (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

(set-frame-parameter (selected-frame) 'alpha '(92 92))

;; Disable these commands which have been enabled by default
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

(setq ns-use-proxy-icon nil)

;; Omit magit from native compilation
;; (setq native-comp-deferred-compilation-deny-list '("magit"))

;; Use existing frame when opening files
(setq ns-pop-up-frames nil)

;; No need to keep duplicates in prompt history.
(setq history-delete-duplicates t)

;; Allow auto revert mode to update vc information
(setq auto-revert-check-vc-info t)

;; Set ctags binary
;; Run ctags -e -R . for indexing a project
(setq ctags-path "/opt/homebrew/bin/ctags")

(use-package smart-comment
  :bind ("M-;" . smart-comment))

(use-package kotlin-mode)

(use-package hungry-delete
  :config
  (setq hungry-delete-join-reluctantly t)
  (global-hungry-delete-mode))

;; Magit requires ‘transient’ >= 0.5.0,
;; but due to bad defaults, Emacs’ package manager, refuses to
;; upgrade this and other built-in packages to higher releases
;; from GNU Elpa.

;; The configuration below must be added to fix this:
;; (setq package-install-upgrade-built-in nil)

(use-package gotest)

(defun kg/go-mode-hook ()
  (if (executable-find "goimports")
      (setq gofmt-command "goimports"))

  ;; (go-eldoc-setup)
  (subword-mode 1)
  (smartparens-mode 1)
  (add-hook 'go-mode-hook (local-set-key (kbd "C-c C-c") 'go-run))
  (add-hook 'go-mode-hook (local-set-key (kbd "C-c C-t") 'go-test-current-project)))

(add-hook 'go-mode-hook 'kg/go-mode-hook)

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

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         ;; (before-save . lsp-format-buffer)
         ;;(before-save . lsp-organize-imports)
         )
  :after exec-path-from-shell

  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

  :config
  (exec-path-from-shell-copy-env "GOROOT")
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package avy
  :config
  (global-set-key (kbd "s-h") 'avy-goto-char-timer))

;; (use-package key-chord
;;   :ensure t
;;   :config
;;   (key-chord-mode +1)
;;   (setq key-chord-one-key-delay 0.185)           ; e.g. "jj", default 0.2
;;   (setq key-chord-two-keys-delay 0.1)          ; e.g. "jk", default 0.1
;;   (setq key-chord-safety-interval-backward 0.2) ; default 0.1 is too close to key delays
;;   (setq key-chord-safety-interval-forward 0.3) ; default 0.35 causes laggy experience

;;   (key-chord-define-global "VV" 'split-window-right)
;;   (key-chord-define-global "HH" 'split-window-below)
;;   (key-chord-define-global "BB" 'switch-to-buffer)
;;   ;; (key-chord-define-global "CC" 'recenter)
;;   (key-chord-define-global "QQ" 'delete-window)
;;   (key-chord-define-global "11" 'delete-other-windows))

;;;; +++++++++++++++++++
;;;; MISC
;;;; +++++++++++++++++++

(setq load-path (cons "/Users/kiran/.asdf/installs/erlang/26.0.1/lib/tools-3.6/emacs/" load-path))
(require 'erlang-start)
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

(use-package git-timemachine)

(use-package find-file-in-project
  :config
  (setq ffip-use-rust-fd t)
  (global-set-key (kbd "s-t") 'find-file-in-project))

(use-package neotree
  :config
  (global-set-key (kbd "<f5>") 'neotree-toggle))

(use-package editorconfig)
(use-package jsonrpc)
(use-package transient)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; =========================================================
;;                        LLM
;; =========================================================

;; (use-package llm
;;   :straight (:host github :repo "ahyatt/llm"))

(use-package ellama
  :straight (:host github :repo "s-kostyaev/ellama")
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e")
  (require 'llm-ollama)
  (setopt ellama-provider
	        (make-llm-ollama :chat-model "llama3.1:latest")))

(use-package minuet
  :init
  ;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  :config
  (setq minuet-provider 'openai-fim-compatible)
  (setq minuet-n-completions 1) ; recommended for Local LLM for resource saving
  (setq minuet-context-window 512)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:3b")
  ;; Use this when running llm with llama.cpp
  ;;
  ;; llama-server \
  ;;   -hf ggml-org/Qwen2.5-Coder-1.5B-Q8_0-GGUF \
  ;;   --port 8012 -ngl 99 -fa -ub 1024 -b 1024 \
  ;;   --ctx-size 0 --cache-reuse 256
  ;;
  ;; (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:8012/v1/completions")
  ;; (plist-put minuet-openai-fim-compatible-options :name "Llama.cpp")
  ;; (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  ;; (plist-put minuet-openai-fim-compatible-options :model "PLACEHOLDER")
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

;; TODO: This needs an auth key to use
;; (use-package codeium
;;   :straight '(:type git :host github :repo "Exafunction/codeium.el")
;;   :init
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (setq-local completion-at-point-functions '(codeium-completion-at-point))))
;;   ;; codeium-completion-at-point is autoloaded, but you can
;;   ;; optionally set a timer, which might speed up things as the
;;   ;; codeium local language server takes ~0.2s to start up
;;   (add-hook 'emacs-startup-hook
;;             (lambda () (run-with-timer 0.1 nil #'codeium-init)))
;;   :config
;;   (setq use-dialog-box nil) ;; do not use popup boxes

;;   ;; if you don't want to use customize to save the api-key
;;   ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

;;   ;; get codeium status in the modeline
;;   (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;   (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;;   ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;   (setq codeium-api-enabled
;;         (lambda (api)
;;           (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))

;;   ;; limit string sent to codeium for better performance
;;   (defun my-codeium/document/text ()
;;     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))

;;   ;; if you change the text, you should also change the cursor_offset
;;   ;; warning: this is measured by UTF-8 encoded bytes
;;   (defun my-codeium/document/cursor_offset ()
;;     (codeium-utf8-byte-length
;;      (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;;   (setq codeium/document/text 'my-codeium/document/text)
;;   (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  ;;(setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "qwen2.5-coder:1.5b")
  (aidermacs-architect-model "deepcoder:latest"))

;; (use-package gptel
;;   :config
;;   (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
;;   (gptel-make-ollama "Ollama"
;;           :host "localhost:11434"
;;           :stream t
;;           :models '(opencoder:latest)))

;; =========================================================
;;                           EFUNS
;; =========================================================

(defun kg/iex ()
  (interactive)
  (term "iex"))

(defun kg/no-copilot-mode ()
  "Helper for `kg/no-copilot-modes'."
  (copilot-mode -1))

(defvar kg/no-copilot-modes '(shell-mode
                              inferior-python-mode
                              eshell-mode
                              term-mode
                              vterm-mode
                              comint-mode
                              compilation-mode
                              debugger-mode
                              dired-mode-hook
                              compilation-mode-hook
                              flutter-mode-hook
                              minibuffer-mode-hook
                              shell-script-modes)
  "Modes in which copilot is inconvenient.")

(defun kg/copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (or (member major-mode kg/no-copilot-modes)))

(add-to-list 'copilot-disable-predicates #'kg/copilot-disable-predicate)

(defun kg/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
                     (setq copilot-disable-predicates (list (lambda () t)))
                     (copilot-clear-overlay)
                     (run-with-idle-timer
                      1.0
                      nil
                      (lambda ()
                        (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(advice-add 'keyboard-quit :before #'kg/copilot-quit)

;; Load custom functions
(load "/Users/kiran/.emacs.d/init-efuns.el")

;; =========================================================
;;                      GLOBAL KEYBINDINGS
;; =========================================================

(global-set-key (kbd "s-g") 'kg/search-marked-region-if-available)
(global-set-key (kbd "s-l") 'consult-line)
(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x 3") 'kg/split-right-and-move)
(global-set-key (kbd "C-x 2") 'kg/split-below-and-move)
(global-set-key (kbd "C-a") 'kg/beginning-of-line-dwim)
(global-set-key [(meta shift down)] 'kg/duplicate-start-of-line-or-region)
(global-set-key (kbd "<f6>") 'kg/show-user-config)
(global-set-key (kbd "s-w") 'kill-current-buffer)
(global-set-key (kbd "<s-S-return>") 'kg/toggle-maximize-buffer)


;; =============== ADD THEME FOLDER =========================
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
