;; A minimial setup for Clojurians

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000
      gc-cons-percentage 0.6)


;; Automatically reload files when they change on disk
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(setq package-enable-at-startup nil)

;; Bootstrap configuration for straight.el
(setq straight-use-package-by-default t
      ;;      use-package-always-defer t
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1
      vc-follow-symlinks t
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-recipes-emacsmirror-use-mirror nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Move custom configuration variables set by Emacs, to a seperate temporary file
;; (setq custom-file "~/.emacs.d/custom.el")
(setq custom-file (make-temp-file "emacs-custom-"))
(load custom-file 'noerror)

;;
;; APPEARANCE
;;

;; prevent the glimpse of unstyled UI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; initial starting position
(push '(height . 43) default-frame-alist)
(push '(width . 130) default-frame-alist)
(push '(left . 70) default-frame-alist)
(push '(top . 30) default-frame-alist)

;; disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      comp-deferred-compilation nil
      frame-resize-pixelwise t
      cursor-in-non-selected-windows nil
      site-run-file nil
      system-time-locale "en_US.utf8"
      initial-major-mode 'markdown-mode
      initial-scratch-message nil)

;; Make the title bar blend with the background color
;; Set the appearance to light/dark depending on your theme
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

;; Show full file path in the title bar
(setq
 frame-title-format
 '((:eval (if (buffer-file-name)
              (abbreviate-file-name (buffer-file-name))
            "%b"))))

;; Line numbers
;; Add some padding when displaying line numbers
(setq linum-format "%5d ")
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;
;; SANE DEFAULTS
;;

;; Make native compilation silent and prune its cache.
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-compile-prune-cache t)

;; Use y/n instead of full yes/no for confirmation messages
(fset 'yes-or-no-p 'y-or-n-p)

;; Use spaces instead of tabs
(setq tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(setq-default c-basic-indent 2)

;; Type over selected text
(delete-selection-mode 1)

;; Kill whole line
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)

;; Use Cmd for movement
(global-set-key (kbd "s-<right>") (kbd "C-e"))  ;; End of line
(global-set-key (kbd "s-<left>") (kbd "C-a"))   ;; Beginning of line


;; Kills the current buffer without displaying the annoying menu.
;; A confirmation will be asked for, if the buffer has been modified
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Follow compilation buffer output
(setq compilation-scroll-output t)

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package clojure-mode)

(require 'clojure-mode)
;; Then define your custom syntax table
;; (defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
;;   (let ((st (make-syntax-table clojure-mode-syntax-table)))
;;     (modify-syntax-entry 45 "w" st)
;;     st))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package vertico
  :custom
  (vertico-cycle nil)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)

  :config
  (vertico-mode))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package consult
  :bind (("s-b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g l" . consult-line)
         ("M-g f" . consult-find)
         ("s-l"   . consult-line)
         ("C-x 4 b" . consult-buffer-other-window))

  :config
  (setq consult-find-args "find . -not ( -path */.[A-Za-z]* -prune )")

  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package nerd-icons-corfu)

(use-package corfu
  :straight (corfu :repo "minad/corfu" :branch "main" :files (:defaults "extensions/*.el"))
  :demand t
  :config
  (defun corfu-complete-and-quit ()
    (interactive)
    (corfu-complete)
    (corfu-quit))
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (global-corfu-mode 1)
  (corfu-popupinfo-mode +1)
  :bind  (:map corfu-map
               ("TAB" . corfu-next)
               ([tab] . corfu-next)
               ("RET" . corfu-complete-and-quit)
               ("<return>" . corfu-complete-and-quit)
               ([remap completion-at-point] . corfu-complete))
  :custom
  (corfu-auto t)
  (corfu-cycle nil)
  (corfu-count 9)
  (corfu-on-exact-match 'quit)
  (corfu-preselect-first t)
  (corfu-quit-at-boundary 'separator)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5))

(use-package cape
  :demand t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Install and setup company-mode for autocompletion
;; (use-package company
;;   :bind (("C-p" . company-select-previous)
;;          ("C-n" . company-select-next))
;;   :init
;;   (add-hook 'prog-mode-hook 'company-mode)
;;   :config
;;   (global-company-mode)
;;   (setq company-tooltip-limit 10)
;;   (setq company-idle-delay 0.2)
;;   (setq company-echo-delay 0)
;;   (setq company-minimum-prefix-length 2)
;;   (setq company-require-match nil)
;;   (setq company-selection-wrap-around t)
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-tooltip-flip-when-above nil)
;;   (setq company-dabbrev-ignore-case nil
;;         company-dabbrev-downcase nil)
;;   ;; weight by frequency
;;   (setq company-transformers '(company-sort-by-occurrence)))

;; Better syntax highlighting
;;(use-package clojure-mode-extra-font-locking)

;; Highlight matching parentheses
(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 0.125)
(set-face-background 'show-paren-match (face-background 'default))
(if (eq (frame-parameter nil 'background-mode) 'dark)
    (set-face-foreground 'show-paren-match "red")
  (set-face-foreground 'show-paren-match "black"))
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)


;; Add ability to shift between buffers using shift+arrow keys.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; Paredit makes it easier to navigate/edit s-expressions as blocks.
(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (paredit-mode)
              (local-set-key (kbd "<C-s-right>") #'paredit-forward-slurp-sexp)
              (local-set-key (kbd "<C-s-left>") #'paredit-forward-barf-sexp))))


;; To add some colors to those boring parens
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package clojure-mode
  :straight (:type built-in)
  :config

  ;;Treat hyphens as a word character when transposing words
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

(define-clojure-indent
 (defrecord 1)
 (as-> 2))

;; Cider integrates a Clojure buffer with a REPL
(use-package cider
  :init
  ;; (add-to-list 'auto-mode-alist '("\\.clj\\'" . go-mode))
  (setq cider-repl-pop-to-buffer-on-connect t
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t
        cider-repl-history-size 100
        cider-repl-use-clojure-font-lock t
        cider-docview-fill-column 70
        cider-stacktrace-fill-column 76
        nrepl-popup-stacktraces nil
        nrepl-log-messages nil
        nrepl-hide-special-buffers t
        cider-repl-use-pretty-printing t
        cider-repl-result-prefix ";; => "
        cider-repl-display-help-banner nil
        cider-font-lock-dynamically '(macro core function var))

  :config
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (local-set-key (kbd "C-l") 'cider-repl-clear-buffer)))
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook
            (lambda ()
              (local-set-key (kbd "<C-return>") 'cider-eval-last-sexp)
              (local-set-key (kbd "C-c C-n") 'cider-eval-buffer)
              (local-set-key (kbd "C-x C-i") 'cider-inspect-last-sexp))))


;; Adds some niceties/refactoring support
(use-package clj-refactor
  :config
  (setq cljr-warn-on-eval nil)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1))))

(use-package flycheck-clj-kondo
  :config
  (remove-hook 'clojure-mode-hook
               (lambda ()
                 (require 'flycheck-clj-kondo))))

;; Aggressively indents your clojure code
(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))

;; Operate (list, search, replace....) on files at a project level.
(use-package projectile
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map))
  :init
  (setq-default projectile-cache-file
                (expand-file-name ".projectile-cache" user-emacs-directory))
  (add-hook 'prog-mode-hook #'projectile-mode)
  :config
  (projectile-mode)
  ;; (setq projectile-completion-system 'ivy)
  (setq-default projectile-enable-caching t
                projectile-mode-line-prefix ""
                projectile-sort-order 'recentf
                ;; Show project (if any) name in modeline
                projectile-mode-line '(:eval (projectile-project-name))))

;; Magit: The only git interface you'll ever need
(use-package magit
  :bind ("C-x g" . 'magit-status)
  :config
  (setq magit-set-upstream-on-push 'askifnotset
        magit-push-always-verify nil
        magit-revert-buffers 'silent
        magit-diff-highlight-indentation nil
        magit-diff-paint-whitespace nil
        magit-diff-highlight-trailing nil))

;; User customizations
;; Add your customizations to `init-user.el`
(when (file-exists-p "~/.emacs.d/init-user.el")
  (setq user-custom-file "~/.emacs.d/init-user.el")
  (load user-custom-file)
  (put 'downcase-region 'disabled nil))
(put 'narrow-to-region 'disabled nil)

(require 'server)
(unless (server-running-p)
  (server-start))

;; Reset GC to reasonable defaults
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))
