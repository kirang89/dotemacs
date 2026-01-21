;;; init.el --- Emacs initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Bootstrap straight.el and load modular configuration.

;;; Code:

;;;; ==========================================================
;;;;                    STRAIGHT.EL BOOTSTRAP
;;;; ==========================================================

(setq straight-built-in-pseudo-packages
      '(emacs project eglot xref cl-lib eldoc flymake repeat jsonrpc))

(setq straight-use-package-by-default t
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1
      vc-follow-symlinks t
      straight-check-for-modifications '(check-on-save find-when-checking))

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

;; use-package is built-in since Emacs 29, but we need straight.el integration
(straight-use-package 'use-package)
(setq use-package-always-ensure nil)

;;;; ==========================================================
;;;;                    CUSTOM FILE
;;;; ==========================================================

(setq custom-file (make-temp-file "emacs-custom-"))
(load custom-file 'noerror)

;;;; ==========================================================
;;;;                    SANE DEFAULTS
;;;; ==========================================================

;; Coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq default-file-name-coding-system 'utf-8
      buffer-file-coding-system 'utf-8)

;; Use y/n instead of yes/no
(setq use-short-answers t)

;; General settings
;; Disable cursor blinking (saves redisplay cycles)
(blink-cursor-mode -1)

(setq ring-bell-function 'ignore
      use-dialog-box t
      use-file-dialog nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      cursor-in-non-selected-windows nil
      site-run-file nil
      system-time-locale "en_US.utf8"
      initial-scratch-message nil
      create-lockfiles nil
      select-enable-clipboard t
      save-interprogram-paste-before-kill t
      confirm-nonexistent-file-or-buffer nil)

;; Follow compilation buffer output
(setq compilation-scroll-output t)

;; No time estimates
(setq history-delete-duplicates t)

;; Native compilation (Emacs 28+)
(when (native-comp-available-p)
  (setq native-comp-deferred-compilation t
        package-native-compile t))
(setq native-comp-async-report-warnings-errors 'silent
      native-compile-prune-cache t
      warning-suppress-types '((comp)))

;; Long line performance (Emacs 29+)
(setq long-line-threshold 10000
      large-hscroll-threshold 10000
      syntax-wholeline-max 10000)

;; JIT-lock: defer fontification during typing for snappier input
(setq jit-lock-defer-time 0.05            ; Wait 50ms before coloring (imperceptible)
      jit-lock-stealth-time 1.5           ; Color off-screen text after 1.5s idle
      jit-lock-stealth-nice 0.5           ; Be gentle during background fontification
      jit-lock-chunk-size 2000            ; Smaller chunks = less blocking
      jit-lock-stealth-load 100)          ; Only fontify when CPU load < 100%

;; Check for native JSON
(unless (functionp 'json-serialize)
  (message "Native JSON is *not* available, LSP performance will suffer..."))

;; macOS key modifiers
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'super
      mac-option-modifier 'meta)

;; Help window behavior
(setq-default help-window-select t
              truncate-lines t
              fill-column 100
              line-spacing 1)

;; Silence the warning when killing a buffer with a live process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;;;; ==========================================================
;;;;                    LOAD PATH
;;;; ==========================================================

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;;; ==========================================================
;;;;                    LOAD MODULES
;;;; ==========================================================

;; Custom functions (used by other modules)
(load (expand-file-name "init-efuns.el" user-emacs-directory))

;; Core infrastructure
(require 'config-tools)

;; UI and appearance
(require 'config-ui)

;; Completion framework
(require 'config-completion)

;; Search and navigation
(require 'config-search)

;; File and project management
(require 'config-files)

;; Editing enhancements
(require 'config-editing)

;; Version control
(require 'config-vcs)

;; LSP and code intelligence
(require 'config-lsp)

;; Programming languages
(require 'config-languages)

;; AI assistants
(require 'config-ai)

;;;; ==========================================================
;;;;                    GLOBAL KEYBINDINGS
;;;; ==========================================================

;; Movement and editing
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)
(global-set-key (kbd "s-<right>") (kbd "C-e"))
(global-set-key (kbd "s-<left>") (kbd "C-a"))
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "s-x") 'execute-extended-command)
(global-set-key (kbd "C-g") #'kg/keyboard-quit-dwim)

;; Window management
(global-set-key (kbd "C-x 3") 'kg/split-right-and-move)
(global-set-key (kbd "C-x 2") 'kg/split-below-and-move)
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<s-S-return>") 'kg/toggle-maximize-buffer)

;; Custom functions
(global-set-key (kbd "C-a") 'kg/beginning-of-line-dwim)
(global-set-key [(meta shift down)] 'kg/duplicate-start-of-line-or-region)
(global-set-key (kbd "<f6>") 'kg/show-user-config)

;; Duplicate line or region (Emacs 29+)
(global-set-key (kbd "C-c d") 'duplicate-dwim)

;; Search and navigation
(global-set-key (kbd "s-l") 'consult-goto-line)
(global-set-key (kbd "s-f") 'isearch-forward)
(global-set-key (kbd "s-r") 'query-replace-regexp)
(global-set-key (kbd "s-w") 'kill-current-buffer)

;; Copy line if no region
(advice-add 'ns-copy-including-secondary :around #'kg/copy-line-if-no-region)

;; Elisp eval
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") #'kg/eval-root-expression)))

;;;; ==========================================================
;;;;                    DISABLED COMMANDS
;;;; ==========================================================

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Disable these commands
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

;;;; ==========================================================
;;;;                    SERVER
;;;; ==========================================================

(require 'server)
(unless (server-running-p)
  (server-start))

;;;; ==========================================================
;;;;                    CLEANUP
;;;; ==========================================================

;; Reset GC after startup (fallback if GCMH fails to load)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

(provide 'init)
;;; init.el ends here
