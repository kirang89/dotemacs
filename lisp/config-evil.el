;;; config-evil.el --- Evil mode (Vim emulation) -*- lexical-binding: t; -*-

;;; Commentary:
;; Modal editing via evil-mode with SPC leader key, surround motions,
;; and comprehensive keybindings via general.el.

;;; Code:

;;;; Evil Core
(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil          ; Let evil-collection handle non-file buffers
        evil-want-C-u-scroll t            ; C-u = page up (vim style)
        evil-want-Y-yank-to-eol t         ; Y yanks to EOL (like D deletes to EOL)
        evil-undo-system 'undo-redo       ; Native Emacs 28+ undo/redo
        evil-split-window-below t         ; Split below and move
        evil-vsplit-window-right t        ; Split right and move
        evil-respect-visual-line-mode t
        evil-want-fine-undo t)            ; More granular undo in insert mode
  :config
  (evil-mode 1)

  ;; Display full state names in modeline
  (setq evil-normal-state-tag   " NORMAL "
        evil-insert-state-tag   " INSERT "
        evil-visual-state-tag   " VISUAL "
        evil-replace-state-tag  " REPLACE "
        evil-emacs-state-tag    " EMACS "
        evil-motion-state-tag   " MOTION "
        evil-operator-state-tag " OPERATOR ")

  ;; Keep C-g as kg/keyboard-quit-dwim in all states
  (define-key evil-normal-state-map (kbd "C-g") #'kg/keyboard-quit-dwim)
  (define-key evil-insert-state-map (kbd "C-g") #'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-g") #'kg/keyboard-quit-dwim)

  ;; Keep M-. / M-, for xref navigation
  (define-key evil-normal-state-map (kbd "M-.") #'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "M-,") #'xref-go-back)

  ;; Keep C-a for beginning-of-line-dwim
  (define-key evil-insert-state-map (kbd "C-a") #'kg/beginning-of-line-dwim)
  (define-key evil-normal-state-map (kbd "C-a") #'kg/beginning-of-line-dwim)

  ;; Keep M-j for join-line
  (define-key evil-normal-state-map (kbd "M-j") (lambda () (interactive) (join-line -1)))

  ;; Embark act (C-. works in all states)
  (define-key evil-normal-state-map (kbd "C-.") #'embark-act)
  (define-key evil-insert-state-map (kbd "C-.") #'embark-act)
  (define-key evil-visual-state-map (kbd "C-.") #'embark-act)

  ;; Visual state: v/V for expand/contract region
  (define-key evil-visual-state-map (kbd "v") #'er/expand-region)
  (define-key evil-visual-state-map (kbd "V") #'er/contract-region)

  ;; Make :q/:wq operate on buffers, not Emacs
  (evil-ex-define-cmd "q" #'kill-current-buffer)
  (evil-ex-define-cmd "wq" (lambda () (interactive) (save-buffer) (kill-current-buffer)))
  (evil-ex-define-cmd "qa" #'save-buffers-kill-terminal))

;;;; Evil Collection
(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))

;;;; Evil Surround
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;;;; Evil Commentary
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;;;; Evil Matchit
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

;;;; Evil Paredit
(use-package evil-paredit
  :after (evil paredit)
  :hook ((emacs-lisp-mode . evil-paredit-mode)
         (clojure-mode . evil-paredit-mode)))

;;;; Evil Snipe (2-char search replacing f/t)
(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

;;;; Evil MC (Multiple Cursors)
(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1))

;;;; General.el (Leader Key Framework)
(use-package general
  :demand t
  :config
  ;; SPC as leader in normal/visual states
  (general-create-definer kg/leader-keys
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; , as local leader
  (general-create-definer kg/local-leader-keys
    :states '(normal visual)
    :keymaps 'override
    :prefix ",")

  ;; --- Leader Bindings ---

  (kg/leader-keys
    "SPC" '(find-file-in-project :wk "find file in project")
    "x"   '(execute-extended-command :wk "M-x"))

  ;; Buffer
  (kg/leader-keys
    "b"  '(:ignore t :wk "buffer")
    "bb" '(consult-buffer :wk "switch buffer")
    "bd" '(kill-current-buffer :wk "kill buffer")
    "bn" '(next-buffer :wk "next buffer")
    "bp" '(previous-buffer :wk "prev buffer")
    "bi" '(ibuffer :wk "ibuffer")
    "bR" '(rename-buffer :wk "rename buffer"))

  ;; File
  (kg/leader-keys
    "f"  '(:ignore t :wk "file")
    "ff" '(find-file-in-project :wk "find file in project")
    "fF" '(find-file :wk "find file")
    "fr" '(consult-recent-file :wk "recent files")
    "fs" '(save-buffer :wk "save file")
    "fc" '((lambda () (interactive) (find-file user-init-file)) :wk "open config")
    "fR" '(kg/rename-this-buffer-and-file :wk "rename file")
    "fD" '(kg/delete-this-buffer-and-file :wk "delete file"))

  ;; Window
  (kg/leader-keys
    "w"  '(:ignore t :wk "window")
    "ww" '(ace-window :wk "ace window")
    "ws" '(kg/split-below-and-move :wk "split below")
    "wv" '(kg/split-right-and-move :wk "split right")
    "wd" '(delete-window :wk "delete window")
    "wo" '(delete-other-windows :wk "delete other windows")
    "wm" '(kg/toggle-maximize-buffer :wk "maximize")
    "wh" '(windmove-left :wk "move left")
    "wj" '(windmove-down :wk "move down")
    "wk" '(windmove-up :wk "move up")
    "wl" '(windmove-right :wk "move right"))

  ;; Project
  (kg/leader-keys
    "p" '(:keymap projectile-command-map :package projectile :wk "project"))

  ;; Goto
  (kg/leader-keys
    "g"  '(:ignore t :wk "goto")
    "gd" '(xref-find-definitions :wk "definition")
    "gi" '(eglot-find-implementation :wk "implementation")
    "gr" '(xref-find-references :wk "references")
    "gt" '(eglot-find-typeDefinition :wk "type definition")
    "gl" '(consult-goto-line :wk "goto line")
    "gb" '(xref-go-back :wk "go back"))

  ;; Magit / Git
  (kg/leader-keys
    "m"  '(:ignore t :wk "magit")
    "mm" '(magit-status :wk "status")
    "mc" '(magit-checkout :wk "checkout")
    "mP" '(kg/git-pull-rebase-autostash :wk "pull rebase")
    "md" '(magit-diff-buffer-file :wk "diff file")
    "ml" '(magit-log-buffer-file :wk "log file")
    "mb" '(magit-blame :wk "blame")
    "ms" '(magit-stage-file :wk "stage file")
    "mu" '(magit-unstage-file :wk "unstage file")
    "mt" '(git-timemachine :wk "timemachine"))

  ;; Search
  (kg/leader-keys
    "s"  '(:ignore t :wk "search")
    "ss" '(kg/consult-line-with-region :wk "search line")
    "sl" '(kg/consult-line-with-region :wk "search line")
    "sp" '(kg/consult-ripgrep-with-region :wk "ripgrep project")
    "sr" '(kg/consult-ripgrep-with-region :wk "ripgrep project")
    "sg" '(kg/search-marked-region-if-available :wk "ripgrep region")
    "si" '(consult-imenu :wk "imenu")
    "so" '(consult-outline :wk "outline")
    "sR" '(query-replace-regexp :wk "query replace"))

  ;; Code
  (kg/leader-keys
    "c"  '(:ignore t :wk "code")
    "cd" '(duplicate-dwim :wk "duplicate")
    "cf" '(apheleia-format-buffer :wk "format")
    "co" '(symbols-outline-toggle :wk "outline")
    "cR" '(eglot-rename :wk "rename")
    "ca" '(eglot-code-actions :wk "code actions"))

  ;; AI
  (kg/leader-keys
    "a"  '(:ignore t :wk "ai")
    "aa" '(aidermacs-transient-menu :wk "aidermacs")
    "ae" '(ellama-transient-main-menu :wk "ellama"))

  ;; Jump
  (kg/leader-keys
    "j"  '(:ignore t :wk "jump")
    "jj" '(avy-goto-char-timer :wk "avy char")
    "jl" '(consult-goto-line :wk "goto line")
    "ji" '(consult-imenu :wk "imenu"))

  ;; Help
  (kg/leader-keys
    "h"  '(:ignore t :wk "help")
    "hf" '(helpful-callable :wk "describe function")
    "hv" '(helpful-variable :wk "describe variable")
    "hk" '(helpful-key :wk "describe key")
    "hx" '(helpful-command :wk "describe command"))

  ;; Toggle
  (kg/leader-keys
    "t"  '(:ignore t :wk "toggle")
    "tl" '(display-line-numbers-mode :wk "line numbers")
    "tw" '(whitespace-mode :wk "whitespace")
    "tf" '(flycheck-mode :wk "flycheck"))

  ;; Quit
  (kg/leader-keys
    "q"  '(:ignore t :wk "quit")
    "qq" '(save-buffers-kill-terminal :wk "quit emacs"))

  ;; Open
  (kg/leader-keys
    "o"  '(:ignore t :wk "open")
    "od" '(dired-jump :wk "dired")
    "ot" '(kg/open-terminal-in-new-frame :wk "terminal"))

  ;; --- Local Leader (Emacs Lisp) ---
  (kg/local-leader-keys
    :keymaps 'emacs-lisp-mode-map
    "e"  '(:ignore t :wk "eval")
    "ee" '(eval-last-sexp :wk "eval last sexp")
    "er" '(kg/eval-root-expression :wk "eval root")
    "eb" '(eval-buffer :wk "eval buffer")))

;;;; VTerm
(use-package vterm
  :defer t
  :config
  (setq vterm-max-scrollback 10000))

;;;; Perspective (Workspace Management)
(use-package perspective
  :demand t
  :custom
  (persp-mode-prefix-key (kbd "C-c w"))
  (persp-sort 'created)
  (persp-show-modestring nil)  ; Use doom-modeline instead
  :init
  (persp-mode)
  :config
  ;; Integrate with consult for buffer switching
  (with-eval-after-load 'consult
    ;; Hide default buffer source in favor of perspective-filtered buffers
    (when (boundp 'consult-source-buffer)
      (plist-put consult-source-buffer :hidden t)
      (plist-put consult-source-buffer :default nil))
    (add-to-list 'consult-buffer-sources persp-consult-source)))

;; Leader bindings for perspective (SPC l for "layout")
(with-eval-after-load 'general
  (kg/leader-keys
    "l"  '(:ignore t :wk "layout/workspace")
    "ll" '(persp-switch :wk "switch/create")
    "ln" '(persp-next :wk "next")
    "lp" '(persp-prev :wk "previous")
    "ld" '(persp-kill :wk "delete")
    "lr" '(persp-rename :wk "rename")
    "lb" '(persp-switch-to-buffer* :wk "switch buffer")
    "l1" '((lambda () (interactive) (persp-switch-by-number 1)) :wk "workspace 1")
    "l2" '((lambda () (interactive) (persp-switch-by-number 2)) :wk "workspace 2")
    "l3" '((lambda () (interactive) (persp-switch-by-number 3)) :wk "workspace 3")
    "l4" '((lambda () (interactive) (persp-switch-by-number 4)) :wk "workspace 4")
    "l5" '((lambda () (interactive) (persp-switch-by-number 5)) :wk "workspace 5")
    "l6" '((lambda () (interactive) (persp-switch-by-number 6)) :wk "workspace 6")
    "l7" '((lambda () (interactive) (persp-switch-by-number 7)) :wk "workspace 7")
    "l8" '((lambda () (interactive) (persp-switch-by-number 8)) :wk "workspace 8")
    "l9" '((lambda () (interactive) (persp-switch-by-number 9)) :wk "workspace 9")))

(provide 'config-evil)
;;; config-evil.el ends here
