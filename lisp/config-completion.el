;;; config-completion.el --- Completion framework -*- lexical-binding: t; -*-

;;; Commentary:
;; Vertico-based minibuffer completion and Corfu for in-buffer completion.

;;; Code:

;;;; Minibuffer Settings
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;;; Vertico
(use-package vertico
  :custom
  (vertico-scroll-margin 0)
  (vertico-cycle nil)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (vertico-mode))

;;;; Marginalia
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;;;; Consult
(use-package consult
  :bind (("s-b" . consult-buffer)
         ("s-g" . #'kg/consult-ripgrep-with-region)
         ("M-g f" . #'kg/fast-grep)
         ("M-g g" . #'kg/fast-grep-symbol)
         ("M-g o" . consult-outline)
         ("s-l"   . consult-line)
         ("C-x 4 b" . consult-buffer-other-window))

  :config
  (setq consult-find-args "fd --type f --exclude '.git' --exclude '.svn' --exclude 'node_modules'"
        consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=500 --max-count=50 --path-separator=/ --smart-case --no-heading --with-filename --line-number --threads=0 --max-filesize=1M --glob=!.git/ --glob=!node_modules/ --glob=!target/ --glob=!build/ --glob=!dist/ --glob=!coverage/ --glob=!*.min.js --glob=!*.map --glob=!*.log --glob=!*.png --glob=!*.jpg --glob=!*.gif --glob=!*.svg --glob=!*.ico --glob=!*.woff --glob=!*.woff2 --glob=!*.ttf --glob=!*.eot"
        consult-line-start-from-top nil
        consult-async-refresh-delay 0.3
        consult-async-input-throttle 0.4
        consult-async-input-debounce 0.3
        consult-async-min-input 3)
  (setq consult-buffer-sources
        '(consult-source-hidden-buffer
          consult-source-modified-buffer
          consult-source-buffer)
        consult-project-buffer-sources
        '(consult-source-project-buffer-hidden
          consult-source-project-buffer))

  ;; Process optimization
  (setq read-process-output-max (* 1024 1024)
        process-adaptive-read-buffering nil))

;;;; Savehist
(use-package savehist
  :init
  (savehist-mode))

;;;; Orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;; Corfu (in-buffer completion)
(use-package nerd-icons-corfu
  :after corfu)

(use-package corfu
  :defer 0.1
  :straight (corfu :repo "minad/corfu" :branch "main" :files (:defaults "extensions/*.el"))
  :config
  (defun corfu-complete-and-quit ()
    (interactive)
    (corfu-complete)
    (corfu-quit))
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  :bind  (:map corfu-map
               ("TAB" . corfu-next)
               ([tab] . corfu-next)
               ("RET" . corfu-complete-and-quit)
               ("<return>" . corfu-complete-and-quit)
               ([remap completion-at-point] . corfu-complete))
  :custom-face
  (corfu-default ((t (:family "CommitMono"))))
  (corfu-current ((t (:family "CommitMono"))))
  :custom
  (corfu-auto t)
  (corfu-cycle nil)
  (corfu-count 9)
  (corfu-min-width 20)
  (corfu-on-exact-match 'quit)
  (corfu-preselect-first t)
  (corfu-quit-at-boundary 'separator)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 3)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5))

;;;; Cape (completion-at-point extensions)
(use-package cape
  :defer t
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(provide 'config-completion)
;;; config-completion.el ends here
