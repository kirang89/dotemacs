;;; config-vcs.el --- Version control -*- lexical-binding: t; -*-

;;; Commentary:
;; Git integration with Magit and diff indicators.

;;; Code:

;;;; VC Settings
(setq vc-handled-backends '(Git))

;;;; Magit
(use-package magit
  :defer t
  :bind ("C-x g" . 'magit-status)
  :config
  (setq magit-commit-show-diff nil
        magit-save-repository-buffers 'dontask
        magit-format-file-function #'magit-format-file-nerd-icons
        magit-commit-ask-to-stage nil
        ;; Performance settings
        magit-refresh-status-buffer nil
        magit-diff-refine-hunk nil
        magit-diff-highlight-trailing nil
        magit-diff-paint-whitespace nil
        magit-revision-show-gravatars nil
        magit-log-arguments '("-n256" "--graph" "--decorate"))
  ;; Collapse stashes by default
  (setq magit-section-initial-visibility-alist '((stashes . hide)))
  ;; Remove slow remote-checking sections
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
  (defun my/magit-commit-quick ()
    "Commit with message from minibuffer, no editor."
    (interactive)
    (let ((msg (read-string "Commit message: ")))
      (when (not (string-empty-p msg))
        (magit-run-git "commit" "-m" msg))))
  (transient-append-suffix 'magit-commit "c"
    '("q" "Quick commit" my/magit-commit-quick)))

;;;; Diff-hl
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
  ;; Disabled for performance - enable manually with M-x diff-hl-mode
  ;; (add-hook 'prog-mode-hook #'diff-hl-mode)
  )

;;;; Git Timemachine
(use-package git-timemachine)

(provide 'config-vcs)
;;; config-vcs.el ends here
