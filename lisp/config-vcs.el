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
        magit-revert-buffers 'silent
        magit-format-file-function #'magit-format-file-nerd-icons)
  (setq magit-commit-ask-to-stage nil)
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
  (add-hook 'prog-mode-hook #'diff-hl-mode))

;;;; Git Timemachine
(use-package git-timemachine)

(provide 'config-vcs)
;;; config-vcs.el ends here
