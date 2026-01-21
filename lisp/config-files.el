;;; config-files.el --- File and project management -*- lexical-binding: t; -*-

;;; Commentary:
;; Dired enhancements, projectile, and file finding utilities.

;;; Code:

;;;; Auto-revert
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(setq auto-revert-check-vc-info t)

;;;; Backup Settings
(setq backup-inhibited t
      make-backup-files nil
      auto-save-default nil)

;;;; Dired
(use-package dired
  :straight (:type built-in)
  :custom
  (dired-listing-switches "-lah")
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "xdg-open" "open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open")
     (".*" "open" "xdg-open")))
  (dired-kill-when-opening-new-dired-buffer t))

;; Tree view - expand subdirectories inline
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)))

;; Copy/paste operations across buffers
(use-package dired-ranger
  :straight t
  :after dired
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

;; Collapse empty intermediate directories
(use-package dired-collapse
  :straight t
  :after dired
  :hook (dired-mode . dired-collapse-mode))

;;;; Projectile
(use-package projectile
  :defer t
  :bind (("s-p" . projectile-command-map))
  :hook (prog-mode . projectile-mode)
  :init
  (setq-default projectile-cache-file
                (expand-file-name ".projectile-cache" user-emacs-directory))
  :config
  (setq-default projectile-enable-caching t
                projectile-mode-line-prefix ""
                projectile-sort-order 'recentf
                projectile-mode-line '(:eval (projectile-project-name))))

;;;; Find File in Project
(use-package find-file-in-project
  :config
  (setq ffip-use-rust-fd t)
  (global-set-key (kbd "s-t") 'find-file-in-project))

(provide 'config-files)
;;; config-files.el ends here
