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

;;;; Find Sibling File (Emacs 29+)
;; Navigate between related files (implementation ↔ test)
(setq find-sibling-rules
      '(;; Python: foo.py <-> test_foo.py, tests/test_foo.py
        ("\\([^/]+\\)\\.py\\'" "\\1_test.py" "test_\\1.py" "tests/test_\\1.py")
        ("test_\\([^/]+\\)\\.py\\'" "\\1.py" "../\\1.py" "src/\\1.py")
        ("\\([^/]+\\)_test\\.py\\'" "\\1.py" "../\\1.py" "src/\\1.py")

        ;; Clojure: foo.clj <-> foo_test.clj, test/foo_test.clj
        ("src/\\(.+\\)\\.clj\\'" "test/\\1_test.clj")
        ("test/\\(.+\\)_test\\.clj\\'" "src/\\1.clj")
        ("\\([^/]+\\)\\.clj\\'" "\\1_test.clj")
        ("\\([^/]+\\)_test\\.clj\\'" "\\1.clj")

        ;; OCaml: foo.ml <-> foo.mli, foo_test.ml
        ("\\([^/]+\\)\\.ml\\'" "\\1.mli" "\\1_test.ml" "test/\\1_test.ml")
        ("\\([^/]+\\)\\.mli\\'" "\\1.ml")
        ("\\([^/]+\\)_test\\.ml\\'" "\\1.ml" "../\\1.ml")))

(global-set-key (kbd "C-c s") 'find-sibling-file)

;;;; Find File in Project
(use-package find-file-in-project
  :config
  (setq ffip-use-rust-fd t)
  (global-set-key (kbd "s-t") 'find-file-in-project))

(provide 'config-files)
;;; config-files.el ends here
