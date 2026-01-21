;;; config-search.el --- Search tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Ripgrep-based search, embark actions, and navigation.

;;; Code:

;;;; Ripgrep
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
               vc
             default-directory))
    :confirm prefix
    :flags ("--hidden -g !.git")))

;;;; Deadgrep
(use-package deadgrep)

;;;; Embark
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)))

(use-package embark-consult
  :after embark
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; Avy
(use-package avy
  :config
  (global-set-key (kbd "s-h") 'avy-goto-char-timer))

;;;; Color Moccur
(use-package color-moccur
  :commands (isearch-moccur isearch-all isearch-moccur-all)
  :bind (("M-s O" . moccur)
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all))
  :custom
  (moccur-following-mode-toggle nil))

;;;; Imenu
(use-package imenu
  :ensure nil
  :bind
  ("s-i" . consult-imenu)
  :config
  (setq imenu-max-item-length 'unlimited))

;;;; Custom isearch with region
(defun my/isearch-forward-with-region ()
  "Start isearch-forward using the active region as initial search term."
  (interactive)
  (if (use-region-p)
      (let ((search-term (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))))
        (deactivate-mark)
        (isearch-mode t nil nil nil)
        (isearch-yank-string search-term))
    (isearch-forward)))

(global-set-key (kbd "C-s") #'my/isearch-forward-with-region)

(provide 'config-search)
;;; config-search.el ends here
