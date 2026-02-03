;;; config-search.el --- Search tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Ripgrep-based search, embark actions, and navigation.

;;; Code:

;;;; Ripgrep
(use-package rg
  :commands (rg rg-project rg-dwim rg-literal)
  :config
  (setq rg-command-line-flags
        '("-w"
          "-M" "500"              ; Skip lines >500 chars
          "-m" "100"              ; Max 100 matches per file
          "--max-filesize" "1M"
          "-j" "0"                ; Use ALL cores
          "--mmap"                ; Memory-map files (faster for large files)
          "--engine" "auto"))     ; Let rg pick fastest regex engine
  (setq rg-ignore-case 'smart
        rg-group-result t         ; Group by file (less rendering)
        rg-show-header nil)       ; Skip header rendering

  (setq rg-custom-type-aliases
        '(("all" . "*.{el,py,rb,js,ts,go,rs,c,h,cpp,java,clj}")))

  (rg-define-search kg/grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc vc default-directory))
    :confirm prefix
    :flags ("--hidden" "-g" "!.git" "-g" "!node_modules" "-g" "!target" "-g" "!build")))

;;;; Deadgrep
(use-package deadgrep)

;;;; Embark
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Embark Actions\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.4))))

(use-package embark-consult
  :after embark
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; Avy
(use-package avy
  :bind (("s-h" . avy-goto-char-timer)))

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
  :bind (("s-i" . consult-imenu))
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
