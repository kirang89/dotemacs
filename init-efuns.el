(defun kg/split-below-and-move ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun kg/split-right-and-move ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun kg/beginning-of-line-dwim ()
  "Toggle between moving point to the first non-whitespace character, and the start of the line."
  (interactive)
  (let ((start-position (point)))
    ;; Move to the first non-whitespace character.
    (back-to-indentation)

    ;; If we haven't moved position, go to start of the line.
    (when (= (point) start-position)
      (move-beginning-of-line nil))))

(defun kg/duplicate-start-of-line-or-region ()
  "Duplicate start of line or region."
  (interactive)
  (if mark-active
      (kg/duplicate-region)
    (kg/duplicate-start-of-line)))

(defun kg/duplicate-start-of-line ()
  "Duplicate start of line."
  (let ((text (buffer-substring (point)
                                (beginning-of-thing 'line))))
    (forward-line)
    (push-mark)
    (insert text)
    (open-line 1)))

(defun kg/duplicate-region ()
  "Duplicate start of region."
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(defun kg/rename-this-buffer-and-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'"
			                  name
			                  (file-name-nondirectory new-name))))))))

(defun kg/delete-this-buffer-and-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun kg/search-marked-region-if-available (start end)
  "Pre-fill consult-ripgrep with marked region if available."
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
        (consult-ripgrep nil regionp))
    (consult-ripgrep)))

(defun kg/show-user-config ()
  (interactive)
  (find-file "~/.emacs.d/init-user.el"))

(defun kg/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun kg/isearch-query-replace-symbol-at-point ()
  "Run `query-replace-regexp' for the symbol at point."
  (interactive)
  (isearch-forward-symbol-at-point)
  (isearch-query-replace-regexp))

(defun kg/create-tags (dir-name)
  "Create tags file."
  (interactive "Directory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" ctags-path (directory-file-name dir-name))))

;; taken from https://github.com/howardabrams/dot-files/blob/master/emacs.org#unfill-paragraph
(defun kg/unfill-paragraph ()
  "Convert a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun kg/magit-status-fullscreen (prefix)
  (interactive)
  (magit-status)
  (unless prefix
    (delete-other-windows)))

(defun kg/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

   The generic `keyboard-quit' does not do the expected thing when
   the minibuffer is open.  Whereas we want it to close the
   minibuffer, even without explicitly focusing it.
   The DWIM behaviour of this command is as follows:
   - When the region is active, disable it.
   - When a minibuffer is open, but not focused, close the minibuffer.
   - When the Completions buffer is selected, close it.
   - In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(defun kg/rg-find-definitions ()
  "Find definition using ripgrep and display results in minibuffer."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol t))
         (default-directory (or (and (fboundp 'projectile-project-root)
                                     (projectile-project-root))
                                default-directory)))
    (if (fboundp 'consult-ripgrep)
        (consult-ripgrep default-directory (format "\\b%s\\b" symbol))
      (rg-run symbol "everything" default-directory nil nil 'interactive))))

(defun kg/find-definition ()
  "Find definition with precedence: LSP/eglot > ripgrep."
  (interactive)
  (let ((xref-used nil))
    (cond
     ;; Use xref only if eglot/lsp is active
     ((and (boundp 'eglot--managed-mode) eglot--managed-mode)
      (setq xref-used t)
      (call-interactively 'xref-find-definitions))

     ((and (fboundp 'lsp-mode) (bound-and-true-p lsp-mode))
      (setq xref-used t)
      (call-interactively 'xref-find-definitions)))

    ;; Fallback to ripgrep
    (unless xref-used
      (kg/rg-find-definitions))))

(provide 'init-efuns)
