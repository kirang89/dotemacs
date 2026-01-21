(defun kg/split-below-and-move ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun kg/split-right-and-move ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun kg/beginning-of-line-dwim ()
  "Toggle between moving point to the first non-whiteospace character, and the start of the line."
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

(defun kg/consult-line-with-region ()
  "Run consult-line with selected region as initial input."
  (interactive)
  (let ((initial-input (if (use-region-p)
                           (buffer-substring-no-properties
                            (region-beginning) (region-end))
                         nil)))
    (when (use-region-p)
      (deactivate-mark))
    (consult-line initial-input)))

(defun kg/consult-ripgrep-with-region ()
  "Run consult-ripgrep with selected region as initial input."
  (interactive)
  (let ((initial-input (if (use-region-p)
                           (buffer-substring-no-properties
                            (region-beginning) (region-end))
                         nil)))
    (when (use-region-p)
      (deactivate-mark))
    (consult-ripgrep nil initial-input)))

(defun kg/eval-root-expression ()
  "Evaluate the root expression that contains point."
  (interactive)
  (save-excursion
    (let ((current-pos (point)))
      (condition-case nil
          (while t
            (backward-up-list)
            (when (= (point) current-pos)
              (error "No more lists"))
            (setq current-pos (point)))
        (error nil))
      (forward-sexp)
      (eval-last-sexp nil))))

(defun kg/fast-grep (search-term)
  "Ultra-fast ripgrep search with minimal Emacs overhead."
  (interactive "sSearch: ")
  (let* ((project-root (or (projectile-project-root) default-directory))
         (cmd (format "rg --vimgrep --smart-case --hidden --threads=8 %s %s"
                      (shell-quote-argument search-term)
                      (shell-quote-argument project-root)))
         ;; Use compilation-mode for fast results viewing
         (compilation-buffer (compilation-start cmd 'grep-mode)))
    (pop-to-buffer compilation-buffer)))

;; 3. Fastest possible search - current word
(defun kg/fast-grep-symbol ()
  "Instantly grep for symbol at point."
  (interactive)
  (when-let ((symbol (thing-at-point 'symbol t)))
    (kg/fast-grep (format "\\b%s\\b" symbol))))

(defun kg/telescope-grep ()
  "Telescope-like grep with enhanced UX."
  (interactive)
  (let ((consult-ripgrep-args
         "rg --null --line-buffered --color=never --max-columns=1000 --path-separator=/ --smart-case --no-heading --with-filename --line-number --search-zip --hidden --glob=!.git/ --threads=8"))
    (consult-ripgrep)))

;; 9. Live grep (search as you type)
(defun kg/telescope-live-grep ()
  "Live grep that starts searching immediately."
  (interactive)
  (let ((initial-input (if (use-region-p)
                           (buffer-substring-no-properties
                            (region-beginning) (region-end))
                         "")))
    (consult-ripgrep nil initial-input)))

(defun kg/copy-line-if-no-region (orig-fun &rest args)
  "Copy current line if no region is active."
  (if (region-active-p)
      (apply orig-fun args)
    (kill-ring-save (line-beginning-position)
                    (line-end-position))))

(defun kg/swiper-region ()
  "Run `swiper' with the active region as the initial input."
  (interactive)
  (if (use-region-p)
      (let ((init (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (swiper init))
    (swiper)))

(defun kg/setup-imenu-expressions ()
  "Set up imenu expressions for different major modes."
  (pcase major-mode
    ;; Clojure
    ('clojure-mode
     (setq imenu-generic-expression
           '(("Functions" "^\\s-*(defn-?\\s-+\\(.*?\\)\\s-*" 1)
             ("Macros" "^\\s-*(defmacro\\s-+\\(.*?\\)\\s-*" 1)
             ("Variables" "^\\s-*(def\\s-+\\(.*?\\)\\s-*" 1)
             ("Protocols" "^\\s-*(defprotocol\\s-+\\(.*?\\)\\s-*" 1)
             ("Records" "^\\s-*(defrecord\\s-+\\(.*?\\)\\s-*" 1)
             ("Multimethods" "^\\s-*(defmulti\\s-+\\(.*?\\)\\s-*" 1)
             ("Types" "^\\s-*(deftype\\s-+\\(.*?\\)\\s-*" 1))))

    ;; Emacs Lisp
    ('emacs-lisp-mode
     (setq imenu-generic-expression
           '(("Functions" "^\\s-*(defun\\s-+\\(.*?\\)\\s-*(" 1)
             ("Macros" "^\\s-*(defmacro\\s-+\\(.*?\\)\\s-*(" 1)
             ("Variables" "^\\s-*(defvar\\s-+\\(.*?\\)\\s-*" 1)
             ("Custom Variables" "^\\s-*(defcustom\\s-+\\(.*?\\)\\s-*" 1)
             ("Groups" "^\\s-*(defgroup\\s-+\\(.*?\\)\\s-*" 1)
             ("Faces" "^\\s-*(defface\\s-+\\(.*?\\)\\s-*" 1)
             ("Advice" "^\\s-*(defadvice\\s-+\\(.*?\\)\\s-*" 1))))

    ;; Python
    ('python-mode
     (setq imenu-generic-expression
           '(("Classes" "^class\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
             ("Functions" "^def\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
             ("Async Functions" "^async\\s-+def\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
             ("Methods" "^\\s-+def\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
             ("Variables" "^\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*=" 1))))

    ;; Ruby
    ('ruby-mode
     (setq imenu-generic-expression
           '(("Classes" "^\\s-*class\\s-+\\([a-zA-Z_][a-zA-Z0-9_:]*\\)" 1)
             ("Modules" "^\\s-*module\\s-+\\([a-zA-Z_][a-zA-Z0-9_:]*\\)" 1)
             ("Methods" "^\\s-*def\\s-+\\([a-zA-Z_][a-zA-Z0-9_?!]*\\)" 1)
             ("Constants" "^\\s-*\\([A-Z][A-Z0-9_]*\\)\\s-*=" 1)
             ("Instance Variables" "@\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
             ("Class Variables" "@@\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1))))

    ;; TypeScript
    ('typescript-mode
     (setq imenu-generic-expression
           '(("Interfaces" "^\\s-*interface\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
             ("Classes" "^\\s-*class\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
             ("Types" "^\\s-*type\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
             ("Enums" "^\\s-*enum\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
             ("Functions" "^\\s-*function\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
             ("Arrow Functions" "^\\s-*const\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*=\\s-*(" 1)
             ("Variables" "^\\s-*(?:const|let|var)\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1))))))

(provide 'init-efuns)
