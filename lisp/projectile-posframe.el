;;; projectile-posframe.el --- Projectile file finder in posframe -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (posframe "1.0") (projectile "2.0"))
;; Keywords: convenience, projectile
;; URL: https://github.com/yourusername/projectile-posframe

;;; Commentary:

;; Display projectile file finder in a posframe with incremental filtering.
;;
;; Usage:
;;   (require 'projectile-posframe)
;;   (global-set-key (kbd "C-c p") #'projectile-posframe-find-file)

;;; Code:

(require 'posframe)
(require 'projectile)

(defvar projectile-posframe-buffer " *projectile-posframe*"
  "Buffer name for projectile posframe.")

(defvar projectile-posframe--files nil
  "List of project files.")

(defvar projectile-posframe--filter ""
  "Current filter string.")

(defvar projectile-posframe--root nil
  "Current project root.")

(defgroup projectile-posframe nil
  "Projectile file finder in posframe."
  :group 'projectile
  :prefix "projectile-posframe-")

(defcustom projectile-posframe-width 100
  "Width of the posframe."
  :type 'integer
  :group 'projectile-posframe)

(defcustom projectile-posframe-height 30
  "Height of the posframe."
  :type 'integer
  :group 'projectile-posframe)

(defcustom projectile-posframe-border-width 2
  "Border width of the posframe."
  :type 'integer
  :group 'projectile-posframe)

(defcustom projectile-posframe-max-results 25
  "Maximum number of files to display."
  :type 'integer
  :group 'projectile-posframe)

(defun projectile-posframe--add-char (char)
  "Add CHAR to filter."
  (interactive)
  (setq projectile-posframe--filter
        (concat projectile-posframe--filter (char-to-string char)))
  (projectile-posframe--render)
  (projectile-posframe--focus))

(defun projectile-posframe--delete-char ()
  "Delete last character from filter."
  (interactive)
  (setq projectile-posframe--filter
        (if (> (length projectile-posframe--filter) 0)
            (substring projectile-posframe--filter 0 -1)
          ""))
  (projectile-posframe--render)
  (projectile-posframe--focus))

(defun projectile-posframe--open-file ()
  "Open file at point and close posframe."
  (interactive)
  (when (>= (line-number-at-pos) 4)
    (let* ((line (string-trim
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
           (file (when (not (string-empty-p line))
                   (concat projectile-posframe--root line))))
      (when (and file (file-exists-p file))
        (posframe-hide projectile-posframe-buffer)
        (posframe-delete projectile-posframe-buffer)
        (run-at-time 0.05 nil
                     (lambda ()
                       (find-file file)))))))

(defun projectile-posframe--close ()
  "Close posframe."
  (interactive)
  (posframe-hide projectile-posframe-buffer)
  (posframe-delete projectile-posframe-buffer))

(defun projectile-posframe--focus ()
  "Focus the posframe."
  (when-let ((frame (posframe-frame projectile-posframe-buffer)))
    (select-frame-set-input-focus frame)
    (with-selected-frame frame
      (select-window (frame-first-window frame)))))

;;;###autoload
(defun projectile-posframe-find-file ()
  "Find file in project using posframe with incremental filtering."
  (interactive)
  (if (not (projectile-project-p))
      (message "Not in a project")
    (setq projectile-posframe--files (projectile-project-files (projectile-project-root)))
    (setq projectile-posframe--filter "")
    (setq projectile-posframe--root (projectile-project-root))
    (projectile-posframe--render)
    (projectile-posframe--focus)))

(defun projectile-posframe--render ()
  "Render posframe with filtered files."
  (let* ((buffer (get-buffer-create projectile-posframe-buffer))
         (filtered-files
          (if (string-empty-p projectile-posframe--filter)
              projectile-posframe--files
            (seq-filter
             (lambda (file)
               (string-match-p (regexp-quote projectile-posframe--filter) file))
             projectile-posframe--files))))

    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Filter: %s_\n" projectile-posframe--filter)
                            'face 'bold))
        (insert (propertize (format "(%d files)\n\n" (length filtered-files))
                            'face 'font-lock-comment-face))

        (dolist (file (seq-take filtered-files projectile-posframe-max-results))
          (insert file "\n"))

        (when (> (length filtered-files) projectile-posframe-max-results)
          (insert (propertize "\n...more files\n" 'face 'font-lock-comment-face))))

      (goto-char (point-min))
      (forward-line 3)

      (let ((map (make-sparse-keymap)))
        ;; Navigation
        (define-key map (kbd "C-n") #'next-line)
        (define-key map (kbd "C-p") #'previous-line)
        (define-key map (kbd "<down>") #'next-line)
        (define-key map (kbd "<up>") #'previous-line)

        ;; Actions
        (define-key map (kbd "RET") #'projectile-posframe--open-file)
        (define-key map (kbd "C-g") #'projectile-posframe--close)
        (define-key map (kbd "q") #'projectile-posframe--close)
        (define-key map (kbd "DEL") #'projectile-posframe--delete-char)
        (define-key map (kbd "<backspace>") #'projectile-posframe--delete-char)

        ;; Printable characters
        (dotimes (i 95)
          (let ((char (+ i 32)))
            (define-key map (vector char)
                        (lambda ()
                          (interactive)
                          (setq projectile-posframe--filter
                                (concat projectile-posframe--filter (char-to-string char)))
                          (projectile-posframe--render)
                          (projectile-posframe--focus)))))

        (use-local-map map)))

    (posframe-show buffer
                   :poshandler #'posframe-poshandler-frame-center
                   :width projectile-posframe-width
                   :height projectile-posframe-height
                   :border-width projectile-posframe-border-width
                   :border-color (face-attribute 'shadow :foreground nil t)
                   :background-color (face-background 'default nil t)
                   :internal-border-width 10
                   :accept-focus t
                   :override-parameters '((no-accept-focus . nil)))))

(provide 'projectile-posframe)

;;; projectile-posframe.el ends here
