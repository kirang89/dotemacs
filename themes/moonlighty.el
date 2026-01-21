;;; moonlighty-theme.el --- Custom theme based on user screenshot

;; Author: Gemini
;; Version: 1.0
;; Keywords: faces, theme, dark

;; Optimized for Emacs 30.2+

;;; Code:

(deftheme moonlighty
  "A custom theme based on a specific dark blue/slate tone.")

(let ((class '((class color) (min-colors 89)))
      ;; Color Palette Extraction
      (bg-main     "#181923")  ;; Deep dark blue/gray background
      (bg-hl       "#252735")  ;; Slight highlight for current line
      (bg-region   "#343a53")  ;; Selection highlight (blue-ish)
      (fg-main     "#cdd6f4")  ;; Off-white/cool gray foreground
      (fg-dim      "#aeb2c4")  ;; Dimmed text
      (comment     "#5c6370")  ;; Grey comments
      (cyan        "#89ddff")  ;; Operators / Symbols
      (blue-slate  "#89a0ce")  ;; Keywords (local, function, if)
      (pink        "#f28795")  ;; Builtins/Methods (self, table, insert)
      (beige       "#e8dca8")  ;; Strings
      (orange      "#fab387")  ;; Numbers
      (line-num    "#454959")  ;; Line numbers
      (cursor      "#cdd6f4")) ;; Cursor color

  (custom-theme-set-faces
   'moonlighty

   ;; Base UI
   `(default ((,class (:foreground ,fg-main :background ,bg-main))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,bg-region :extend t))))
   `(fringe ((,class (:background ,bg-main :foreground ,line-num))))
   `(minibuffer-prompt ((,class (:foreground ,blue-slate :bold t))))
   `(vertical-border ((,class (:foreground ,bg-hl))))

   ;; Line Numbers
   `(line-number ((,class (:foreground ,line-num :background ,bg-main))))
   `(line-number-current-line ((,class (:foreground ,fg-main :background ,bg-hl :weight bold))))

   ;; Syntax Highlighting (Font Lock)
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-doc-face ((,class (:foreground ,comment :slant italic))))

   ;; Keywords (local, function, end, return) -> Slate Blue
   `(font-lock-keyword-face ((,class (:foreground ,blue-slate))))

   ;; Builtins & Constants (self, table, insert) -> Pink
   `(font-lock-builtin-face ((,class (:foreground ,pink))))
   `(font-lock-constant-face ((,class (:foreground ,pink))))
   `(font-lock-type-face ((,class (:foreground ,pink))))

   ;; Strings -> Beige
   `(font-lock-string-face ((,class (:foreground ,beige))))

   ;; Numbers -> Orange
   `(font-lock-number-face ((,class (:foreground ,orange))))

   ;; Function definitions -> Foreground/White (as seen in `RequestStatus:start`)
   `(font-lock-function-name-face ((,class (:foreground ,fg-main :weight bold))))

   ;; Variables -> Foreground/White (as seen in `braille_chars`, `index`)
   `(font-lock-variable-name-face ((,class (:foreground ,fg-main))))

   ;; Warning/Error
   `(font-lock-warning-face ((,class (:foreground ,orange :weight bold))))
   `(error ((,class (:foreground "#ff5555" :weight bold))))

   ;; Mode Line
   `(mode-line ((,class (:background ,bg-hl :foreground ,fg-main :box (:line-width -1 :style released-button)))))
   `(mode-line-inactive ((,class (:background ,bg-main :foreground ,comment :box (:line-width -1 :style released-button)))))

   ;; Search
   `(isearch ((,class (:background ,orange :foreground ,bg-main))))
   `(lazy-highlight ((,class (:background ,bg-hl :foreground ,orange))))

   ;; Paren Match
   `(show-paren-match ((,class (:background ,bg-region :foreground ,pink :weight bold))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-directory load-file-name)))

(provide-theme 'moonlighty)

;;; moonlighty-theme.el ends here
