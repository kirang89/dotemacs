;;; moonlight-theme.el --- A dark theme inspired by VS Code's Moonlight -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Brettm12345 <https://github.com/Brettm12345>
;; Source: https://github.com/atomiks/moonlight-vscode-theme
;; Standalone conversion by Claude
;;
;;; Commentary:
;;
;; A standalone port of the doom-moonlight theme that doesn't require
;; doom-themes as a dependency.
;;
;;; Code:

(require 'cl-lib)

(deftheme moonlight
  "A dark theme inspired by VS Code's Moonlight.")

;;; Helper functions

(defun moonlight--name-to-rgb (color)
  "Convert named COLOR to RGB values."
  (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
           for x in (tty-color-standard-values (downcase color))
           collect (/ x div)))

(defun moonlight--blend (color1 color2 alpha)
  "Blend COLOR1 and COLOR2 by ALPHA (float 0-1)."
  (when (and color1 color2)
    (cond ((and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
           (apply (lambda (r g b)
                    (format "#%02x%02x%02x"
                            (* r 255) (* g 255) (* b 255)))
                  (cl-loop for it    in (moonlight--name-to-rgb color1)
                           for other in (moonlight--name-to-rgb color2)
                           collect (+ (* alpha it) (* other (- 1 alpha))))))
          (t color1))))

(defun moonlight--darken (color alpha)
  "Darken COLOR by ALPHA (float 0-1)."
  (moonlight--blend color "#000000" (- 1 alpha)))

(defun moonlight--lighten (color alpha)
  "Lighten COLOR by ALPHA (float 0-1)."
  (moonlight--blend color "#FFFFFF" (- 1 alpha)))

;;; Color definitions

(let* (;; Base colors
       (bg         "#212337")
       (bg-alt     "#191a2a")
       (base0      "#161a2a")
       (base1      "#191a2a")
       (base2      "#1e2030")
       (base3      "#222436")
       (base4      "#2f334d")
       (base5      "#444a73")
       (base6      "#828bb8")
       (base7      "#a9b8e8")
       (base8      "#b4c2f0")
       (indigo     "#7a88cf")
       (region     "#383e5c")
       (fg         "#c8d3f5")
       (fg-alt     "#b4c2f0")

       (grey       base5)

       (dark-red      "#ff5370")
       (red           "#ff757f")
       (light-red     "#ff98a4")
       (orange        "#ff995e")
       (green         "#c3e88d")
       (dark-teal     "#4fd6be")
       (teal          "#77e0c6")
       (light-teal    "#7af8ca")
       (yellow        "#ffc777")
       (blue          "#82aaff")
       (dark-blue     "#4976eb")
       (light-blue    "#50c4fa")
       (light-magenta "#baacff")
       (magenta       "#c099ff")
       (violet        "#f989d3")
       (light-pink    "#fca7ea")
       (pink          "#f3c1ff")
       (cyan          "#b4f9f8")
       (dark-cyan     "#86e1fc")

       ;; Semantic colors
       (highlight      blue)
       (vertical-bar   base0)
       (line-highlight base4)
       (selection      region)
       (builtin        magenta)
       (comments       indigo)
       (doc-comments   (moonlight--lighten comments 0.25))
       (constants      orange)
       (functions      blue)
       (keywords       magenta)
       (methods        red)
       (operators      dark-cyan)
       (type           yellow)
       (strings        green)
       (variables      light-red)
       (numbers        orange)
       (error          red)
       (warning        yellow)
       (success        green)
       (vc-modified    blue)
       (vc-added       teal)
       (vc-deleted     red)

       ;; Modeline
       (modeline-bg     (moonlight--darken base2 0.1))
       (modeline-bg-alt (moonlight--darken bg 0.1))
       (modeline-fg     base8)
       (modeline-fg-alt comments))

  (custom-theme-set-faces
   'moonlight

   ;; Basic faces
   `(default ((t (:background ,bg :foreground ,fg))))
   `(bold ((t (:weight bold :foreground ,base8))))
   `(italic ((t (:slant italic))))
   `(bold-italic ((t (:inherit (bold italic)))))
   `(fringe ((t (:inherit default :foreground ,base4))))
   `(region ((t (:background ,region :extend t))))
   `(highlight ((t (:background ,highlight :foreground ,base0 :distant-foreground ,base8))))
   `(cursor ((t (:background ,highlight))))
   `(shadow ((t (:foreground ,base5))))
   `(minibuffer-prompt ((t (:foreground ,highlight))))
   `(tooltip ((t (:background ,base0 :foreground ,fg))))
   `(secondary-selection ((t (:background ,grey :extend t))))
   `(lazy-highlight ((t (:background ,base4 :foreground ,fg))))
   `(match ((t (:foreground ,green :background ,base0 :weight bold))))
   `(trailing-whitespace ((t (:background ,red))))
   `(nobreak-space ((t (:inherit escape-glyph :underline t))))
   `(vertical-border ((t (:background ,vertical-bar :foreground ,vertical-bar))))
   `(link ((t (:foreground ,highlight :underline t :weight bold))))
   `(error ((t (:foreground ,error))))
   `(warning ((t (:foreground ,warning))))
   `(success ((t (:foreground ,success))))
   `(escape-glyph ((t (:foreground ,cyan))))
   `(hl-line ((t (:background ,line-highlight))))

   ;; Font-lock faces
   `(font-lock-builtin-face ((t (:foreground ,builtin))))
   `(font-lock-comment-face ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((t (:foreground ,constants))))
   `(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground ,doc-comments))))
   `(font-lock-function-name-face ((t (:foreground ,functions))))
   `(font-lock-function-call-face ((t (:inherit font-lock-function-name-face :slant italic :foreground ,(moonlight--blend functions fg 0.7)))))
   `(font-lock-keyword-face ((t (:foreground ,keywords))))
   `(font-lock-negation-char-face ((t (:inherit bold :foreground ,operators))))
   `(font-lock-number-face ((t (:foreground ,numbers))))
   `(font-lock-operator-face ((t (:foreground ,operators))))
   `(font-lock-preprocessor-face ((t (:inherit bold :foreground ,operators))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit bold :foreground ,operators))))
   `(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground ,operators))))
   `(font-lock-string-face ((t (:foreground ,strings))))
   `(font-lock-type-face ((t (:foreground ,type))))
   `(font-lock-variable-name-face ((t (:foreground ,variables))))
   `(font-lock-variable-use-face ((t (:inherit font-lock-variable-name-face :foreground ,(moonlight--blend variables fg 0.5)))))
   `(font-lock-warning-face ((t (:inherit warning))))
   `(font-lock-bracket-face ((t (:inherit font-lock-punctuation-face))))
   `(font-lock-delimiter-face ((t (:inherit font-lock-punctuation-face))))
   `(font-lock-escape-face ((t (:inherit font-lock-regexp-grouping-backslash))))
   `(font-lock-misc-punctuation-face ((t (:inherit font-lock-punctuation-face))))
   `(font-lock-punctuation-face ((t (:foreground ,(moonlight--blend operators fg 0.25)))))
   `(font-lock-property-name-face ((t (:foreground ,(moonlight--blend keywords fg 0.6) :weight bold))))
   `(font-lock-property-use-face ((t (:inherit font-lock-property-name-face :weight normal))))

   ;; Mode line
   `(mode-line ((t (:background ,modeline-bg :foreground ,modeline-fg))))
   `(mode-line-active ((t (:inherit mode-line))))
   `(mode-line-inactive ((t (:background ,modeline-bg-alt :foreground ,modeline-fg-alt))))
   `(mode-line-emphasis ((t (:foreground ,highlight))))
   `(mode-line-highlight ((t (:inherit highlight))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(header-line ((t (:inherit mode-line))))
   `(header-line-highlight ((t (:inherit mode-line-highlight))))

   ;; Line numbers
   `(line-number ((t (:inherit default :foreground ,base5 :background ,(moonlight--darken bg 0.06) :weight normal :slant normal))))
   `(line-number-current-line ((t (:inherit (hl-line default) :foreground ,fg :background ,line-highlight :weight normal :slant normal))))

   ;; Tab bar/line
   `(tab-line ((t (:background ,bg-alt :foreground ,bg-alt))))
   `(tab-line-tab ((t (:background ,bg :foreground ,fg))))
   `(tab-line-tab-inactive ((t (:background ,bg-alt :foreground ,fg-alt))))
   `(tab-line-tab-current ((t (:background ,bg :foreground ,fg))))
   `(tab-line-highlight ((t (:inherit tab-line-tab))))
   `(tab-line-close-highlight ((t (:foreground ,highlight))))
   `(tab-bar ((t (:inherit tab-line))))
   `(tab-bar-tab ((t (:inherit tab-line-tab))))
   `(tab-bar-tab-inactive ((t (:inherit tab-line-tab-inactive))))

   ;; Isearch
   `(isearch ((t (:inherit highlight :weight bold))))
   `(isearch-fail ((t (:background ,red :foreground ,base0 :weight bold))))

   ;; Show paren
   `(show-paren-match ((t (:foreground ,red :background ,base0 :weight ultra-bold))))
   `(show-paren-mismatch ((t (:foreground ,base0 :background ,red :weight ultra-bold))))

   ;; All-the-icons
   `(all-the-icons-cyan ((t (:foreground ,dark-cyan))))
   `(all-the-icons-cyan-alt ((t (:foreground ,dark-cyan))))
   `(all-the-icons-dblue ((t (:foreground ,(moonlight--darken blue 0.1)))))
   `(all-the-icons-dgreen ((t (:foreground ,dark-teal))))
   `(all-the-icons-dmaroon ((t (:foreground ,magenta))))
   `(all-the-icons-dorange ((t (:foreground ,orange))))
   `(all-the-icons-dpink ((t (:foreground ,pink))))
   `(all-the-icons-dpurple ((t (:foreground ,magenta))))
   `(all-the-icons-dred ((t (:foreground ,dark-red))))
   `(all-the-icons-dsilver ((t (:foreground ,grey))))
   `(all-the-icons-dyellow ((t (:foreground ,orange))))
   `(all-the-icons-green ((t (:foreground ,teal))))
   `(all-the-icons-lcyan ((t (:foreground ,(moonlight--lighten dark-cyan 0.3)))))
   `(all-the-icons-lgreen ((t (:foreground ,green))))
   `(all-the-icons-lmaroon ((t (:foreground ,light-magenta))))
   `(all-the-icons-lorange ((t (:foreground ,orange))))
   `(all-the-icons-lpink ((t (:foreground ,light-pink))))
   `(all-the-icons-lpurple ((t (:foreground ,light-magenta))))
   `(all-the-icons-lred ((t (:foreground ,light-red))))
   `(all-the-icons-lsilver ((t (:foreground ,(moonlight--lighten grey 0.4)))))
   `(all-the-icons-lyellow ((t (:foreground ,(moonlight--lighten yellow 0.3)))))
   `(all-the-icons-orange ((t (:foreground ,orange))))
   `(all-the-icons-pink ((t (:foreground ,pink))))
   `(all-the-icons-purple ((t (:foreground ,magenta))))
   `(all-the-icons-purple-alt ((t (:foreground ,magenta))))
   `(all-the-icons-red-alt ((t (:foreground ,red))))
   `(all-the-icons-silver ((t (:foreground ,(moonlight--lighten grey 0.2)))))
   `(all-the-icons-blue ((t (:foreground ,blue))))
   `(all-the-icons-maroon ((t (:foreground ,magenta))))
   `(all-the-icons-red ((t (:foreground ,red))))
   `(all-the-icons-yellow ((t (:foreground ,yellow))))

   ;; All-the-icons-dired
   `(all-the-icons-dired-dir-face ((t (:foreground ,indigo))))

   ;; Nerd-icons
   `(nerd-icons-blue ((t (:foreground ,blue))))
   `(nerd-icons-cyan ((t (:foreground ,dark-cyan))))
   `(nerd-icons-dblue ((t (:foreground ,(moonlight--darken blue 0.1)))))
   `(nerd-icons-dgreen ((t (:foreground ,dark-teal))))
   `(nerd-icons-dmaroon ((t (:foreground ,magenta))))
   `(nerd-icons-dorange ((t (:foreground ,orange))))
   `(nerd-icons-dpink ((t (:foreground ,pink))))
   `(nerd-icons-dpurple ((t (:foreground ,magenta))))
   `(nerd-icons-dred ((t (:foreground ,dark-red))))
   `(nerd-icons-dsilver ((t (:foreground ,grey))))
   `(nerd-icons-dyellow ((t (:foreground ,orange))))
   `(nerd-icons-green ((t (:foreground ,teal))))
   `(nerd-icons-lblue ((t (:foreground ,(moonlight--lighten blue 0.3)))))
   `(nerd-icons-lcyan ((t (:foreground ,(moonlight--lighten dark-cyan 0.3)))))
   `(nerd-icons-lgreen ((t (:foreground ,green))))
   `(nerd-icons-lmaroon ((t (:foreground ,light-magenta))))
   `(nerd-icons-lorange ((t (:foreground ,orange))))
   `(nerd-icons-lpink ((t (:foreground ,light-pink))))
   `(nerd-icons-lpurple ((t (:foreground ,light-magenta))))
   `(nerd-icons-lred ((t (:foreground ,light-red))))
   `(nerd-icons-lsilver ((t (:foreground ,(moonlight--lighten grey 0.4)))))
   `(nerd-icons-lyellow ((t (:foreground ,(moonlight--lighten yellow 0.3)))))
   `(nerd-icons-maroon ((t (:foreground ,magenta))))
   `(nerd-icons-orange ((t (:foreground ,orange))))
   `(nerd-icons-pink ((t (:foreground ,pink))))
   `(nerd-icons-purple ((t (:foreground ,magenta))))
   `(nerd-icons-red ((t (:foreground ,red))))
   `(nerd-icons-silver ((t (:foreground ,(moonlight--lighten grey 0.2)))))
   `(nerd-icons-yellow ((t (:foreground ,yellow))))

   ;; Company
   `(company-tooltip ((t (:inherit tooltip))))
   `(company-tooltip-common ((t (:foreground ,highlight))))
   `(company-tooltip-selection ((t (:background ,selection :weight bold))))
   `(company-tooltip-search ((t (:background ,highlight :foreground ,bg :weight bold))))
   `(company-tooltip-annotation ((t (:foreground ,violet))))
   `(company-scrollbar-bg ((t (:inherit tooltip))))
   `(company-scrollbar-fg ((t (:background ,highlight))))
   `(company-preview ((t (:foreground ,comments))))
   `(company-preview-common ((t (:background ,base3 :foreground ,highlight))))
   `(company-box-annotation ((t (:foreground ,base7))))

   ;; Corfu
   `(corfu-default ((t (:inherit tooltip))))
   `(corfu-current ((t (:background ,selection :foreground ,fg))))
   `(corfu-bar ((t (:background ,highlight))))
   `(corfu-border ((t (:background ,base4))))
   `(corfu-annotations ((t (:foreground ,violet))))

   ;; CSS
   `(css-proprietary-property ((t (:foreground ,orange))))
   `(css-property ((t (:foreground ,green))))
   `(css-selector ((t (:foreground ,blue))))

   ;; Dired
   `(dired-directory ((t (:foreground ,blue))))
   `(dired-ignored ((t (:foreground ,comments))))
   `(dired-flagged ((t (:foreground ,red))))
   `(dired-header ((t (:foreground ,magenta :weight bold))))
   `(dired-mark ((t (:foreground ,orange :weight bold))))
   `(dired-marked ((t (:foreground ,magenta :weight bold))))
   `(dired-perm-write ((t (:foreground ,fg :underline t))))
   `(dired-symlink ((t (:foreground ,cyan))))
   `(dired-warning ((t (:foreground ,warning))))

   ;; Diredfl
   `(diredfl-date-time ((t (:foreground ,blue))))
   `(diredfl-file-name ((t (:foreground ,base7))))
   `(diredfl-file-suffix ((t (:foreground ,base6))))
   `(diredfl-symlink ((t (:foreground ,dark-cyan))))
   `(diredfl-dir-heading ((t (:foreground ,blue :weight bold))))
   `(diredfl-dir-name ((t (:foreground ,blue))))
   `(diredfl-dir-priv ((t (:foreground ,blue))))
   `(diredfl-exec-priv ((t (:foreground ,green))))
   `(diredfl-no-priv ((t (:foreground ,fg))))
   `(diredfl-number ((t (:foreground ,orange))))
   `(diredfl-read-priv ((t (:foreground ,yellow))))
   `(diredfl-write-priv ((t (:foreground ,red))))

   ;; Doom-modeline
   `(doom-modeline-buffer-file ((t (:foreground ,base7))))
   `(doom-modeline-icon-inactive ((t (:foreground ,indigo))))
   `(doom-modeline-evil-normal-state ((t (:foreground ,dark-cyan))))
   `(doom-modeline-evil-insert-state ((t (:foreground ,blue))))
   `(doom-modeline-project-dir ((t (:foreground ,light-teal))))
   `(doom-modeline-buffer-path ((t (:foreground ,blue))))
   `(doom-modeline-buffer-modified ((t (:inherit bold :foreground ,yellow))))
   `(doom-modeline-buffer-major-mode ((t (:inherit doom-modeline-buffer-path))))

   ;; Flycheck
   `(flycheck-error ((t (:underline (:style wave :color ,red)))))
   `(flycheck-warning ((t (:underline (:style wave :color ,yellow)))))
   `(flycheck-info ((t (:underline (:style wave :color ,green)))))

   ;; Flymake
   `(flymake-error ((t (:underline (:style wave :color ,red)))))
   `(flymake-warning ((t (:underline (:style wave :color ,yellow)))))
   `(flymake-note ((t (:underline (:style wave :color ,green)))))

   ;; Git-gutter / Diff-hl
   `(git-gutter:added ((t (:foreground ,vc-added))))
   `(git-gutter:deleted ((t (:foreground ,vc-deleted))))
   `(git-gutter:modified ((t (:foreground ,vc-modified))))
   `(diff-hl-insert ((t (:foreground ,vc-added :background ,(moonlight--blend vc-added bg 0.2)))))
   `(diff-hl-delete ((t (:foreground ,vc-deleted :background ,(moonlight--blend vc-deleted bg 0.2)))))
   `(diff-hl-change ((t (:foreground ,vc-modified :background ,(moonlight--blend vc-modified bg 0.2)))))

   ;; Ivy
   `(ivy-current-match ((t (:background ,selection :extend t))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,blue :weight bold))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,magenta :weight bold))))
   `(ivy-minibuffer-match-highlight ((t (:foreground ,blue))))
   `(ivy-posframe ((t (:background ,base0))))
   `(ivy-posframe-border ((t (:background ,base0))))

   ;; JS2-mode
   `(js2-jsdoc-tag ((t (:foreground ,magenta))))
   `(js2-object-property ((t (:foreground ,dark-teal))))
   `(js2-object-property-access ((t (:foreground ,fg-alt))))
   `(js2-function-param ((t (:foreground ,pink))))
   `(js2-jsdoc-type ((t (:foreground ,base8))))
   `(js2-jsdoc-value ((t (:foreground ,cyan))))

   ;; LSP / Eglot
   `(lsp-face-highlight-read ((t (:background ,region))))
   `(lsp-face-highlight-textual ((t (:background ,region))))
   `(lsp-face-highlight-write ((t (:background ,region))))
   `(eglot-highlight-symbol-face ((t (:background ,region))))

   ;; Magit
   `(magit-filename ((t (:foreground ,teal))))
   `(magit-section-heading ((t (:foreground ,blue :weight bold))))
   `(magit-section-highlight ((t (:background ,base3 :extend t))))
   `(magit-diff-context-highlight ((t (:background ,base3 :foreground ,fg))))
   `(magit-diff-removed ((t (:foreground ,red :background ,(moonlight--blend red bg 0.1)))))
   `(magit-diff-removed-highlight ((t (:foreground ,red :background ,(moonlight--blend red bg 0.2) :weight bold))))
   `(magit-diff-added ((t (:foreground ,green :background ,(moonlight--blend green bg 0.1)))))
   `(magit-diff-added-highlight ((t (:foreground ,green :background ,(moonlight--blend green bg 0.2) :weight bold))))
   `(magit-diff-hunk-heading ((t (:background ,base3))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,base4))))
   `(magit-branch-current ((t (:foreground ,blue))))
   `(magit-branch-local ((t (:foreground ,cyan))))
   `(magit-branch-remote ((t (:foreground ,green))))
   `(magit-hash ((t (:foreground ,comments))))
   `(magit-log-author ((t (:foreground ,orange))))
   `(magit-log-date ((t (:foreground ,blue))))

   ;; Man
   `(Man-overstrike ((t (:inherit bold :foreground ,magenta))))
   `(Man-underline ((t (:inherit underline :foreground ,blue))))

   ;; Markdown
   `(markdown-header-face ((t (:inherit bold :foreground ,yellow))))
   `(markdown-header-delimiter-face ((t (:inherit markdown-header-face))))
   `(markdown-metadata-key-face ((t (:foreground ,magenta :slant italic))))
   `(markdown-list-face ((t (:foreground ,red))))
   `(markdown-url-face ((t (:inherit underline :foreground ,orange))))
   `(markdown-gfm-checkbox-face ((t (:foreground ,blue))))
   `(markdown-blockquote-face ((t (:slant italic :foreground ,fg))))
   `(markdown-code-face ((t (:background ,base1))))
   `(markdown-inline-code-face ((t (:background ,base1 :foreground ,green))))
   `(markdown-pre-face ((t (:background ,base1 :foreground ,green))))

   ;; Message
   `(message-header-name ((t (:foreground ,green))))
   `(message-header-subject ((t (:foreground ,highlight :weight bold))))
   `(message-header-to ((t (:foreground ,highlight :weight bold))))
   `(message-header-cc ((t (:inherit message-header-to :foreground ,(moonlight--darken highlight 0.15)))))
   `(message-header-other ((t (:foreground ,violet))))
   `(message-header-newsgroups ((t (:foreground ,yellow))))
   `(message-header-xheader ((t (:foreground ,doc-comments))))
   `(message-separator ((t (:foreground ,comments))))
   `(message-mml ((t (:foreground ,comments :slant italic))))
   `(message-cited-text ((t (:foreground ,magenta))))

   ;; Nix
   `(nix-attribute-face ((t (:foreground ,blue))))
   `(nix-builtin-face ((t (:foreground ,dark-teal))))

   ;; Orderless
   `(orderless-match-face-0 ((t (:foreground ,blue :weight bold))))
   `(orderless-match-face-1 ((t (:foreground ,magenta :weight bold))))
   `(orderless-match-face-2 ((t (:foreground ,green :weight bold))))
   `(orderless-match-face-3 ((t (:foreground ,yellow :weight bold))))

   ;; Org
   `(org-level-1 ((t (:foreground ,light-blue :weight bold :height 1.2))))
   `(org-level-2 ((t (:foreground ,dark-cyan :weight bold :height 1.1))))
   `(org-level-3 ((t (:foreground ,light-red :weight bold))))
   `(org-level-4 ((t (:foreground ,blue))))
   `(org-level-5 ((t (:foreground ,magenta))))
   `(org-level-6 ((t (:foreground ,red))))
   `(org-level-7 ((t (:foreground ,violet))))
   `(org-level-8 ((t (:foreground ,orange))))
   `(org-block ((t (:background ,base2 :extend t))))
   `(org-block-begin-line ((t (:background ,base2 :foreground ,comments :extend t))))
   `(org-block-end-line ((t (:inherit org-block-begin-line))))
   `(org-code ((t (:foreground ,orange))))
   `(org-date ((t (:foreground ,yellow))))
   `(org-document-info ((t (:foreground ,blue))))
   `(org-document-title ((t (:foreground ,blue :weight bold))))
   `(org-done ((t (:foreground ,green))))
   `(org-headline-done ((t (:foreground ,base5))))
   `(org-link ((t (:foreground ,blue :underline t))))
   `(org-meta-line ((t (:foreground ,comments))))
   `(org-priority ((t (:foreground ,red))))
   `(org-scheduled ((t (:foreground ,green))))
   `(org-scheduled-previously ((t (:foreground ,yellow))))
   `(org-scheduled-today ((t (:foreground ,orange))))
   `(org-special-keyword ((t (:foreground ,comments))))
   `(org-table ((t (:foreground ,violet))))
   `(org-tag ((t (:foreground ,comments :weight normal))))
   `(org-todo ((t (:foreground ,yellow :weight bold))))
   `(org-upcoming-deadline ((t (:foreground ,red))))
   `(org-verbatim ((t (:foreground ,green))))
   `(org-warning ((t (:foreground ,warning))))

   ;; Outline
   `(outline-1 ((t (:foreground ,light-blue :weight bold))))
   `(outline-2 ((t (:foreground ,dark-cyan :weight bold))))
   `(outline-3 ((t (:foreground ,light-red :weight bold))))
   `(outline-4 ((t (:foreground ,blue))))
   `(outline-5 ((t (:foreground ,magenta))))
   `(outline-6 ((t (:foreground ,red))))
   `(outline-7 ((t (:foreground ,violet))))
   `(outline-8 ((t (:foreground ,orange))))

   ;; Rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,magenta))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,light-red))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,violet))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,teal))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,dark-cyan))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,red :weight bold))))

   ;; RJSX
   `(rjsx-tag ((t (:foreground ,violet))))
   `(rjsx-attr ((t (:foreground ,yellow :slant italic :weight medium))))

   ;; Selectrum
   `(selectrum-current-candidate ((t (:background ,selection :extend t))))
   `(selectrum-primary-highlight ((t (:foreground ,blue :weight bold))))
   `(selectrum-secondary-highlight ((t (:foreground ,magenta :weight bold))))

   ;; Swiper
   `(swiper-line-face ((t (:background ,selection :extend t))))
   `(swiper-match-face-1 ((t (:inherit unspecified :background ,base0 :foreground ,fg))))
   `(swiper-match-face-2 ((t (:inherit unspecified :background ,orange :foreground ,base0 :weight bold))))
   `(swiper-match-face-3 ((t (:inherit unspecified :background ,magenta :foreground ,base0 :weight bold))))
   `(swiper-match-face-4 ((t (:inherit unspecified :background ,green :foreground ,base0 :weight bold))))

   ;; Treemacs
   `(treemacs-directory-face ((t (:foreground ,highlight))))
   `(treemacs-git-modified-face ((t (:foreground ,highlight))))
   `(treemacs-file-face ((t (:foreground ,fg))))
   `(treemacs-root-face ((t (:foreground ,blue :weight bold))))

   ;; Tree-sitter
   `(tree-sitter-hl-face:attribute ((t (:foreground ,yellow))))
   `(tree-sitter-hl-face:comment ((t (:foreground ,comments :slant italic))))
   `(tree-sitter-hl-face:constant ((t (:foreground ,constants))))
   `(tree-sitter-hl-face:constant.builtin ((t (:foreground ,orange))))
   `(tree-sitter-hl-face:constructor ((t (:foreground ,blue))))
   `(tree-sitter-hl-face:doc ((t (:foreground ,doc-comments))))
   `(tree-sitter-hl-face:embedded ((t (:foreground ,fg))))
   `(tree-sitter-hl-face:escape ((t (:foreground ,operators))))
   `(tree-sitter-hl-face:function ((t (:foreground ,functions))))
   `(tree-sitter-hl-face:function.builtin ((t (:foreground ,blue))))
   `(tree-sitter-hl-face:function.call ((t (:foreground ,functions))))
   `(tree-sitter-hl-face:function.macro ((t (:foreground ,magenta))))
   `(tree-sitter-hl-face:function.special ((t (:foreground ,magenta))))
   `(tree-sitter-hl-face:keyword ((t (:foreground ,keywords))))
   `(tree-sitter-hl-face:label ((t (:foreground ,blue))))
   `(tree-sitter-hl-face:method ((t (:foreground ,methods))))
   `(tree-sitter-hl-face:method.call ((t (:foreground ,methods))))
   `(tree-sitter-hl-face:number ((t (:foreground ,numbers))))
   `(tree-sitter-hl-face:operator ((t (:foreground ,operators))))
   `(tree-sitter-hl-face:property ((t (:foreground ,dark-teal))))
   `(tree-sitter-hl-face:property.definition ((t (:foreground ,dark-teal))))
   `(tree-sitter-hl-face:punctuation ((t (:foreground ,fg))))
   `(tree-sitter-hl-face:punctuation.bracket ((t (:foreground ,fg))))
   `(tree-sitter-hl-face:punctuation.delimiter ((t (:foreground ,fg))))
   `(tree-sitter-hl-face:punctuation.special ((t (:foreground ,operators))))
   `(tree-sitter-hl-face:string ((t (:foreground ,strings))))
   `(tree-sitter-hl-face:string.special ((t (:foreground ,green))))
   `(tree-sitter-hl-face:tag ((t (:foreground ,red))))
   `(tree-sitter-hl-face:type ((t (:foreground ,type))))
   `(tree-sitter-hl-face:type.argument ((t (:foreground ,yellow))))
   `(tree-sitter-hl-face:type.builtin ((t (:foreground ,yellow))))
   `(tree-sitter-hl-face:type.parameter ((t (:foreground ,yellow))))
   `(tree-sitter-hl-face:type.super ((t (:foreground ,yellow))))
   `(tree-sitter-hl-face:variable ((t (:foreground ,variables))))
   `(tree-sitter-hl-face:variable.builtin ((t (:foreground ,magenta))))
   `(tree-sitter-hl-face:variable.parameter ((t (:foreground ,pink))))
   `(tree-sitter-hl-face:variable.special ((t (:foreground ,magenta))))

   ;; Vertico
   `(vertico-current ((t (:background ,selection :extend t))))
   `(vertico-group-separator ((t (:foreground ,comments))))
   `(vertico-group-title ((t (:foreground ,comments))))

   ;; Which-key
   `(which-key-command-description-face ((t (:foreground ,fg))))
   `(which-key-group-description-face ((t (:foreground ,magenta))))
   `(which-key-local-map-description-face ((t (:foreground ,cyan))))
   `(which-key-key-face ((t (:foreground ,blue))))

   ;; Whitespace
   `(whitespace-empty ((t (:background ,base3))))
   `(whitespace-hspace ((t (:foreground ,base4))))
   `(whitespace-indentation ((t (:foreground ,base4 :background ,base3))))
   `(whitespace-line ((t (:background ,base3))))
   `(whitespace-newline ((t (:foreground ,base4))))
   `(whitespace-space ((t (:foreground ,base4))))
   `(whitespace-tab ((t (:foreground ,base4 :background ,base3))))
   `(whitespace-trailing ((t (:inherit trailing-whitespace))))

   ;; Window-divider
   `(window-divider ((t (:foreground ,base4))))
   `(window-divider-first-pixel ((t (:foreground ,base4))))
   `(window-divider-last-pixel ((t (:foreground ,base4))))

   ;; Compilation
   `(compilation-column-number ((t (:inherit font-lock-comment-face))))
   `(compilation-line-number ((t (:foreground ,highlight))))
   `(compilation-error ((t (:inherit error :weight bold))))
   `(compilation-warning ((t (:inherit warning :slant italic))))
   `(compilation-info ((t (:inherit success))))

   ;; Ansi colors
   `(ansi-color-black ((t (:foreground ,bg :background ,bg))))
   `(ansi-color-red ((t (:foreground ,red :background ,red))))
   `(ansi-color-green ((t (:foreground ,green :background ,green))))
   `(ansi-color-yellow ((t (:foreground ,yellow :background ,yellow))))
   `(ansi-color-blue ((t (:foreground ,blue :background ,blue))))
   `(ansi-color-magenta ((t (:foreground ,magenta :background ,magenta))))
   `(ansi-color-cyan ((t (:foreground ,cyan :background ,cyan))))
   `(ansi-color-white ((t (:foreground ,fg :background ,fg))))
   `(ansi-color-bright-black ((t (:foreground ,base6 :background ,base6))))
   `(ansi-color-bright-red ((t (:foreground ,(moonlight--lighten red 0.15) :background ,(moonlight--lighten red 0.15)))))
   `(ansi-color-bright-green ((t (:foreground ,(moonlight--lighten green 0.15) :background ,(moonlight--lighten green 0.15)))))
   `(ansi-color-bright-yellow ((t (:foreground ,(moonlight--lighten yellow 0.15) :background ,(moonlight--lighten yellow 0.15)))))
   `(ansi-color-bright-blue ((t (:foreground ,(moonlight--lighten blue 0.15) :background ,(moonlight--lighten blue 0.15)))))
   `(ansi-color-bright-magenta ((t (:foreground ,(moonlight--lighten magenta 0.15) :background ,(moonlight--lighten magenta 0.15)))))
   `(ansi-color-bright-cyan ((t (:foreground ,(moonlight--lighten cyan 0.15) :background ,(moonlight--lighten cyan 0.15)))))
   `(ansi-color-bright-white ((t (:foreground ,base8 :background ,base8))))

   ;; Avy
   `(avy-lead-face ((t (:background ,highlight :foreground ,bg :weight bold))))
   `(avy-lead-face-0 ((t (:inherit avy-lead-face :background ,(moonlight--lighten highlight 0.3)))))
   `(avy-lead-face-1 ((t (:inherit avy-lead-face :background ,(moonlight--lighten highlight 0.6)))))
   `(avy-lead-face-2 ((t (:inherit avy-lead-face :background ,(moonlight--lighten highlight 0.9)))))
   `(avy-background-face ((t (:foreground ,comments))))

   ;; Consult
   `(consult-file ((t (:foreground ,fg))))
   `(consult-bookmark ((t (:foreground ,blue))))
   `(consult-buffer ((t (:foreground ,fg))))
   `(consult-grep-context ((t (:foreground ,comments))))
   `(consult-help ((t (:foreground ,fg))))
   `(consult-key ((t (:foreground ,blue))))
   `(consult-line-number ((t (:foreground ,base5))))
   `(consult-line-number-prefix ((t (:foreground ,base5))))
   `(consult-preview-line ((t (:background ,selection :extend t))))
   `(consult-preview-match ((t (:inherit match))))
   `(consult-separator ((t (:foreground ,comments))))

   ;; Embark
   `(embark-keybinding ((t (:foreground ,blue))))
   `(embark-target ((t (:foreground ,yellow :weight bold))))

   ;; Marginalia
   `(marginalia-key ((t (:foreground ,blue))))
   `(marginalia-value ((t (:foreground ,violet))))
   `(marginalia-documentation ((t (:foreground ,comments))))
   `(marginalia-file-priv-dir ((t (:foreground ,blue))))
   `(marginalia-file-priv-read ((t (:foreground ,yellow))))
   `(marginalia-file-priv-write ((t (:foreground ,red))))
   `(marginalia-file-priv-exec ((t (:foreground ,green))))

   ;; Eshell
   `(eshell-prompt ((t (:foreground ,highlight :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,magenta))))
   `(eshell-ls-backup ((t (:foreground ,comments))))
   `(eshell-ls-clutter ((t (:foreground ,red))))
   `(eshell-ls-directory ((t (:foreground ,blue))))
   `(eshell-ls-executable ((t (:foreground ,green))))
   `(eshell-ls-missing ((t (:foreground ,red))))
   `(eshell-ls-product ((t (:foreground ,orange))))
   `(eshell-ls-readonly ((t (:foreground ,orange))))
   `(eshell-ls-special ((t (:foreground ,violet))))
   `(eshell-ls-symlink ((t (:foreground ,cyan))))
   `(eshell-ls-unreadable ((t (:foreground ,base5))))

   ;; Term / Vterm
   `(term-color-black ((t (:foreground ,bg :background ,bg))))
   `(term-color-red ((t (:foreground ,red :background ,red))))
   `(term-color-green ((t (:foreground ,green :background ,green))))
   `(term-color-yellow ((t (:foreground ,yellow :background ,yellow))))
   `(term-color-blue ((t (:foreground ,blue :background ,blue))))
   `(term-color-magenta ((t (:foreground ,magenta :background ,magenta))))
   `(term-color-cyan ((t (:foreground ,cyan :background ,cyan))))
   `(term-color-white ((t (:foreground ,fg :background ,fg))))
   `(vterm-color-black ((t (:foreground ,bg :background ,base4))))
   `(vterm-color-red ((t (:foreground ,red :background ,red))))
   `(vterm-color-green ((t (:foreground ,green :background ,green))))
   `(vterm-color-yellow ((t (:foreground ,yellow :background ,yellow))))
   `(vterm-color-blue ((t (:foreground ,blue :background ,blue))))
   `(vterm-color-magenta ((t (:foreground ,magenta :background ,magenta))))
   `(vterm-color-cyan ((t (:foreground ,cyan :background ,cyan))))
   `(vterm-color-white ((t (:foreground ,fg :background ,fg))))

   ;; Ediff
   `(ediff-current-diff-A ((t (:foreground ,red :background ,(moonlight--blend red bg 0.2)))))
   `(ediff-current-diff-B ((t (:foreground ,green :background ,(moonlight--blend green bg 0.2)))))
   `(ediff-current-diff-C ((t (:foreground ,blue :background ,(moonlight--blend blue bg 0.2)))))
   `(ediff-even-diff-A ((t (:background ,base3))))
   `(ediff-even-diff-B ((t (:background ,base3))))
   `(ediff-even-diff-C ((t (:background ,base3))))
   `(ediff-odd-diff-A ((t (:background ,base3))))
   `(ediff-odd-diff-B ((t (:background ,base3))))
   `(ediff-odd-diff-C ((t (:background ,base3))))
   `(ediff-fine-diff-A ((t (:background ,(moonlight--blend red bg 0.4) :weight bold))))
   `(ediff-fine-diff-B ((t (:background ,(moonlight--blend green bg 0.4) :weight bold))))
   `(ediff-fine-diff-C ((t (:background ,(moonlight--blend blue bg 0.4) :weight bold))))

   ;; Smerge
   `(smerge-lower ((t (:background ,(moonlight--blend green bg 0.1)))))
   `(smerge-upper ((t (:background ,(moonlight--blend red bg 0.1)))))
   `(smerge-base ((t (:background ,(moonlight--blend blue bg 0.1)))))
   `(smerge-markers ((t (:background ,base3 :foreground ,fg :weight bold))))

   ;; Diff
   `(diff-header ((t (:foreground ,fg :background ,base3))))
   `(diff-file-header ((t (:foreground ,fg :background ,base3 :weight bold))))
   `(diff-added ((t (:foreground ,green :background ,(moonlight--blend green bg 0.1)))))
   `(diff-removed ((t (:foreground ,red :background ,(moonlight--blend red bg 0.1)))))
   `(diff-changed ((t (:foreground ,blue :background ,(moonlight--blend blue bg 0.1)))))
   `(diff-refine-added ((t (:foreground ,green :background ,(moonlight--blend green bg 0.3) :weight bold))))
   `(diff-refine-removed ((t (:foreground ,red :background ,(moonlight--blend red bg 0.3) :weight bold))))
   `(diff-refine-changed ((t (:foreground ,blue :background ,(moonlight--blend blue bg 0.3) :weight bold))))

   ;; Completions
   `(completions-common-part ((t (:foreground ,blue :weight bold))))
   `(completions-first-difference ((t (:foreground ,magenta :weight bold))))
   `(completions-annotations ((t (:foreground ,violet))))

   ;; Xref
   `(xref-file-header ((t (:foreground ,blue :weight bold))))
   `(xref-line-number ((t (:foreground ,base5))))
   `(xref-match ((t (:inherit match))))

   ;; Eldoc
   `(eldoc-highlight-function-argument ((t (:foreground ,yellow :weight bold))))

   ;; Minibuffer
   `(minibuffer-prompt ((t (:foreground ,highlight))))

   ;; Helpful
   `(helpful-heading ((t (:foreground ,blue :weight bold :height 1.2))))

   ;; Info
   `(info-title-1 ((t (:foreground ,blue :weight bold :height 1.4))))
   `(info-title-2 ((t (:foreground ,blue :weight bold :height 1.3))))
   `(info-title-3 ((t (:foreground ,blue :weight bold :height 1.2))))
   `(info-title-4 ((t (:foreground ,blue :weight bold :height 1.1))))
   `(info-header-node ((t (:foreground ,magenta))))
   `(info-header-xref ((t (:foreground ,blue :underline t))))
   `(info-menu-star ((t (:foreground ,red))))

   ;; Custom
   `(custom-button ((t (:foreground ,blue :background ,bg :box (:line-width 1 :style nil)))))
   `(custom-button-pressed ((t (:foreground ,bg :background ,blue :box (:line-width 1 :style nil)))))
   `(custom-button-mouse ((t (:foreground ,bg :background ,blue :box (:line-width 1 :style nil)))))
   `(custom-group-tag ((t (:foreground ,violet :weight bold :height 1.2))))
   `(custom-variable-tag ((t (:foreground ,magenta :weight bold))))
   `(custom-state ((t (:foreground ,green))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'moonlight)

;;; moonlight-theme.el ends here
