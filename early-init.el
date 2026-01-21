;; -*- lexical-binding: t; -*-

;; Prevent package.el from loading before straight.el
(setq package-enable-at-startup nil)

;; Increase GC threshold during startup for faster loading
(setq gc-cons-threshold 50000000
      gc-cons-percentage 0.6)

;; Skip fontification during rapid input - major typing responsiveness boost
(setq redisplay-skip-fontification-on-input t)

;; Disable bidirectional text processing (not needed for LTR-only languages)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Prevent frame resizing during startup
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Prevent glimpse of unstyled UI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Initial frame size and position
(push '(height . 43) default-frame-alist)
(push '(width . 130) default-frame-alist)
(push '(left . 70) default-frame-alist)
(push '(top . 30) default-frame-alist)
