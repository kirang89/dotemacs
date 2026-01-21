# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is an Emacs configuration targeting Emacs 30+. It uses **straight.el** for reproducible package management and **use-package** for declarative configuration. The design prioritizes performance through lazy loading, native compilation, and optimized garbage collection.

## Architecture

### Module Load Order
Configuration modules in `lisp/` are loaded sequentially with dependencies flowing downward:

```
init.el (bootstrap)
  → init-efuns.el (custom utility functions)
    → config-tools.el (compile-angel, GCMH, exec-path)
      → config-ui.el (themes, fonts, modeline)
        → config-completion.el (vertico, corfu, consult)
          → config-search.el (ripgrep, embark, avy)
            → config-files.el (projectile, dired)
              → config-editing.el (paredit, smartparens)
                → config-vcs.el (magit, diff-hl)
                  → config-lsp.el (eglot, xref, eldoc)
                    → config-languages.el (language modes)
                      → config-ai.el (ellama, minuet, aidermacs)
```

### Key Design Decisions
- **Eglot over LSP-mode**: Built-in, lighter weight, faster
- **Vertico+Corfu over Helm/Company**: Modern, native integration
- **Projectile with alien indexing**: Uses git/fd for fastest project detection
- **straight.el pseudo-packages**: `emacs`, `use-package`, `project`, `eglot`, `xref`, `cl-lib`, `eldoc`, `flymake`, `repeat`, `jsonrpc` are declared as built-ins

### Configuration Patterns
- Packages use `:defer t` or `:defer <seconds>` for lazy loading
- Custom functions prefixed with `kg/` in `init-efuns.el`
- User customizations isolated in `init-user.el` (separate from main config)
- macOS keybindings use `s-` (Command) and `M-` (Option)

## Testing Changes

After modifying elisp files:

```elisp
;; Evaluate a single expression
C-x C-e

;; Evaluate top-level form (in emacs-lisp-mode)
C-c C-c

;; Byte-compile a file
M-x byte-compile-file

;; Check for issues
M-x checkdoc  ;; (disabled by default in this config)
M-x package-lint-current-buffer
```

To reload a module:
```elisp
(load-file "~/.emacs.d/lisp/config-foo.el")
```

## Key Bindings Reference

| Binding | Function |
|---------|----------|
| `C-g` | `kg/keyboard-quit-dwim` (context-aware quit) |
| `C-a` | `kg/beginning-of-line-dwim` (toggle indentation/start) |
| `C-x 2/3` | Split and move to new window |
| `s-{`/`s-}` | Previous/next buffer |
| `s-<S-return>` | Toggle maximize buffer |
| `s-h` | `avy-goto-char-timer` |
| `M-s d` | `consult-ripgrep` (project search) |
| `M-s l` | `consult-line` (buffer search) |
| `F6` | Open `init-user.el` |

## Performance Infrastructure

- **compile-angel**: Auto byte/native compiles on save
- **GCMH**: Smart garbage collection (64MB threshold, 15s idle delay)
- **eglot-booster**: Fast JSON processing for LSP
- **Tree-sitter**: Level 3 font-lock for performance balance

## Language Support

Languages configured with Eglot: Python, Ruby, JavaScript, Clojure, OCaml, Go

AI integration via `config-ai.el`:
- Ellama (Llama 3.1 via Ollama) - `C-c e` prefix
- Minuet (Qwen 3 8B) - code completion
- Aidermacs - multi-file AI editing
