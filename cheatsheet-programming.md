# Programming Mode Cheatsheet

## Eglot (LSP Client)

### Connection Management

| Command | Description |
|---------|-------------|
| `M-x eglot` | Start LSP server for current buffer |
| `M-x eglot-shutdown` | Stop LSP server |
| `M-x eglot-reconnect` | Restart LSP connection |

*Auto-enabled for: Python, Ruby, JavaScript, Clojure, OCaml, Go*

### Code Navigation (Xref)

| Binding | Command | Description |
|---------|---------|-------------|
| `M-.` | `xref-find-definitions` | Go to definition |
| `M-,` | `xref-go-back` | Return to previous location |
| `M-?` | `xref-find-references` | Find all references |

### Code Actions

| Command | Description |
|---------|-------------|
| `M-x eglot-code-actions` | Show available code actions |
| `M-x eglot-rename` | Rename symbol across project |
| `M-x eglot-format-buffer` | Format entire buffer |
| `M-x eglot-format` | Format selection/region |

## Eldoc (Documentation)

*Shows documentation in echo area automatically (0.75s delay)*

| Command | Description |
|---------|-------------|
| `M-x eldoc` | Show documentation for symbol at point |
| `M-x eldoc-doc-buffer` | Show documentation in separate buffer |

## Flycheck (Syntax Checking)

*Auto-enabled in prog-mode, checks on save*

| Command | Description |
|---------|-------------|
| `C-c ! n` | Next error |
| `C-c ! p` | Previous error |
| `C-c ! l` | List all errors |
| `C-c ! c` | Check buffer now |
| `C-c ! C` | Clear errors |
| `C-c ! v` | Verify setup |
| `C-c ! x` | Disable flycheck |

### Flycheck Error Navigation

| Command | Description |
|---------|-------------|
| `M-x flycheck-next-error` | Go to next error |
| `M-x flycheck-previous-error` | Go to previous error |
| `M-x flycheck-list-errors` | Show error list buffer |

## Corfu (In-Buffer Completion)

*Auto-popup after 3 characters (0.2s delay)*

| Binding | Description |
|---------|-------------|
| `TAB` | Select next candidate |
| `S-TAB` | Select previous candidate |
| `RET` | Complete and quit |
| `C-g` | Cancel completion |
| `M-n` | Scroll popup down |
| `M-p` | Scroll popup up |

### Cape (Additional Completions)

Fallback completions enabled:
- `cape-dabbrev` - Words from buffer
- `cape-file` - File paths

## Imenu (Code Outline)

| Binding | Command | Description |
|---------|---------|-------------|
| `s-i` | `consult-imenu` | Jump to function/class/definition |

*Custom imenu expressions configured for: Clojure, Emacs Lisp, Python, Ruby, TypeScript*

## Helpful (Enhanced Help)

| Binding | Command | Description |
|---------|---------|-------------|
| `C-h f` | `helpful-callable` | Describe function |
| `C-h v` | `helpful-variable` | Describe variable |
| `C-h k` | `helpful-key` | Describe key binding |
| `C-h x` | `helpful-command` | Describe command |

## Dumb Jump (Fallback Navigation)

*Used automatically when Eglot is not available*

Uses ripgrep to find definitions by pattern matching.

## Emacs Lisp Mode

| Binding | Command | Description |
|---------|---------|-------------|
| `C-x C-e` | `eval-last-sexp` | Evaluate expression before point |
| `C-c C-c` | `kg/eval-root-expression` | Evaluate top-level form |
| `M-x eval-buffer` | Evaluate entire buffer |
| `M-x byte-compile-file` | Compile elisp file |

## Apheleia (Auto-formatting)

*Auto-enabled in prog-mode, formats on save*

| Command | Description |
|---------|-------------|
| `M-x apheleia-format-buffer` | Format buffer manually |
| `M-x apheleia-mode` | Toggle auto-format |

Configured formatters:
- Python: ruff-isort + ruff

## Dape (Debugger)

| Command | Description |
|---------|-------------|
| `M-x dape` | Start debugger |
| `M-x dape-breakpoint-toggle` | Toggle breakpoint |
| `M-x dape-next` | Step over |
| `M-x dape-step-in` | Step into |
| `M-x dape-step-out` | Step out |
| `M-x dape-continue` | Continue execution |
| `M-x dape-quit` | Stop debugging |

## Code Editing

| Binding | Command | Description |
|---------|---------|-------------|
| `C-c d` | `duplicate-dwim` | Duplicate line or region |
| `M-S-down` | `kg/duplicate-start-of-line-or-region` | Duplicate line/region |
| `s-<backspace>` | `kill-whole-line` | Delete entire line |

## Definition Finding (Smart)

| Command | Description |
|---------|-------------|
| `M-x kg/find-definition` | Find definition (Eglot -> ripgrep fallback) |
| `M-x kg/rg-find-definitions` | Find definitions using ripgrep |

## which-key

*Shows available keybindings after prefix (built-in Emacs 30+)*

Press any prefix key and wait to see available completions.

## EditorConfig

*Auto-enabled - respects `.editorconfig` files for indent style, size, etc.*

## Vertico (Minibuffer Completion)

*Enhanced minibuffer with fuzzy matching (orderless)*

| Key | Description |
|-----|-------------|
| `C-n` / `down` | Next candidate |
| `C-p` / `up` | Previous candidate |
| `RET` | Select candidate |
| `TAB` | Complete common prefix |

## Compile Mode

| Binding | Description |
|---------|-------------|
| `M-x compile` | Run compile command |
| `M-x recompile` | Re-run last compile |
| `g` | Recompile (in compile buffer) |
| `C-c C-k` | Kill compilation |
| `M-g n` | Next error |
| `M-g p` | Previous error |
