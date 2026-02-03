# Navigation Cheatsheet

## Avy - Jump to Visible Text

| Binding | Command | Description |
|---------|---------|-------------|
| `s-h` | `avy-goto-char-timer` | Type chars, jump to match |

## Window Management

| Binding | Command | Description |
|---------|---------|-------------|
| `M-o` | `ace-window` | Jump to window by number |
| `C-x 2` | `kg/split-below-and-move` | Split below and focus new window |
| `C-x 3` | `kg/split-right-and-move` | Split right and focus new window |
| `s-S-return` | `kg/toggle-maximize-buffer` | Toggle maximize current window |
| `s-{` | `previous-buffer` | Previous buffer |
| `s-}` | `next-buffer` | Next buffer |
| `s-w` | `kill-current-buffer` | Close current buffer |
| `C-x k` | `kill-current-buffer` | Close current buffer |

## Project (Projectile)

Prefix: `s-p` (then key below)

| Key | Command | Description |
|-----|---------|-------------|
| `p` | `projectile-switch-project` | Switch to project |
| `f` | `projectile-find-file` | Find file in project |
| `b` | `projectile-switch-to-buffer` | Switch to project buffer |
| `d` | `projectile-find-dir` | Find directory |
| `s g` | `projectile-grep` | Grep in project |
| `s r` | `projectile-ripgrep` | Ripgrep in project |
| `k` | `projectile-kill-buffers` | Kill all project buffers |
| `!` | `projectile-run-shell-command-in-root` | Run shell command |

## Find File in Project

| Binding | Command | Description |
|---------|---------|-------------|
| `s-t` | `find-file-in-project` | Fast file finder (uses fd) |

## Consult - Enhanced Navigation

| Binding | Command | Description |
|---------|---------|-------------|
| `s-b` | `consult-buffer` | Switch buffer with preview |
| `s-l` | `consult-goto-line` | Go to line |
| `M-g o` | `consult-outline` | Jump to heading/outline |
| `s-i` | `consult-imenu` | Jump to symbol in buffer |
| `C-x 4 b` | `consult-buffer-other-window` | Buffer in other window |

## Search - In Buffer

| Binding | Command | Description |
|---------|---------|-------------|
| `s-f` / `C-s` | `isearch-forward` | Incremental search (uses region if active) |
| `s-r` | `query-replace-regexp` | Search and replace with regex |
| `s-l` | `consult-line` | Fuzzy search buffer lines |
| `M-s l` | `consult-line` | Search buffer lines |

### Isearch Commands (during search)

| Key | Description |
|-----|-------------|
| `C-s` | Next match |
| `C-r` | Previous match |
| `M-o` | `isearch-moccur` - Show all matches |
| `M-O` | `isearch-moccur-all` - All matches in all buffers |
| `RET` | Exit at current match |
| `C-g` | Cancel and return to start |

## Search - In Project (Ripgrep)

| Binding | Command | Description |
|---------|---------|-------------|
| `s-g` | `kg/consult-ripgrep-with-region` | Ripgrep project (uses region) |
| `M-g f` | `kg/fast-grep` | Fast grep with prompt |
| `M-g g` | `kg/fast-grep-symbol` | Grep symbol at point |
| `M-s O` | `moccur` | Multi-occur search |

### Ripgrep (rg) Package

| Command | Description |
|---------|-------------|
| `rg` | Interactive ripgrep |
| `rg-project` | Ripgrep in project |
| `rg-dwim` | Ripgrep with sensible defaults |
| `kg/grep-vc-or-dir` | Ripgrep in VC root or current dir |
| `deadgrep` | Alternative ripgrep interface |

## Movement

| Binding | Command | Description |
|---------|---------|-------------|
| `C-a` | `kg/beginning-of-line-dwim` | Toggle between indentation and line start |
| `s-<left>` | Beginning of line | Same as `C-a` |
| `s-<right>` | End of line | Same as `C-e` |
| `M-j` | Join line below | Join next line to current |
| `C-g` | `kg/keyboard-quit-dwim` | Smart quit (closes minibuffer, deactivates region) |

## Sibling Files (Implementation <-> Test)

| Binding | Command | Description |
|---------|---------|-------------|
| `C-c s` | `find-sibling-file` | Switch between impl/test files |

Configured for: Python, Clojure, OCaml

## Embark - Actions on Things

| Binding | Command | Description |
|---------|---------|-------------|
| `C-. ` | `embark-act` | Act on thing at point or selection |

## Harpoon - Quick File Marks

| Binding | Description |
|---------|-------------|
| `j1` (key-chord) | Jump to harpoon slot 1 |
| `j2` (key-chord) | Jump to harpoon slot 2 |
| `j3` (key-chord) | Jump to harpoon slot 3 |
| `j4` (key-chord) | Jump to harpoon slot 4 |

*Note: key-chord-mode must be enabled manually*
