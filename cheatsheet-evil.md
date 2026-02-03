# Evil Mode Cheatsheet

## States

| Key | State | Cursor |
|-----|-------|--------|
| `ESC` | Normal | Block |
| `i` / `a` / `o` | Insert | Bar |
| `v` / `V` / `C-v` | Visual | Block highlight |
| `:` | Command | Minibuffer |

## Core Vim Motions

| Key | Action |
|-----|--------|
| `h/j/k/l` | Left/Down/Up/Right |
| `w/b/e` | Word forward/backward/end |
| `0` / `$` | Line start/end |
| `gg` / `G` | File start/end |
| `C-u` / `C-d` | Page up/down |
| `%` | Jump to matching bracket (evil-matchit extends this) |
| `f{c}` / `t{c}` | Find/Till character |
| `*` / `#` | Search word under cursor forward/backward |

## Editing

| Key | Action |
|-----|--------|
| `dd` / `yy` / `p` | Delete/Yank/Paste line |
| `d{motion}` | Delete with motion |
| `c{motion}` | Change with motion |
| `u` / `C-r` | Undo/Redo |
| `J` | Join lines |
| `.` | Repeat last change |
| `>>` / `<<` | Indent/Dedent |

## Surround (evil-surround)

| Key | Action |
|-----|--------|
| `ys{motion}{char}` | Add surround (e.g. `ysiw"` wraps word in quotes) |
| `cs{old}{new}` | Change surround (e.g. `cs"'` changes `"` to `'`) |
| `ds{char}` | Delete surround (e.g. `ds"` removes quotes) |
| `S{char}` | Surround selection (visual mode) |

## Commentary (evil-commentary)

| Key | Action |
|-----|--------|
| `gc{motion}` | Comment/uncomment (e.g. `gcc` = current line) |
| `gc` | Comment selection (visual mode) |

## Multiple Cursors (evil-mc)

| Key | Action |
|-----|--------|
| `grm` | Make cursors for all matches |
| `gru` | Undo last cursor |
| `grq` | Remove all cursors |
| `C-n` / `C-p` | Make cursor & go next/prev (in visual) |

## SPC Leader Keys

### Top-Level

| Key | Action |
|-----|--------|
| `SPC SPC` | M-x (execute command) |
| `SPC x` | M-x (execute command) |

### Buffer (`SPC b`)

| Key | Action |
|-----|--------|
| `SPC b b` | Switch buffer (consult) |
| `SPC b d` | Kill buffer |
| `SPC b n` | Next buffer |
| `SPC b p` | Previous buffer |
| `SPC b i` | ibuffer |
| `SPC b R` | Rename buffer |

### File (`SPC f`)

| Key | Action |
|-----|--------|
| `SPC f f` | Find file in project |
| `SPC f F` | Find file |
| `SPC f r` | Recent files |
| `SPC f s` | Save file |
| `SPC f c` | Open config |
| `SPC f R` | Rename file |
| `SPC f D` | Delete file |

### Window (`SPC w`)

| Key | Action |
|-----|--------|
| `SPC w w` | Ace window |
| `SPC w s` | Split below |
| `SPC w v` | Split right |
| `SPC w d` | Delete window |
| `SPC w o` | Delete other windows |
| `SPC w m` | Maximize buffer |
| `SPC w h/j/k/l` | Move left/down/up/right |

### Project (`SPC p`)

| Key | Action |
|-----|--------|
| `SPC p` | Projectile command map |

### Goto (`SPC g`)

| Key | Action |
|-----|--------|
| `SPC g d` | Go to definition |
| `SPC g i` | Go to implementation |
| `SPC g r` | Find references |
| `SPC g t` | Go to type definition |
| `SPC g l` | Go to line |
| `SPC g b` | Go back |

### Magit (`SPC m`)

| Key | Action |
|-----|--------|
| `SPC m m` | Magit status |
| `SPC m c` | Checkout |
| `SPC m P` | Pull rebase autostash |
| `SPC m d` | Diff buffer file |
| `SPC m l` | Log buffer file |
| `SPC m b` | Blame |
| `SPC m s` | Stage file |
| `SPC m u` | Unstage file |
| `SPC m t` | Git timemachine |

### Search (`SPC s`)

| Key | Action |
|-----|--------|
| `SPC s s` / `SPC s l` | Search in buffer (consult-line) |
| `SPC s p` / `SPC s r` | Ripgrep project |
| `SPC s g` | Ripgrep with region |
| `SPC s i` | Imenu |
| `SPC s o` | Outline |
| `SPC s R` | Query replace |

### Code (`SPC c`)

| Key | Action |
|-----|--------|
| `SPC c d` | Duplicate (dwim) |
| `SPC c f` | Format buffer |
| `SPC c o` | Symbol outline |
| `SPC c R` | Rename (eglot) |
| `SPC c a` | Code actions (eglot) |

### AI (`SPC a`)

| Key | Action |
|-----|--------|
| `SPC a a` | Aidermacs |
| `SPC a e` | Ellama |

### Jump (`SPC j`)

| Key | Action |
|-----|--------|
| `SPC j j` | Avy jump (char timer) |
| `SPC j l` | Go to line |
| `SPC j i` | Imenu |

### Help (`SPC h`)

| Key | Action |
|-----|--------|
| `SPC h f` | Describe function |
| `SPC h v` | Describe variable |
| `SPC h k` | Describe key |
| `SPC h x` | Describe command |

### Toggle (`SPC t`)

| Key | Action |
|-----|--------|
| `SPC t l` | Toggle line numbers |
| `SPC t w` | Toggle whitespace |
| `SPC t f` | Toggle flycheck |

### Quit (`SPC q`)

| Key | Action |
|-----|--------|
| `SPC q q` | Quit Emacs |

### Open (`SPC o`)

| Key | Action |
|-----|--------|
| `SPC o d` | Dired jump |
| `SPC o t` | Terminal (vterm in new frame) |

## Local Leader (`,`)

### Emacs Lisp Mode

| Key | Action |
|-----|--------|
| `, e e` | Eval last sexp |
| `, e r` | Eval root expression |
| `, e b` | Eval buffer |

## Preserved Emacs Bindings

| Key | Action |
|-----|--------|
| `C-g` | keyboard-quit-dwim (all states) |
| `C-a` | beginning-of-line-dwim (normal/insert) |
| `M-.` / `M-,` | xref find/go back |
| `M-j` | Join line |
| `v` / `V` (visual) | Expand/Contract region |
