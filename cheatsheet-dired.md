# Dired Cheatsheet

## Opening Dired

| Command | Description |
|---------|-------------|
| `C-x d` | Open dired in prompted directory |
| `C-x C-j` | Open dired at current file (dired-jump) |

## Navigation

| Key | Description |
|-----|-------------|
| `n` / `SPC` | Move to next line |
| `p` | Move to previous line |
| `j` | Jump to file by name |
| `<` | Jump to previous directory |
| `>` | Jump to next directory |
| `^` | Go to parent directory |
| `RET` | Open file/directory |
| `o` | Open in other window |
| `v` | View file (read-only) |

## Marking Files

| Key | Description |
|-----|-------------|
| `m` | Mark file |
| `u` | Unmark file |
| `U` | Unmark all |
| `t` | Toggle marks (invert selection) |
| `* /` | Mark all directories |
| `* *` | Mark all executables |
| `* .` | Mark by extension |
| `% m` | Mark by regexp |
| `% g` | Mark files containing regexp |

## File Operations

| Key | Description |
|-----|-------------|
| `C` | Copy marked files |
| `R` | Rename/move marked files |
| `D` | Delete marked files |
| `S` | Create symbolic link |
| `H` | Create hard link |
| `M` | Change mode (chmod) |
| `G` | Change group |
| `O` | Change owner |
| `T` | Touch (update timestamp) |
| `Z` | Compress/uncompress |
| `!` | Shell command on file |
| `&` | Async shell command on file |

## Creating Files/Directories

| Key | Description |
|-----|-------------|
| `+` | Create directory |
| `C-x C-f` | Create new file (find-file) |

## Dired Ranger (Copy/Paste Across Buffers)

*Custom bindings from config*

| Key | Command | Description |
|-----|---------|-------------|
| `W` | `dired-ranger-copy` | Copy (yank) marked files to ring |
| `X` | `dired-ranger-move` | Stage marked files for move |
| `Y` | `dired-ranger-paste` | Paste files from ring |

## Dired Subtree (Tree View)

*Custom bindings from config*

| Key | Command | Description |
|-----|---------|-------------|
| `TAB` | `dired-subtree-toggle` | Expand/collapse directory inline |
| `S-TAB` | `dired-subtree-cycle` | Cycle subtree visibility |

## Dired Collapse

*Enabled automatically - collapses empty intermediate directories*

## Display and Sorting

| Key | Description |
|-----|-------------|
| `s` | Toggle sorting (name/date) |
| `(` | Toggle details visibility |
| `g` | Refresh buffer |
| `l` | Redisplay |

## Search in Dired

| Key | Description |
|-----|-------------|
| `A` | Search (regexp) in marked files |
| `Q` | Query-replace (regexp) in marked files |
| `% R` | Rename by regexp |

## Wdired - Edit Filenames Directly

| Command | Description |
|---------|-------------|
| `C-x C-q` | Enter wdired mode (editable filenames) |
| `C-c C-c` | Commit changes (in wdired mode) |
| `C-c ESC` | Abort changes (in wdired mode) |

## Bulk Operations Workflow

1. Mark files with `m` or pattern matching (`% m`)
2. Perform operation:
   - `C` - Copy all marked
   - `R` - Move all marked
   - `D` - Delete all marked
3. For cross-buffer copy/move:
   - `W` - Copy to ring
   - Navigate to destination
   - `Y` - Paste

## DWIM Target

*Enabled in config: when two dired buffers are open side-by-side, copy/move operations default to the other buffer's directory*

## Opening Files with External Apps

*Configured for macOS with `open` command*

| Key | Description |
|-----|-------------|
| `!` | Run shell command (use `open` for default app) |
| `&` | Run async shell command |

File associations configured:
- Images (png, jpg, tiff) -> `open`
- Media (mp4, mkv, etc.) -> `mpv` or `open`
- Others -> `open`

## Useful Commands (M-x)

| Command | Description |
|---------|-------------|
| `dired-do-find-regexp` | Search files by content |
| `dired-do-find-regexp-and-replace` | Search and replace in files |
| `dired-hide-details-mode` | Toggle file details |
| `dired-narrow` | Filter displayed files (if installed) |
