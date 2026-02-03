#!/bin/bash
# Clean Emacs packages for fresh rebuild

set -e

EMACS_DIR="$HOME/.emacs.d"

echo "Cleaning Emacs for fresh rebuild..."

# Remove straight.el directory
if [ -d "$EMACS_DIR/straight" ]; then
    echo "Removing straight directory..."
    rm -rf "$EMACS_DIR/straight"
fi

# Remove compiled elisp files
echo "Removing .elc files..."
find "$EMACS_DIR" -name "*.elc" -type f -delete 2>/dev/null || true

# Remove native compilation cache
if [ -d "$EMACS_DIR/eln-cache" ]; then
    echo "Removing eln-cache..."
    rm -rf "$EMACS_DIR/eln-cache"
fi

echo "Done. Run 'emacs --batch -l init.el' to rebuild packages."
