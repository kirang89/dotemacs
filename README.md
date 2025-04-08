## dotemacs

This is the new home for my Emacs configuration, after a detox from my [old, bigger and more
customized setup](https://github.com/kirang89/.emacs.d).

## Setup

### Prerequisites

Before running Emacs, ensure the following are installed:
1. tree-sitter
2. isort
3. ruff
4. pandoc
5. ctags
6. fd

I use [straight](https://github.com/raxod502/straight.el) as my package manager, so please read a
bit about it, before setting up this configuration for your use.

To proceed, run the following command

```
git clone https://gitlab.com/nilenso/dotemacs ~/.emacs.d
```

and run Emacs. On the first run, it will install and compile the packages, so it might take a few
minutes.
