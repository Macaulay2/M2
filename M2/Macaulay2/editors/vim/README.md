# Vim Plugin for Macaulay2

Syntax highlighting and key mappings to send code to a running Macaulay2 session.

> **Neovim users:** The [macaulay2.nvim](https://github.com/Macaulean/macaulay2.nvim) plugin provides a more complete Neovim integration.

## Requirements

- Vim 8.1+
- `M2` on your `PATH`

## Installation

```sh
cp -r dict ftdetect ftplugin syntax ~/.vim/
```

Then, in Macaulay2, generate the symbol files.  You may also do this after a
new Macaulay2 release to update the files with any new symbols.

```m2
needsPackage "Style"
changeDirectory "~/.vim"
generateGrammar("dict/m2.vim.dict", demark_" ")
generateGrammar("syntax/m2.vim", demark_" ")
```

Ensure your `~/.vimrc` contains:

```vim
filetype plugin on
syntax on
```

## Usage

### Key mappings

| Key | Mode | Action |
|-----|------|--------|
| `F12` | Normal | Start M2 in a terminal split (or switch to it) |
| `F11` | Normal/Visual | Send current line or visual selection to M2 |
| `F11` | Insert | Send current line to M2 and continue editing |
| `Tab` | Insert | Complete word from dictionary, or insert tab |

### Commands

| Command | Action |
|---------|--------|
| `:M2Start` | Open M2 in a terminal split, or switch to it if already running |
| `:M2Restart` | Restart M2 |
| `:M2Exit` | Exit M2 |
| `:M2Send` | Send current line or visual selection to M2 |
| `:M2SendBuffer` | Send the entire buffer to M2 |
| `:M2SendString {str}` | Send a string to M2 |
