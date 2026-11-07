# Macaulay2 editors

This directory contains the editor and syntax-highlighting assets that ship
with Macaulay2. The shared generator script is
[`make-M2-symbols.m2`](make-M2-symbols.m2); when run from this directory, it
rewrites the symbol files used by Emacs, Prism, Pygments, and Vim.

Each subdirectory documents a particular integration:

* [`emacs/README.md`](emacs/README.md): running and editing Macaulay2 in GNU
  Emacs, including the bundled support files.
* [`vim/README.md`](vim/README.md): running Macaulay2 inside Vim, including
  the current setup notes for GNU/Linux and macOS.
* [`prism/README.md`](prism/README.md): generating and using the Prism grammar
  used for syntax highlighting the HTML documentation.
* [`pygments/README.md`](pygments/README.md): generating and using the Pygments
  lexer.

Several related integrations are maintained in standalone repositories:

* highlight.js: https://github.com/d-torrance/highlightjs-macaulay2
* Linguist: https://github.com/Macaulay2/language-macaulay2
* TextMate: https://github.com/Macaulay2/vscode-macaulay2
* Macaulay2Web (symbols for autocompletion): https://github.com/pzinn/Macaulay2Web/
