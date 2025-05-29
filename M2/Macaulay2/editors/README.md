# Macaulay2 editors

This directory contains template files used by the
[generateGrammar](https://macaulay2.com/doc/Macaulay2/share/doc/Macaulay2/Style/html/_generate__Grammar.html) function in the *Style* package to generate
grammar files used by various editors for syntax highlighting and automatic
completion of Macaulay2 code.

Each subdirectory contains one or more template files for a particular
application.

* [`Macaulay2Web`](Macaulay2Web): Symbols for the Macaulay2 web app.
* [`emacs`](https://github.com/Macaulay2/M2-emacs): Symbols for GNU Emacs.
* [`prism`](prism): Symbols for the Prism Javascript library.  These are used
  for syntax highlighting the online Macaulay2 documentation.
* [`pygments`](pygments): Symbols for Pygments, a syntax highlighter written
  in Python.
* [`vim`](vim): Symbols for the Vim editor.

Several template files that formerly were kept in this directory have since
been moved to their own repositories:

* highlight.js: https://github.com/d-torrance/highlightjs-macaulay2
* Linguist: https://github.com/Macaulay2/language-macaulay2
* TextMate: https://github.com/Macaulay2/vscode-macaulay2
