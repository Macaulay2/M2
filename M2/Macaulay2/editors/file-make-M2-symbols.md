# `make-M2-symbols.m2` — single-source-of-truth grammar generator

`make-M2-symbols.m2` is an **M2 script** that collects every
built-in symbol (keywords, types, functions, ...) from a running
M2 and emits per-editor grammar files. Run during the build via
`M2 --script make-M2-symbols.m2`.

Part of [`editors/`](README.md).

[← editors/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What it does

```m2
needsPackage "Style"

-- Emacs: Write M2-symbols.el
generateGrammar("emacs/M2-symbols.el", x -> demark(" ", format \ x))

-- Prism: Write macaulay2.js
generateGrammar("prism/macaulay2.js", x -> demark("|", x))

-- Vim: Write m2.vim.syntax and m2.vim.dict
generateGrammar("vim/m2.vim.syntax", x -> demark(" ", x))
generateGrammar("vim/m2.vim.dict", x -> demark(" ", x))

-- Pygments: Write macaulay2.py
generateGrammar("pygments/macaulay2.py",
    x -> demark("," | newline | "    ", format \ x))
```

The script:

1. Loads the `Style` package (provides `generateGrammar`).
2. For each editor target, calls `generateGrammar(filename,
   formatter)`.
3. `generateGrammar` reads the matching `.in` template, substitutes
   placeholders with **the actual list of symbols** (formatted by
   the per-editor formatter callback), and writes the output.

The formatters differ because each grammar format has different
separator conventions:

- **Emacs `.el`** — space-separated, quoted strings
  (`format \ x`).
- **Prism `.js`** — pipe-separated (regex alternation).
- **Vim `.syntax`** — space-separated, no quotes.
- **Pygments `.py`** — comma-then-newline-then-4-spaces, quoted.

## Why a single source of truth

Without this script, every editor's symbol list would drift over
time. Adding a new built-in function would require remembering to
update prism, pygments, vim, emacs, ... separately.

This script makes the M2 binary itself the canonical source:
**whatever the current M2 considers a built-in**, that's what
goes into every editor's grammar.

## Templates and `.in` files

The actual grammar files (`prism/macaulay2.js.in`,
`pygments/macaulay2.py.in`, `vim/m2.vim.syntax.in`, etc.) are
*templates*. They contain the editor-specific syntax structure
plus placeholders like `@SYMBOLS@` that get substituted. After
`generateGrammar` runs, the non-`.in` versions in each subdir are
the ready-to-ship grammars.

## Why isn't there an `.in` for `m2.vim.dict`?

Vim's "dict" is just a flat list of words, not a structured
grammar file. The output is generated directly without a
template.

## CMake target

The CMake module wires this up:

```cmake
set(GRAMMAR_FILES
  prism/macaulay2.js
  pygments/macaulay2.py
  vim/m2.vim.syntax
  vim/m2.vim.dict
  emacs/M2-symbols.el)
```

A custom CMake target runs `M2 --script make-M2-symbols.m2` and
rebuilds the grammar files when M2's symbol list changes.

## Used by

- The build system, via the `M2-editors` CMake target.
- Developers regenerating syntax files after adding a built-in.

## Related

- [`README.md`](README.md) — editors/ overview.
- [`../packages/Style/Style.m2`](../packages/) — provides
  `generateGrammar`.
- `prism/macaulay2.js.in`, `pygments/macaulay2.py.in`,
  `vim/m2.vim.syntax.in`, `emacs/M2-symbols.el.in` — input
  templates.
