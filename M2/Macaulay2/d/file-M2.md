# `M2.d` — top-level interpreter module declarations

`M2.d` is the **top-level module declaration** for the M2
interpreter — the `.d`-language equivalent of an umbrella header
file. It declares the most fundamental types (`string`, `arrayint`,
…) that almost every other interpreter file uses.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```d
-- Copyright 2010 by Daniel R. Grayson

declarations "#include <M2/config.h>";
header "#include <interface/m2-types.h>";

use arithmetic;

export string := array(char);
export arrayint := array(int);
export arrayintOrNull := array(int) or null;
```

The file is short but consequential. It establishes:

- **`declarations "..."` / `header "..."`** — `scc1`-language
  directives that emit raw C/C++ `#include`s into the translated
  output. This is how `.d` files pull in C-side headers.
- **`use arithmetic;`** — import `arithmetic.d` (basic integer /
  pointer arithmetic primitives provided by the `.d` standard
  module).
- **Type exports** — `string` aliases `array(char)`, `arrayint`
  aliases `array(int)`. These two types alone are used in nearly
  every interpreter file.

## Why "M2.d"

The naming convention is unusual: most `.d` files are
named after the feature they declare (`lex.d`, `parse.d`, ...).
`M2.d` is the equivalent of an "M2.h" — the file that defines the
language's most basic vocabulary so other files can build on it.

## How the `.d` language works

The `.d` files are processed by `scc1`
([`../c/README.md`](../c/README.md)) which translates them into
plain C / C++. The `.d` language adds:

- Sum types and pattern matching (`when ... is ... do ...`).
- Garbage-collected pointers.
- Module-level `use` directives.
- `Ccode(t, …)` for inline C escape hatches.

The `declarations "..."` directive lets `.d` code emit literal C
preprocessor directives — used here to ensure
`<M2/config.h>` and `<interface/m2-types.h>` are visible after
translation.

## Used by

Every other `.d` / `.dd` file in `d/` ultimately transitively
imports `M2.d` (via `use` chains).

## Related

- [`README.md`](README.md) — d/ overview.
- [`../c/README.md`](../c/README.md) — `scc1` language spec.
- `arithmetic.d` — pulled in via `use`.
- [`../e/file-engine-h.md`](../e/file-engine-h.md) — engine public
  header that `interface/m2-types.h` is one piece of.
