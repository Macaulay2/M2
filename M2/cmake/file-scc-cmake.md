# `scc.cmake` — `scc1` invocation macro

`scc.cmake` defines the **CMake macro that calls `scc1`** —
M2's `.d`/`.dd` → `.c`/`.cc` translator — on every interpreter
source file. Called from `Macaulay2/d/CMakeLists.txt`.

Part of [`cmake/`](README.md).

[← cmake/ overview](README.md) · [← top-level repo TOC](../../README.md)

## Header

```cmake
###############################################################################
## This script is called from Macaulay2/d/CMakeLists.txt and contains the macro
## for translating .d and .dd files to .c and .cc files, respectively.

# Tip: -noline remove the line macros to make reading the output easier
set(SCCFLAGS "-O" CACHE STRING "Flags for the Safe C Compiler")

# Generate a C or C++ source (.c or .cc) from the D source (.d or .dd, resp.)
#  _source:  D source filename; e.g. interp.dd
#  _prev:    see the notes below
#
# NOTE: also sets two variables:
#  ${_prev}_name.sig: the signature file of the generated source
#  ${_prev}_source:   filename of the generated source
```

The script defines a macro that takes:

- `_source` — input `.d` or `.dd` file (e.g. `interp.dd`).
- `_prev` — name of the previous module in the sequence.

For each call, the macro:

1. Determines if it's `.d` (→ `.c`) or `.dd` (→ `.cc`).
2. Creates a CMake custom command running `scc1`:

   ```
   scc1 -o output.c input.d --dependencies output.dep --signature output.sig
   ```

3. Records the generated `.c`/`.cc` and `.sig` files as outputs.

## Why `_prev` matters

`.d` modules can `use othermodule;`, which depends on
`othermodule`'s `.sig` file. The translator can't proceed until
the previous module has been translated (which produces its
`.sig`).

CMake doesn't natively understand this dependency chain — the
`_prev` parameter lets the macro chain output dependencies
manually:

```
foo.dd  → foo.sig
   ↓ depends on
bar.dd uses foo  → bar.sig
   ↓ depends on
baz.dd uses bar  → baz.sig
```

Without this chaining, parallel builds would race and produce
unpredictable results.

## `SCCFLAGS = "-O"`

The cache option controls translator behaviour. `-O` enables
some optimisations. Common alternatives:

- `-noline` — skip `#line` directives (cleaner output for
  manual review).
- `-noarraychk` — disable array bounds checks (release-only).

Set with `cmake -DSCCFLAGS="-O -noline" ...`.

## Used by

- [`../Macaulay2/d/CMakeLists.txt`](../Macaulay2/d/) — every
  source file passes through here.

## Related

- [`README.md`](README.md) — cmake/ overview.
- [`../Macaulay2/c/file-scc1.md`](../Macaulay2/c/file-scc1.md) —
  the translator being invoked.
- [`../Macaulay2/d/README.md`](../Macaulay2/d/README.md) — the
  files being translated.
