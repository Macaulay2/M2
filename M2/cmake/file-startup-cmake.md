# `startup.cmake` — `startup.c` materialisation

`startup.cmake` materialises **`startup.c`** from
[`Macaulay2/bin/startup.c.cmake`](../Macaulay2/bin/file-startup.md)
— embedding `startup.m2` and the `--check` test strings as
properly-escaped C string literals.

Part of [`cmake/`](README.md).

[← cmake/ overview](README.md) · [← top-level repo TOC](../../README.md)

## Header

```cmake
###############################################################################
## This script is called from Macaulay2/d/CMakeLists.txt and is responsible for
## setting the Layout strings and strings for startup.c. Note that the system's
## layout structure is detected in cmake/configure.cmake and startup.c will be
## configured in Macaulay2/d/CMakeLists.txt

###############################################################################
## regex macro for C-style character escaping
## escapes / and "
## converts each line to "CONTENT" if with_newline is NO or "CONTENT\n" if it is YES
MACRO (_STARTUP_REGEX input retval with_newline)
  STRING (STRIP "${input}" _output)
  string(      REPLACE "\\" "\\\\"    _output "${_output}") # sed -e 's/\\/\\\\/g'
  string(REGEX REPLACE "\"" "\\\\\""  _output "${_output}") # set -e 's/"/\\"/g'
  # Note: we could use s/(.*)\n/.../g but for now string(REGEX REPLACE) behave differently than `sed -e`
```

The `_STARTUP_REGEX` macro is the **C-escape-string converter**.
Given raw M2 source like:

```
needsPackage "Foo"
```

it produces a properly-escaped C string literal:

```
"needsPackage \"Foo\"\n"
```

The escaping order is important:

1. **Backslashes first** — `\` → `\\`. Must come first so further
   substitutions don't accidentally double-escape.
2. **Double quotes** — `"` → `\"`.
3. **Newlines** — handled per-line if `with_newline=YES`.

The comment "for now string(REGEX REPLACE) behave differently
than sed -e" captures a CMake quirk: CMake's regex doesn't
handle multi-line patterns the same way sed does. The macro
works around this by processing line-by-line.

## What `startup.cmake` produces

```
M2 source files: startup.m2, test1.m2, test2.m2, ...
        ↓
cmake/startup.cmake reads each, escapes, emits:
        ↓
@STARTUP_M2_CONTENT@ → "needsPackage \"Foo\"\n..."
@TEST_STRINGS@      → array initializer
        ↓
Macaulay2/bin/startup.c.cmake (template)
        ↓ configure_file()
Macaulay2/bin/startup.c (compiled into M2)
```

After this, `Macaulay2/bin/startup.c` contains:

- A C string for the entire `startup.m2`.
- An array of C strings for every test in `--check`.

These get linked into the M2 binary and used at startup.

## Why escape in CMake, not C?

Alternative: ship `startup.m2` as a separate data file at install
time. But then M2 needs to **find it** at runtime, which is
fragile across install layouts.

Embedding it at compile time:

- Makes M2 self-contained (`M2-binary` includes `startup.m2`).
- Avoids path-finding bugs.
- Costs ~200KB of binary size — fine trade-off.

## Used by

- `Macaulay2/d/CMakeLists.txt` — calls into `startup.cmake` to
  produce the embedded strings.

## Related

- [`README.md`](README.md) — cmake/ overview.
- [`../Macaulay2/bin/file-startup.md`](../Macaulay2/bin/file-startup.md)
  — the template materialised here.
- [`../Macaulay2/m2/file-startup.md`](../Macaulay2/m2/file-startup.md)
  — the M2-side `startup.m2`.
