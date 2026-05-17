# `startup.c.cmake` — generated startup-content table

`startup.c.cmake` is the **CMake template** that produces
`startup.c` at build time. The output file contains compiled-in
**byte arrays** for `startup.m2` (the bootstrap M2 source) and
the test strings used by `--check`. They're embedded into the
binary so M2 can run without finding files on disk.

Part of the [`bin/` directory](README.md).

[← bin/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```c
/* Macaulay2/bin/startup.c is generated from Macaulay2/bin/startup.c.cmake. */

typedef struct {
  const char *filename, *contents;
} cached_file;

extern cached_file startupFile;
extern cached_file testStrings[];
extern int num_testStrings;

cached_file startupFile =
  {
   @STARTUP_M2_ADDR@,
   @STARTUP_M2_CONTENT@
  };

cached_file testStrings[] =
  {
@TEST_STRINGS@
  };

int num_testStrings = sizeof(testStrings)/sizeof(testStrings[0]);
```

The `@VAR@` placeholders are CMake substitution tokens. At build
time, `cmake/startup.cmake` reads `startup.m2`, escapes its
contents as a C string literal, and substitutes:

- **`@STARTUP_M2_ADDR@`** — the absolute path to the original
  `startup.m2` source (for debug messages).
- **`@STARTUP_M2_CONTENT@`** — the file's contents as a C string.
- **`@TEST_STRINGS@`** — all `--check` test strings, similarly
  embedded.

## Why embed `startup.m2`?

`startup.m2` is the M2-language file that runs **before any user
input**, defining built-in operators, loading `Core`, and setting
up the prompt. M2 must find it to start.

Three options:

1. **Find on disk** — requires `startup.m2` to be in a known path
   relative to the binary.
2. **Embed in binary** — `startup.c.cmake` approach.
3. **Bake into a separate data file** — extra file to ship.

Option 2 makes M2 a **single self-contained binary** for
distribution. The interpreter has both: it embeds `startup.m2`
*and* tries to find a newer version on disk so developers can
edit and reload.

## CMake-only

This template only works with the CMake build. The autotools
build uses a different mechanism (`Makefile.in` directly invokes
a script). Both produce equivalent `startup.c` files.

## Used by

- [`file-main.md`](file-main.md) — `main.cpp` references
  `startupFile` / `testStrings`.
- M2's `--check` test runner.

## Related

- [`README.md`](README.md) — bin/ overview.
- `M2/cmake/startup.cmake` — CMake module that processes this
  template.
- [`../m2/file-startup.md`](../m2/file-startup.md) — the M2-side
  `startup.m2` content.
- [`file-main.md`](file-main.md) — primary consumer.
