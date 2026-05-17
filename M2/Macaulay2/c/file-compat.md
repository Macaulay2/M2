# `compat.c`, `compat.h` — portability shims

`compat.c` and `compat.h` provide **portability adjustments** that
let `scc1` build identically on Linux, macOS, classic Mac (MPWC
back in the day), and Windows.

Part of the [`c/` scc1 translator](README.md).

[← back to c/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## `compat.c` — platform-specific strings

```c
#ifdef MPWC
char posfmt[] = "    File \"%s\"; Line %d # Column %d: Loaddepth %d: ";
char errfmt[] = "    File \"%s\"; Line %d # Column %d: %s";
char errfmtnc[] = "    File \"%s\"; Line %d # %s";
#else
char posfmt[] = "%s:%d:%d:(%d):";
```

The same error-message *format strings* in two flavours:

- **`MPWC`** — Macintosh Programmer's Workshop C, the classic-Mac
  build target. Used the verbose "File ...; Line ...; Column ..."
  format the MPW IDE understood.
- **Everything else** — the standard Unix `filename:line:col:`
  format that most editors and CI logs parse natively.

The MPWC code path is mostly historical (MPW hasn't shipped since
the early 2000s) but the macro guard keeps it harmless.

## `compat.h` — type / function shims

```c
#ifndef __cplusplus
#undef bool
typedef char bool;
#endif

extern char posfmt[];
```

A small `compat.h` that:

- Redefines `bool` as `char` if compiling as C (pre-C99 conventions
  — `<stdbool.h>` came after `scc1` was originally written).
- Declares the position format strings for `error.c` consumption.
- Provides macros for any missing-but-needed library functions.

## Why a separate compat module

Centralising platform variance in one file:

- Lets every other `.c` file ignore portability concerns.
- Makes it easy to grep for "what's different on platform X."
- Cuts maintenance time when adding new platform support.

## Modern relevance

Today most platforms are POSIX-shaped and `<stdbool.h>` is
ubiquitous, so `compat.{c,h}` is *almost* a no-op. It's kept
because:

- The MPWC branch costs nothing to leave in.
- New platforms (e.g. emscripten / WASM) might need shims.
- Removing it would require touching every translator file.

## Used by

- [`file-error.md`](file-error.md) — uses `posfmt`.
- [`file-scc-h.md`](file-scc-h.md) — `scc.h` includes `compat.h`.
- Anywhere `bool` is used in pre-C99 mode.

## Related

- [`README.md`](README.md) — c/ overview.
- [`file-error.md`](file-error.md) — primary consumer.
- [`file-scc-h.md`](file-scc-h.md) — pulls `compat.h` in.
