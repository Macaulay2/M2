# `M2/` and `valgrind/` — shared C/C++ headers

The two **subdirectories** of `M2/include/` hold headers shared
across the engine, interpreter, and binary:

- **`M2/`** — M2-specific shared headers (e.g., `gc-include.h`,
  `math-include.h`, `atomic-field.h`).
- **`valgrind/`** — bundled Valgrind suppression / macro headers.

Part of [`include/`](README.md).

[← include/ overview](README.md) · [← top-level repo TOC](../../README.md)

## `M2/` — shared headers

Files commonly include things like:

```cpp
#include <M2/gc-include.h>      // Boehm GC
#include <M2/math-include.h>    // GMP + MPFR + MPFI + FLINT
#include <M2/atomic-field.h>    // atomic-pointer typedef
#include <M2/config.h>          // configure-time constants
```

A few of the more important headers:

### `gc-include.h`

```c
#include <gc.h>
#include <gc/gc_cpp.h>
```

— a one-stop include for Boehm GC plus C++ helpers. Picks the
right `gc.h` path based on configure detection (`/usr/include/gc.h`
vs `/usr/local/include/gc/gc.h` vs vendored).

### `math-include.h`

Aggregates the math-library headers M2 builds on:

```c
#include <gmp.h>
#include <mpfr.h>
#include <mpfi.h>
#ifdef WITH_ARB
#include <flint/arb.h>
#endif
```

Why aggregate? Without this header, every source file would
juggle `#include`s and `#ifdef`s for each math lib. Centralising
keeps them consistent.

### `atomic-field.h`

The C-side `atomic_field` declaration the interpreter's
[`../Macaulay2/d/file-atomic.md`](../Macaulay2/d/file-atomic.md)
uses. A `struct atomic_field { volatile uint32_t field; }` with
`__sync_*` operations.

### `config.h`

Generated from
[`file-configuration-in.md`](file-configuration-in.md). Auto-generated;
don't edit.

## `valgrind/` — Valgrind integration

```
valgrind/
├── valgrind.h            (Valgrind's user-program macros)
├── memcheck.h            (memcheck-specific macros)
└── ...
```

These are **Valgrind's own public macros** — `VALGRIND_*` macros
like `VALGRIND_MAKE_MEM_NOACCESS`, `VALGRIND_CHECK_MEM_IS_DEFINED`
that allow M2's code to interact with Valgrind when running
under it.

M2 vendors these for two reasons:

1. **Optional dependency** — most users don't run Valgrind.
   Building should still work if Valgrind headers aren't
   installed.
2. **Version stability** — the macros are ABI-compatible across
   Valgrind versions, so a vendored copy is safe.

Used by `M2mem.c`, `M2mem.h` (in
[`../Macaulay2/d/file-c-glue.md`](../Macaulay2/d/file-c-glue.md))
for explicit memory-region declarations.

## Why headers live here, not in `Macaulay2/`

These headers cross **multiple Macaulay2 sub-trees**:

- `e/` (engine) uses them.
- `d/` (interpreter) uses them.
- `bin/` (final binary) uses them.

Placing them at `M2/include/` makes the include paths uniform:

```cmake
target_include_directories(<target> PRIVATE ${M2_SOURCE_DIR}/include)
```

— one path covers all of them. If they lived in `Macaulay2/e/`,
the interpreter and `bin/` would need engine-specific include
paths.

## Used by

- The engine (`Macaulay2/e/`).
- The interpreter (`Macaulay2/d/`).
- The final binary (`Macaulay2/bin/`).
- Test binaries (`Macaulay2/e/unit-tests/`).

## Related

- [`README.md`](README.md) — include/ overview.
- [`file-configuration-in.md`](file-configuration-in.md) —
  generated headers.
- [`../Macaulay2/d/file-atomic.md`](../Macaulay2/d/file-atomic.md)
  — interpreter consumer of `atomic-field.h`.
- [`../Macaulay2/d/file-c-glue.md`](../Macaulay2/d/file-c-glue.md)
  — uses Valgrind macros.
- Boehm GC, GMP, MPFR, MPFI, FLINT, Valgrind — vendored / detected
  libraries.
