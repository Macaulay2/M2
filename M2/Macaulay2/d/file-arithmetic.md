# `arithmetic.d` — integer type declarations

`arithmetic.d` declares the **integer type aliases** used
throughout the `.d` codebase — the `.d`-side equivalent of
`<stdint.h>`.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's in the file

```d
--		Copyright 1994-2006,2010 by Daniel R. Grayson

declarations "
#ifndef _GNU_SOURCE
 #define _GNU_SOURCE
#endif
#include <stdint.h>
#define hash_t uint64_t";

export nothing := void();
export size_t := integerType "size_t";
export uchar := integerType "unsigned char";
export short := integerType "short";
export ushort := integerType "unsigned short";
export uint := integerType "unsigned int";
export long := integerType "long";
export ulong := integerType "unsigned long";
export longlong := integerType "long long";
export ulonglong := integerType "unsigned long long";
export int8_t := integerType "int8_t";
```

The `integerType "..."` syntax is a `.d` primitive that creates a
strongly-typed alias for a C integer type. After this file runs,
all the standard widths are available in `.d` code:

| `.d` name | C type | Use |
|---|---|---|
| `short` / `ushort` | `short` / `unsigned short` | small counters |
| `int` / `uint` | `int` / `unsigned int` | default integer |
| `long` / `ulong` | `long` / `unsigned long` | wider arithmetic |
| `longlong` / `ulonglong` | `long long` | 64-bit guaranteed |
| `int8_t`...`int64_t` | from `<stdint.h>` | fixed-width |
| `size_t` | `size_t` | array indexing |
| `hash_t` | `uint64_t` | hash codes (always 64-bit) |

## Why `hash_t` is special

The `#define hash_t uint64_t` in the C declarations block fixes
hash codes at 64 bits across all platforms. Without this, hash
codes would be `size_t` (32 bits on 32-bit platforms) — leading
to:

- Smaller hash space, more collisions.
- Hash-code dependent serialised data being incompatible across
  architectures.

The 64-bit choice is portable and the value-cost trade-off is
fine for modern systems.

## Why no `bool`

`bool` is set up earlier (in [`file-tokens.md`](file-tokens.md) /
the scc1 prelude) because it has different mapping rules (it's a
sum type with `true`/`false` constructors, not an integer alias).

## Used by

- Essentially every `.d` file. `arithmetic.d` is the most-imported
  module after the scc1 prelude.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-basic.md`](file-basic.md) — sister file; provides hash
  primitives building on `hash_t`.
- [`../c/README.md`](../c/README.md) — scc1's `integerType` syntax
  documented here.
