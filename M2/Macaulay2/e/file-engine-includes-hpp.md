# `engine-includes.hpp` — common include prelude

A 41-line "**umbrella header**" pulled in by every `.cpp` / `.hpp` in
the engine that needs the standard portability shims. Acts as a
**single point of truth** for which platform / config headers to
include and in what order — so individual translation units don't have
to repeat the dance.

Pulled in by ~30 of the larger engine headers — grep
`#include "engine-includes.hpp"` to see the consumers
(e.g. `style.hpp`, `monomial.hpp`, `dmat.hpp`, `ExponentList.hpp`,
`M2FreeAlgebra.hpp`, `gb-walk.hpp`, `mutablecomplex.cpp`, …).

## What it wraps

| Bringing in | Why |
|---|---|
| `M2/config.h` | Autotools/CMake configure-time `HAVE_*` macros |
| `interface/m2-types.h` | `M2_arrayint`, `M2_string`, `M2_ArrayString`, etc. — the cross-language types shared with the interpreter (unless `SAFEC_EXPORTS` is set, which means scc1 generation is producing the C side) |
| `<stdint.h>` or `<inttypes.h>` | Fixed-width integer types (`int32_t`, `uint64_t`, …) — picked based on `HAVE_STDINT_H`/`HAVE_INTTYPES_H` set by `configure`. Defines `__STDC_LIMIT_MACROS` first so the macros (`UINT64_MAX`, …) are exposed in C++ as well |
| `M2/gc-include.h` | Boehm GC headers — but **only when compiled as C**. In C++ contexts, `newdelete.hpp` handles GC integration via overloaded `operator new`. |

## Why it exists

Three reasons:

1. **Configure-driven include selection** — picking between `<stdint.h>`
   and `<inttypes.h>` based on autotools/CMake feature detection
   should happen in exactly one place.
2. **`SAFEC_EXPORTS` gating** — when scc1 is generating the C side of
   the engine ABI, it doesn't want the M2-flavoured type aliases from
   `m2-types.h`; this header knows the rule.
3. **GC-include policy** — C and C++ code integrate with Boehm GC
   differently; this header encodes the policy ("C code includes the
   Boehm headers here; C++ code defers to `newdelete.hpp`") so
   downstream files don't get it wrong.

## IWYU pragma

The file is bracketed by

```cpp
// IWYU pragma: begin_exports
…
// IWYU pragma: end_exports
```

so include-what-you-use treats the bundled headers as **re-exported** —
a file that includes `engine-includes.hpp` is considered to have
included the symbols defined by the inner headers, and IWYU won't
recommend pulling them in directly.

## When to add to this header

**Don't**, unless what you're adding is genuinely needed by ≥90% of the
engine. Per-area headers (`aring-includes.hpp`, … etc., when they
exist) are a better home for narrower commonality. The risk of
bloating this header is large: every `.cpp` that includes anything
ending up here ends up recompiling whenever you touch this file.

## See also

- [`file-engine-cpp.md`](file-engine-cpp.md) — engine-wide globals
- [`file-style.md`](file-style.md) — engine-wide compile-time tuning constants
- [`file-newdelete.md`](file-newdelete.md) — GC integration in C++
- [`interface/file-m2-types-interface.md`](interface/file-m2-types-interface.md) — cross-language types
- [`architecture.md`](architecture.md) — engine architecture
