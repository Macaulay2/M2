# `math-include.h` — aggregated big-number / math headers

A 31-line header that includes **the right combination of math
headers in the right order** for engine and interpreter code. The
ordering matters because GMP, MPFR, and MPFI have non-trivial
interactions, and `<math.h>` must appear before any of them.

## What it includes (in order)

```c
include <M2/config.h>     // autotools/CMake feature macros
include <math.h>          // std libm
include <stdint.h>        // when HAVE_STDINT_H (most platforms)
include <stddef.h>        // workaround for macOS cstddef before stddef.h
include <gmp.h>           // arbitrary-precision integers + rationals
include <mpfr.h>          // arbitrary-precision reals
include <mpfi.h>          // arbitrary-precision intervals
```

The `#define MPFR_USE_NO_MACRO` just before `<mpfr.h>` disables
MPFR's macros that expand to inline function calls — useful when
mixing C and C++ translation units (MPFR's macro forms can fail to
compile under C++).

## Why the macOS guard for `<stddef.h>`

The comment hints at it:

```c
/* this prevents a problem in Mac OS X, where 'cstddef' is loaded
   before 'stddef.h', and it causes a problem */
```

On older Apple toolchains, `<cstddef>` (from libc++) would be
brought in by some earlier C++ header and define `size_t`, then
`<stddef.h>` would later try to define it again with slightly
different attributes. Including `<stddef.h>` explicitly here makes
the conflict resolve in our favour. The bug is theoretically fixed
in modern Apple Clang but the workaround is harmless and stays.

## `__STDC_LIMIT_MACROS`

```c
#define __STDC_LIMIT_MACROS
```

Defined before `<stdint.h>` to expose the integer-limit macros
(`UINT64_MAX`, `INT32_MIN`, …) in C++ translation units. Without
this, C++ historically (pre-C++11) hid those macros.

## What's deliberately NOT in here

- **FLINT** — included case-by-case where needed; FLINT pulls in a
  lot and is only used by some engine areas (`aring-zz-flint.cpp`,
  `dmat-lu-zzp-flint.hpp`, …).
- **NTL** — same; NTL is C++ and has its own include conventions.
- **Boost** — same.
- **`<complex>`** / **`<complex.h>`** — kept out because C++ and C
  declare complex types incompatibly. Each translation unit picks
  the one it needs.

Keeping these out means including `math-include.h` doesn't pull in
the entire big-number library zoo into every file.

## `IWYU pragma`

`IWYU pragma: begin_exports` … `end_exports` brackets — see
[`file-gc-include-h.md`](file-gc-include-h.md) for the same pattern
applied to the GC headers.

## Consumers

Every engine file that does arbitrary-precision arithmetic:

- All `aring-zz-*.{cpp,hpp}` for ZZ backends
- All `aring-rr-*.{cpp,hpp}` for RR backends
- `mat-arith.hpp`, `dmat.hpp` for dense-matrix arithmetic
- `LLL.cpp`, `factor.cpp` — heavy number-theoretic algorithms

The interpreter side uses [`d/gmp1.d`](../../Macaulay2/d/file-gmp.md)
which `header "#include <M2/math-include.h>";`s.

## See also

- [`file-M2-headers.md`](../file-M2-headers.md) — overview of M2's public headers
- [`README.md`](README.md) — `include/M2/` overview
- [Repo `DEPENDENCIES.md`](../../../DEPENDENCIES.md) — full external-library catalogue
- [Repo `RING-ZOO.md`](../../../RING-ZOO.md) — which big-number library backs which ring type
- [`../../Macaulay2/d/file-gmp.md`](../../Macaulay2/d/file-gmp.md) — interpreter-side `mpz` / `mpq` / `mpfr` bindings
