# `ntl-interface.{cpp,hpp}` — bridge to the NTL number-theory library

`ntl-interface.{cpp,hpp}` is the **engine bridge** to Victor Shoup's
[NTL](https://libntl.org/) (Number Theory Library). NTL is linked into
M2 for selected operations — integer LLL, integer matrix LLL,
factoring, and primality — even though the engine's primary number-
theory back end is FLINT.

Part of the [Coefficient rings](coefficient-rings.md) area
(implementation utility).

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include <stddef.h>
#include <M2/math-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <NTL/ZZ.h>
#include <NTL/mat_ZZ.h>
#include <NTL/LLL.h>
#pragma GCC diagnostic pop
```

Three NTL headers pulled in:

- **`<NTL/ZZ.h>`** — integer type and basic arithmetic.
- **`<NTL/mat_ZZ.h>`** — integer matrices.
- **`<NTL/LLL.h>`** — LLL basis reduction.

The diagnostic pragmas suppress NTL's internal narrowing warnings.

## When NTL is used

The engine prefers FLINT for most number-theory work. NTL is the
chosen back end when:

- **LLL with stricter guarantees** — NTL's LLL implementation has
  options FLINT lacks (e.g. floating-point LLL with extra error
  checks). [`file-LLL.md`](file-LLL.md) uses NTL when one of those
  options is requested.
- **Legacy paths** — some engine code paths predate the FLINT
  migration and still call NTL directly.

The dispatch is per-operation, not per-build: both libraries are linked
in.

## Conversion helpers

`ntl-interface.cpp` provides conversion utilities between:

- NTL's `ZZ` ↔ GMP's `mpz_t`.
- NTL's `mat_ZZ` ↔ engine `Matrix` / `MutableMatrix`.

These conversions are not free — they involve copying limb data — but
they happen at the boundaries of operations, not inside hot loops.

## Cross-library coexistence

FLINT and NTL both define their own `ZZ` type. Inside this file,
`NTL::ZZ` is the NTL one; FLINT's `fmpz_t` is what the rest of the
engine uses. The bridge is responsible for keeping them straight.

## Companion files

- **`ntl-internal.{cpp,hpp}`** — additional NTL-side helpers not part
  of the public bridge.
- **`ntl-debugio.{cpp,hpp}`** — debug-time printing of NTL values
  through the engine's `buffer`/`text-io` machinery.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-LLL.md`](file-LLL.md) — primary consumer.
- [`file-aring-zz-flint.md`](file-aring-zz-flint.md) — FLINT-side
  default; the alternative to NTL.
- NTL — external linked library (not vendored as a submodule).
