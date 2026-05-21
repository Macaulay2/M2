# `dmat-qq-flint.hpp` — `DMat<ARingQQFlint>` FLINT specialisation

`dmat-qq-flint.hpp` is the **FLINT-backed specialisation** of
[`DMat<R>`](file-dmat.md) for `QQ` matrices. It stores the matrix as
a FLINT `fmpq_mat_t` and dispatches arithmetic to FLINT's rational
matrix routines.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include <assert.h>
#include <utility>             // for swap
#include "aring-qq-flint.hpp"  // for ARingQQFlint

#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/fmpq_mat.h>    // for fmpq_mat_t, fmpq_mat_entry, fmpq_mat_init, …
#pragma GCC diagnostic pop
```

Same preamble dance as every FLINT-backed file:
`<M2/gc-include.h>` first, then the FLINT header with warnings
suppressed.

## What's specialised

A full template specialisation `DMat<M2::ARingQQFlint>`:

- **Storage** — `fmpq_mat_t` (array of `fmpq_t` rationals).
- **Arithmetic** — `fmpq_mat_add`, `fmpq_mat_mul`, `fmpq_mat_neg`.
- **LU / rank** — `fmpq_mat_solve`, `fmpq_mat_rank`.
- **Inverse / det** — `fmpq_mat_inv`, `fmpq_mat_det`.

`fmpq` values inherit the small-value-inlined representation of
`fmpz` ([`file-aring-zz-flint.md`](file-aring-zz-flint.md)) for both
numerator and denominator, which is why FLINT's `fmpq_mat_*` is fast
for the typical "rationals with small num/denom" case.

## Companion `dmat-qq-interface-flint.hpp`

A sibling header `dmat-qq-interface-flint.hpp` provides additional
interface routines specifically for the FLINT QQ path — typically
conversion helpers between `fmpq_mat_t` and the engine's `Matrix`
type ([`file-matrix.md`](file-matrix.md)) and the legacy
[`file-aring-qq-gmp.md`](file-aring-qq-gmp.md) representation.

## Used by

- [`file-mutablemat.md`](file-mutablemat.md) — `QQ` mutable matrices
  default to this back end.
- [`file-mat-linalg.md`](file-mat-linalg.md) — dispatches to this for
  QQ matrices.
- M2-level `LUdecomposition`, `solve`, `inverse` over `QQ`.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-dmat.md`](file-dmat.md) — generic template.
- [`file-aring-qq-flint.md`](file-aring-qq-flint.md) — element type.
- [`file-dmat-zz-flint.md`](file-dmat-zz-flint.md), [`file-dmat-zzp-flint.md`](file-dmat-zzp-flint.md)
  — sibling FLINT specialisations.
- FLINT submodule under [`../../submodules/README.md`](../../submodules/README.md).
