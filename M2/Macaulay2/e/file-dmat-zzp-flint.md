# `dmat-zzp-flint.hpp` — `DMat<ARingZZpFlint>` FLINT specialisation

`dmat-zzp-flint.hpp` is the **FLINT-backed specialisation** of
[`DMat<R>`](file-dmat.md) for `Z/p` (via FLINT's `nmod_mat_t`). It
mirrors [`file-dmat-zz-flint.md`](file-dmat-zz-flint.md) in shape,
specialised on the modular-arithmetic FLINT type.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include <utility>              // for swap
#include "aring-zzp-flint.hpp"  // for ARingZZpFlint

#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/nmod_mat.h>     // for nmod_mat_t, nmod_mat_clear, nmod_mat_init, nmod_m...
#pragma GCC diagnostic pop
```

`<flint/nmod_mat.h>` is the FLINT header for matrices over Z/p (where
the modulus is `n`, not necessarily prime, but the engine restricts
to primes). The `nmod_mat_t` stores entries as machine words in
`[0, p)`, with a precomputed reciprocal for fast modular reduction.

## What's specialised

A full template specialisation `DMat<M2::ARingZZpFlint>`:

- **Storage** — `nmod_mat_t`, which is an array of `mp_limb_t` plus
  metadata about `p` and its reciprocal.
- **Arithmetic** — `nmod_mat_add`, `nmod_mat_mul` (uses Barrett or
  Newton-style fast modular reduction), `nmod_mat_neg`.
- **LU / rank** — `nmod_mat_lu`, `nmod_mat_rank`. For primes
  fitting in 32 bits, these are extremely fast.
- **Inverse / nullspace** — `nmod_mat_inv`, `nmod_mat_nullspace`.

## When FLINT wins over FFLAS-FFPACK

For Z/p matrices the engine has two back ends:

- **`dmat-zzp-flint.hpp`** (this file) — FLINT-backed.
- **`dmat-zzp-ffpack.hpp`** — FFLAS-FFPACK-backed.

Both are linked in. The dispatcher picks based on:

- **Tiny matrices, large primes** — FLINT wins (no BLAS overhead).
- **Large matrices, small primes** — FFLAS wins (can use BLAS).
- **Matrix shape (square vs. rectangular)** — sometimes one path
  has a specialised routine the other lacks.

The dispatch is wired in [`file-mat-linalg.md`](file-mat-linalg.md).

## Used by

- [`file-mutablemat.md`](file-mutablemat.md) — Z/p mutable matrices.
- F4 GB code over Z/p, when FLINT is the chosen back end.
- M2-level matrix arithmetic over a finite field.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-dmat.md`](file-dmat.md) — generic template.
- [`file-dmat-zz-flint.md`](file-dmat-zz-flint.md) — ZZ counterpart.
- [`file-aring-zzp-flint.md`](file-aring-zzp-flint.md) — element type.
- `dmat-zzp-ffpack.hpp` ([`file-aring-zzp-ffpack.md`](file-aring-zzp-ffpack.md) covers
  the aring side) — sibling specialisation.
- FLINT submodule under [`../../submodules/README.md`](../../submodules/README.md).
