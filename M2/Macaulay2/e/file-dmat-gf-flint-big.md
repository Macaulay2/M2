# `dmat-gf-flint-big.hpp` — dense matrix specialisation for big Galois fields

`dmat-gf-flint-big.hpp` is a **`DMat<M2::ARingGFFlintBig>`
specialisation** — uses FLINT's `fq_nmod_mat_t` directly instead
of the generic templated path. Faster for matrices over large
Galois fields.

Part of the [engine](README.md) — matrices.

[← engine overview](README.md) · [matrices](matrices.md)

## Header

```cpp
// Copyright 2014  Michael E. Stillman

#include <utility>                 // for swap
#include "aring-gf-flint-big.hpp"  // for ARingGFFlintBig

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/fq_nmod_mat.h>  // for fq_nmod_mat_t, fq_nmod_mat_entry, ...
#pragma GCC diagnostic pop
```

The `#pragma GCC diagnostic push/pop` block silences a benign
warning that FLINT's headers trigger on modern GCC. The pragma is
**file-local** — only this file's includes get the suppression,
not the rest of the engine.

## Why a specialisation

For a templated `DMat<R>`:

```
DMat<R>(rows, cols).rank()
   ↓ generic path
   uses R.set, R.add, R.mult inner loops
   ↓ ARingGFFlintBig
   each operation is itself a FLINT `fq_nmod_*` call
```

Generic path: one FLINT call per matrix entry per operation.
**Specialised path**: one FLINT `fq_nmod_mat_*` call for the
whole matrix.

The FLINT-native matrix ops use cache-aware blocked algorithms,
vectorised arithmetic, and shared scratchspace. Huge speedup for
matrices over `GF(p^n)` with large `n`.

## What's specialised

Typical methods that get FLINT-native specialisations:

- `DMat<ARingGFFlintBig>::rank()`.
- `DMat<ARingGFFlintBig>::determinant(result)`.
- `DMat<ARingGFFlintBig>::LU_decomposition(L, U, perm)`.
- `DMat<ARingGFFlintBig>::null_space(result)`.

Each calls FLINT's `fq_nmod_mat_*` instead of using the generic
templated implementation.

## Related specialisations

| File | Spec |
|---|---|
| `dmat-gf-flint-big.hpp` (this file) | Big GF |
| `dmat-gf-flint.hpp` (covered separately) | Medium GF |
| `dmat-lu-zzp-ffpack.hpp` | Z/p via FFPACK |
| `dmat-lu-zzp-flint.hpp` | Z/p via FLINT |
| `dmat-lu-qq.hpp` | QQ via FLINT |

The pattern is consistent: ring-specific dense matrix algorithms
that beat the generic template implementation.

## Used by

- Anything using `DMat<ARingGFFlintBig>` — typically matrix
  algebra over `GF(p^n)` with large `n`.
- The F4 GB engine's BIG-GF specialisation paths.

## Related

- [`README.md`](README.md) — engine overview.
- [`matrices.md`](matrices.md) — area.
- [`file-aring-gf-flint-big.md`](file-aring-gf-flint-big.md) —
  underlying ring.
- [`file-dmat-lu-variants.md`](file-dmat-lu-variants.md) — sister
  LU specialisations.
- FLINT — external linked library.
