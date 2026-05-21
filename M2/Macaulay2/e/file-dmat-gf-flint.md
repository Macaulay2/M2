# `dmat-gf-flint.hpp` and `dmat-gf-flint-big.hpp` — FLINT GF matrices

`dmat-gf-flint.hpp` and `dmat-gf-flint-big.hpp` are the
**FLINT-backed specialisations** of [`DMat<R>`](file-dmat.md) for
Galois-field matrices. The two correspond to FLINT's two GF
representations: Zech-log tables for small fields (`fq_zech`) and
polynomial-quotient form for large fields (`fq_nmod`).

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Two specialisations

### `dmat-gf-flint.hpp` — small GF

```cpp
#include <utility>
#include "aring-gf-flint.hpp"     // for ARingGFFlint

#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/fq_nmod_mat.h>    // for fq_zech_mat_entry, fq_zech_mat_clear
#include <flint/fq_zech_mat.h>    // for fq_zech_mat_t
```

Specialisation: `DMat<M2::ARingGFFlint>` over `fq_zech_mat_t`. Each
matrix entry is a Zech log index in `[0, q-1)`; arithmetic uses
`fq_zech_mat_*` routines that perform table lookups for multiplication
and Zech-log table additions.

For `q = p^k` up to ~32768, this is the fastest GF matrix path the
engine has — every arithmetic operation is O(1) integer math.

### `dmat-gf-flint-big.hpp` — large GF

Specialisation: `DMat<M2::ARingGFFlintBig>` over `fq_nmod_mat_t`.
Each entry is a polynomial of degree less than `k` over `Z/p`;
arithmetic uses `fq_nmod_mat_*` which performs polynomial multiplication
modulo the field's defining polynomial. Slower per operation but
supports arbitrary `q = p^k`.

The dispatch between the two is automatic — set at ring construction
time by [`file-aring-gf-flint.md`](file-aring-gf-flint.md) /
[`file-aring-gf-flint-big.md`](file-aring-gf-flint-big.md), and the
chosen aring type determines which specialisation is instantiated.

## Used by

- [`file-mutablemat.md`](file-mutablemat.md) — GF mutable matrices.
- F4 GB computations over GF.
- M2-level matrix algebra over `GF(q)`.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-dmat.md`](file-dmat.md) — generic template.
- [`file-aring-gf-flint.md`](file-aring-gf-flint.md), [`file-aring-gf-flint-big.md`](file-aring-gf-flint-big.md)
  — element types.
- [`file-dmat-zzp-flint.md`](file-dmat-zzp-flint.md) — Z/p sibling.
- FLINT submodule under [`../../submodules/README.md`](../../submodules/README.md).
