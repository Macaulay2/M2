# `dmat-lu*.hpp` — LU decomposition specialisations

The `dmat-lu*.hpp` family declares **specialised LU decomposition**
implementations for the engine's dense matrix type
[`DMat<R>`](file-dmat.md). LU is the workhorse of numerical linear
algebra; the specialisations route to back-end-specific code (FLINT,
FFLAS-FFPACK, BLAS) when available.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## File family

`dmat-lu.hpp` is the umbrella header that pulls in the rest:

```cpp
#include "dmat.hpp"
#include "mat-elem-ops.hpp"
#include "mat-util.hpp"

#include "dmat-lu-inplace.hpp"

template <class RingType>
class DMatLinAlg;

#include "dmat-lu-zzp-ffpack.hpp"
// ... more specialisations included here ...
```

The family:

| File | Specialisation |
|---|---|
| `dmat-lu.hpp` | Umbrella header; declares `DMatLinAlg<RingType>` |
| `dmat-LU.hpp` | Generic in-place LU |
| `dmat-LU-template.hpp` | Templated reference implementation |
| `dmat-lu-inplace.hpp` | In-place buffer reuse |
| `dmat-lu-qq.hpp` | LU over `QQ` |
| `dmat-lu-zzp-flint.hpp` | LU over `Z/p` via FLINT |
| `dmat-lu-zzp-ffpack.hpp` | LU over `Z/p` via FFLAS-FFPACK (BLAS-fast) |

## `DMatLinAlg<RingType>`

The class template `DMatLinAlg<RingType>` (forward-declared in the
umbrella header) is the **dispatch point**. For each `RingType` that
has a specialisation, `DMatLinAlg<RingType>` is overridden to call the
back-end-specific implementation; otherwise it falls through to the
generic templated version.

## Why so many files

Each LU back end is performance-critical and complex enough to deserve
its own file:

- **In-place** vs. **out-of-place** — caller can save memory by
  overwriting the input.
- **`QQ`** — handles fractions cleanly without coefficient blowup.
- **`Z/p` via FLINT** — fast for medium primes.
- **`Z/p` via FFLAS-FFPACK** — fastest for small primes that fit in
  BLAS-style dispatch.

Splitting them across files keeps each one focused and lets the
compiler ignore the others for typical builds (each .cpp only
instantiates a few specialisations).

## LU output convention

LU decomposes `A = P · L · U` where:

- `P` is a permutation matrix (returned as an `M2_arrayint` perm).
- `L` is lower-triangular with 1s on the diagonal.
- `U` is upper-triangular.

The engine convention is to return `L` and `U` packed into a single
`DMat<R>` (lower-triangle entries are `L`'s strictly-below-diagonal
entries; upper-triangle plus diagonal is `U`). The permutation is
returned separately. The rank of `A` is implicit in the result.

## Used by

- [`file-mutablemat.md`](file-mutablemat.md) — `LUdecomposition` op.
- [`file-mat-linalg.md`](file-mat-linalg.md) — dispatches to the
  right back end.
- [`file-det.md`](file-det.md) — uses LU pivots to compute the
  determinant.
- [`file-LLL.md`](file-LLL.md) — uses LU as a step.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-dmat.md`](file-dmat.md) — generic template.
- [`file-mat-linalg.md`](file-mat-linalg.md) — algorithm dispatch.
- [`file-fractionfreeLU.md`](file-fractionfreeLU.md) — Bareiss
  alternative for integer / polynomial domains.
- [`file-lapack.md`](file-lapack.md) — LAPACK LU for `RR`/`CC`.
