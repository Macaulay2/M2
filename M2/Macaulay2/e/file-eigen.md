# `eigen.{cpp,hpp}` — eigenvalues, eigenvectors, SVD

`eigen.cpp` implements the engine's **eigenvalue / eigenvector / SVD**
routines for real and complex matrices. It is a specialisation of
LAPACK ([`file-lapack.md`](file-lapack.md)) plus a fallback path using
the [Eigen3](https://eigen.tuxfamily.org/) C++ template library.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Matrix-type aliases

```cpp
#include "dmat.hpp"
#include "aring-RR.hpp"
#include "aring-CC.hpp"
#include "aring-RRR.hpp"
#include "aring-CCC.hpp"

using LMatrixRR  = DMat<M2::ARingRR>;
using LMatrixCC  = DMat<M2::ARingCC>;
using LMatrixRRR = DMat<M2::ARingRRR>;
using LMatrixCCC = DMat<M2::ARingCCC>;

namespace EigenM2 {

  bool SVD(const LMatrixRR *A,
           LMatrixRR *Sigma,
           LMatrixRR *U,
           LMatrixRR *VT);

  bool SVD(const LMatrixCC *A,
           LMatrixRR *Sigma,
           // ...

}
```

The `EigenM2` namespace wraps Eigen3 calls in engine-facing signatures
that take/return `DMat<R>` matrices. The function names mirror LAPACK
conventions (`SVD`, `eigenvalues`, `eigenvectors`, `LU`) so the
engine's dispatch code can pick LAPACK or Eigen3 transparently.

## Why both LAPACK and Eigen3

LAPACK is faster for large matrices but doesn't support MPFR-precision
input directly. Eigen3 supports arbitrary numeric types via templates,
so it can compute eigenvalues / SVDs at MPFR or interval precision
(though slower than LAPACK).

The dispatch:

- `RR`, `CC` (hardware precision) → LAPACK
- `RRR`, `CCC` (MPFR precision) → Eigen3
- `RRi`, `CCi` (intervals) → currently unsupported here

## SVD signatures

The header shows multiple SVD overloads — one per matrix-type
combination. The signatures consistently take:

- A `const` source matrix `A`.
- Output `Sigma` (singular values), `U`, `VT` (left / right singular
  vectors).

The return `bool` indicates success / failure (LAPACK can fail
convergence; the engine reports this back to the user).

## Used by

- M2-side `SVD`, `eigenvalues`, `eigenvectors`, `LUdecomposition`
  built-ins.
- NAG path tracking ([`file-NAG.md`](file-NAG.md)) for Jacobian
  eigenvalue queries during continuation.
- Numerical packages that need conditioning estimates.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-lapack.md`](file-lapack.md) — hardware-precision counterpart.
- [`file-dmat.md`](file-dmat.md) — the `DMat<R>` template.
- [`file-aring-RRR.md`](file-aring-RRR.md), [`file-aring-CCC.md`](file-aring-CCC.md)
  — MPFR element types.
- LAPACK, Eigen3 — external libraries.
