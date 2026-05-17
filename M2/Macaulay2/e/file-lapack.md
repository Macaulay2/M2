# `lapack.{cpp,hpp}` — LAPACK bridge for real / complex matrices

`lapack.cpp` is the engine's **bridge to LAPACK** — the numerical
linear-algebra library. It provides linear-equation solving,
factorisation, and eigenvalue routines for matrices over `RR`, `CC`,
`RRR`, and `CCC`.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Matrix-type aliases

```cpp
#include "aring-RR.hpp"
#include "aring-CC.hpp"
#include "aring-RRR.hpp"
#include "aring-CCC.hpp"
#include "dmat.hpp"

typedef DMat<M2::ARingRRR> DMatRRR;
typedef DMat<M2::ARingCCC> DMatCCC;
typedef DMat<M2::ARingRR>  DMatRR;
typedef DMat<M2::ARingCC>  DMatCC;
```

Four `DMat<R>` specialisations:

| Alias | Storage |
|---|---|
| `DMatRR`  | `DMat<ARingRR>` — `double` matrix |
| `DMatCC`  | `DMat<ARingCC>` — `std::complex<double>` matrix |
| `DMatRRR` | `DMat<ARingRRR>` — MPFR matrix |
| `DMatCCC` | `DMat<ARingCCC>` — MPFR-complex matrix |

The hardware-precision pair (`RR`, `CC`) is what LAPACK actually
operates on; the MPFR-precision pair gets converted via per-entry
casts as a fallback for higher-precision requests (with the
warning that this is no longer numerically certified).

## What it exposes

The header declares functions for:

- **Linear solve** — `Ax = b` for square `A`. The header comment notes
  the macOS-specific include path:

  ```text
  MES, On my mac, 10.12.4, lapack include file is at
    /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/Headers/clapack.h
  ```

- **LU decomposition** — with row pivoting.
- **QR decomposition** — Gram-Schmidt with column pivoting.
- **SVD** — singular value decomposition.
- **Eigenvalues** — see also [`file-eigen.md`](file-eigen.md).

Each operation accepts `DMatRR` / `DMatCC` arguments, dispatches to the
LAPACK driver (`dgesv`, `zgesv`, `dgesvd`, …), and writes results back
into output `DMat` slots.

## Linking

LAPACK is detected at configure time via
[`cmake/M2/m4/ax_lapack.m4`](../m4/README.md) (autotools) and
[`cmake/Find*.cmake`](../../cmake/README.md) (CMake). The engine links
against either system LAPACK, Apple Accelerate (on macOS), or OpenBLAS,
depending on what configure found.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-dmat.md`](file-dmat.md) — the `DMat<R>` template.
- [`file-eigen.md`](file-eigen.md) — eigenvalue specialisation.
- [`file-aring-RR.md`](file-aring-RR.md), [`file-aring-CC.md`](file-aring-CC.md)
  — element types.
- LAPACK / Accelerate / OpenBLAS — external linked libraries.
