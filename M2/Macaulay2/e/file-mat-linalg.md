# `mat-linalg.hpp` — templated linear algebra for `DMat<R>`

`mat-linalg.hpp` is the **templated linear-algebra layer** for the
engine's dense matrix type [`DMat<R>`](file-dmat.md). It declares
LU, rank, determinant, null-space, solve, inverse — all parameterised
on the coefficient ring `R`.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include "util.hpp"
#include "exceptions.hpp"
#include "dmat.hpp"

#include "aring-RR.hpp"
#include "aring-CC.hpp"
#include "aring-RRR.hpp"
#include "aring-CCC.hpp"
#include "aring-zzp.hpp"
#include "aring-m2-gf.hpp"
// ... more aring includes ...
```

The header pulls in every `aring-*` it needs to specialise. The
arithmetic operations themselves are written generically; the
specialisations route to FLINT / FFLAS-FFPACK / LAPACK back ends
where available.

## What's templated

A representative declaration shape:

```cpp
template <typename RT>
size_t LU(const DMat<RT> &A,
          DMat<RT>       &LU,
          M2_arrayint    &perm);

template <typename RT>
size_t rank(const DMat<RT> &A);

template <typename RT>
size_t nullSpace(const DMat<RT> &A,
                 DMat<RT>       &result);

// determinant, inverse, solve, ...
```

Every operation takes one or more `const DMat<RT> &` inputs, writes
into output `DMat<RT> &`s, and returns either a rank / status code or
nothing.

## Specialisations

The header is **generic** but the implementations specialise on `RT`
to dispatch to:

- **FLINT** for ZZ, QQ, Z/p, GF (via [`file-dmat.md`](file-dmat.md)'s
  back-end-specific `dmat-*-flint.hpp` files).
- **FFLAS-FFPACK** for Z/p (BLAS-routed).
- **LAPACK** for RR, CC ([`file-lapack.md`](file-lapack.md)).
- **Eigen3** for RRR, CCC ([`file-eigen.md`](file-eigen.md)).
- **Generic** (slow fallback) for everything else.

The specialisations live in companion headers; this file is the unifying
declaration.

## How it differs from `mat-arith.hpp`

[`mat-arith.hpp`](file-mat-arith.md) covers **arithmetic** — add,
multiply, scale, negate, etc. — for both [`DMat<R>`](file-dmat.md) and
[`smat.hpp`](matrices.md)'s sparse counterpart.

`mat-linalg.hpp` covers **linear algebra** — operations that reduce a
matrix to canonical form or solve a system. Only `DMat<R>` is in
scope; sparse matrices typically need their own algorithms.

## Used by

- [`file-mutablemat.md`](file-mutablemat.md) — every mutable-matrix
  linear-algebra method dispatches through here.
- [`file-LLL.md`](file-LLL.md) — uses these helpers internally.
- M2-level `rank`, `det`, `inverse`, `solve` built-ins.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-dmat.md`](file-dmat.md) — `DMat<R>` template.
- [`file-mat-arith.md`](file-mat-arith.md) — arithmetic sibling.
- [`file-lapack.md`](file-lapack.md), [`file-eigen.md`](file-eigen.md)
  — back-end-specific implementations.
- `mat-elem-ops.hpp`, `mat-util.hpp`, `mat-jordan.hpp` — neighbouring
  helpers.
