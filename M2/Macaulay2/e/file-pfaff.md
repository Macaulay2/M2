# `pfaff.{cpp,hpp}` — `PfaffianComputation`

`pfaff.cpp` computes **Pfaffians** of skew-symmetric matrices. The
Pfaffian `Pf(A)` of a `2n × 2n` skew-symmetric matrix `A` satisfies
`Pf(A)² = det(A)`. Unlike the determinant, the Pfaffian is a polynomial
in the matrix entries with **half** the degree, so for skew-symmetric
inputs it's both more natural and computationally cheaper.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "matrix.hpp"
#include "matrix-con.hpp"

class MatrixConstructor;

class PfaffianComputation : public our_new_delete {
    const Ring   *R;
    const Matrix *M;       // the skew-symmetric input
    // ... DP cache, output buffer, ...
};
```

Three pieces of state:

- The ambient ring `R`.
- The input matrix `M` — must be square and skew-symmetric;
  the algorithm doesn't check this.
- A DP cache of intermediate Pfaffians (analogous to
  [`file-det.md`](file-det.md)'s `DET_DYNAMIC` strategy).

## How Pfaffians are computed

The algorithm uses **Laplace-style expansion along a row**:

```
Pf(A) = Σ_j (-1)^{j+1} A[1, j] · Pf(A_{1, j})
```

where `A_{1, j}` is the `(2n-2) × (2n-2)` matrix obtained by deleting
rows / columns 1 and j. The recursion bottoms out at the `2×2` case
where `Pf([[0, a], [-a, 0]]) = a`.

The DP cache stores Pfaffians of submatrices keyed by their index set,
so repeated subproblems aren't recomputed.

## Why a separate algorithm from `det`

Computing `det(A)` and taking a square root works in principle, but:

- `det(A)` has degree `2n` in the entries; `Pf(A)` has degree `n`.
  Direct Pfaffian computation produces a smaller answer.
- Square roots are messy in `Z[x_{ij}]` (you need to track signs and
  factorisation).

`PfaffianComputation` returns the Pfaffian directly.

## Used by

- M2's `pfaffians(I, r)` function — `r`-th Pfaffian ideal of an
  ideal.
- Algebraic-geometry packages dealing with skew-symmetric resolutions
  (Hilbert-Burch, Buchsbaum-Eisenbud, etc.).

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-det.md`](file-det.md) — sibling determinant computation.
- [`file-matrix.md`](file-matrix.md) — input matrix type.
- `matrix-symm.{cpp,hpp}` ([`file-matrix-symm.md`](file-matrix-symm.md)
  when added) — sibling symmetric-power computation.
