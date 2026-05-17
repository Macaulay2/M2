# `points.{cpp,hpp}` — `PointsComputation<CoeffRing>` (ideals of points)

`points.cpp` declares **`PointsComputation<CoeffRing>`** — a templated
routine for computing the ideal of a finite set of points, given as a
matrix of coordinates. The output is a generating set (typically a
Gröbner basis under graded reverse lex) plus the standard monomial
basis of the quotient.

The header is marked for removal once the branch it was developed on
is merged.

Part of the [Other computations](computations.md) area.

[← per-area: computations](computations.md) · [← engine overview](README.md)

## Header preamble

```cpp
#warning "Remove points.hpp, points.cpp once this branch is merged into the trunk"

#ifndef _points_hpp_
#define _points_hpp_

class Matrix;
class PolynomialRing;

template <typename CoeffRing>
class PointsComputation {
public:
    static Matrix *points(const PolynomialRing                 *R,
                          const typename CoeffRing::ring_type  *K,
                          const DMat<CoeffRing>                *Pts,
                          Matrix                              *&result_std_monoms);
};
```

The `#warning` is a leftover from a long-running branch. The file is
slated for removal but persists today; M2 packages that need
ideal-of-points functionality typically call into this via the
M2-level interface.

## What `points(R, K, Pts, std_monoms)` does

Inputs:

- **`R`** — the polynomial ring (`K[x_1, …, x_n]`).
- **`K`** — the coefficient ring.
- **`Pts`** — a `DMat<CoeffRing>` whose columns are the points
  (each column is a `K^n` vector).
- **`std_monoms`** — output: the standard monomials of the quotient
  `R/I(Pts)`.

Outputs:

- Return value: a `Matrix*` whose columns are generators of the
  ideal `I(Pts)`.
- `std_monoms`: a `Matrix*` whose columns are the standard monomials
  (a `K`-basis of `R/I(Pts)`).

## Algorithm

The standard "Buchberger-Möller" algorithm:

1. Order the candidate monomials by graded reverse lex.
2. Process monomials in order, asking whether each one is a linear
   combination of previously-seen monomials when evaluated at the
   points.
3. If yes — record a new generator for the ideal.
4. If no — add the monomial to the standard set.

The dependency check uses linear algebra over `K`.

## Used by

- NAG / `NumericalAlgebraicGeometry` package — when given a witness
  set, compute the variety's defining ideal.
- `Bertini` integration and similar.
- The M2 `pointsByIntersection` and friends.

## Related

- [`computations.md`](computations.md) — area overview.
- [`file-NAG.md`](file-NAG.md) — primary M2-side consumer.
- [`file-dmat.md`](file-dmat.md) — `DMat<CoeffRing>` storage.
- [`unit-tests/PointArray.cpp`](unit-tests/README.md) — adjacent
  point-related tests.
