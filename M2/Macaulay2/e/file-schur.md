# `schur.{cpp,hpp}` — `SchurRing` (Schur function ring)

`schur.cpp` implements **`SchurRing`** — the engine's representation of
the ring of **Schur functions**, the basis of symmetric functions
indexed by partitions. It is the engine's home for symmetric-function
computations such as Littlewood-Richardson coefficients and Schur
expansion.

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## Class shape (paraphrased)

```cpp
#include <vector>
#include "poly.hpp"

const int SCHUR_MAX_WT = 100;
const int LARGE_NUMBER = 32000;

class tableau {
    friend class SchurRing;
    // ... Young tableau data ...
};

class SchurRing : public Ring {
    // ... element ↔ partition mapping, multiplication ...
};
```

`SCHUR_MAX_WT = 100` is the weight cap — the engine refuses to compute
Schur products that would exceed this. A larger cap is technically
possible but expensive in time and memory; `100` is the standard
distribution value.

`LARGE_NUMBER = 32000` is the sentinel used to mark "no tableau here"
slots in the Young-tableau scratch space.

## `tableau`

A semi-standard **Young tableau** — the combinatorial object underlying
the Littlewood-Richardson rule. The `SchurRing` uses `tableau` instances
as scratch space during the LR multiplication algorithm.

## Multiplication via Littlewood-Richardson

Schur-function multiplication `s_λ · s_μ = Σ_ν c_{λμ}^ν · s_ν` is
non-trivial; the coefficients `c_{λμ}^ν` are the Littlewood-Richardson
numbers. The engine computes them by enumerating **LR skew tableaux** —
semi-standard fillings of a skew shape `ν/λ` whose reverse-reading
word is a lattice word.

The implementation walks the enumeration recursively, accumulating
coefficients per output partition.

## Related operations

- **`schur(R, n)`** — construct the Schur ring with at most `n` rows
  per partition.
- **`SchurPolynomial`** — element type (a sum of partitions with
  integer coefficients).
- **Pieri rules** — special-case multiplications used as building
  blocks.

The user-facing M2 wrapper is in [`m2/schubert.m2`](../m2/README.md)
and adjacent files.

## Used by

- **Schubert calculus** — in algebraic geometry, intersections in a
  Grassmannian decompose into Schur products.
- **Representation theory** — characters of `GL_n` representations
  are Schur polynomials.
- **Combinatorics** — many enumerative identities involve Schur
  expansions.

## Companion files

- **`schurSn.{cpp,hpp}`** — Schur functions associated with the
  symmetric group `S_n`.
- **`schur2.{cpp,hpp}`** — variant Schur-ring implementations.
- **`schur-poly-heap.{cpp,hpp}`** — a heap data structure specialised
  to Schur polynomials for fast accumulation during LR enumeration.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-comb.md`](file-comb.md) — `Subsets` and combinatorial helpers
  used by Schur enumeration.
- [`m2/schubert.m2`](../m2/README.md), [`m2/fano.m2`](../m2/README.md)
  — M2-side consumers.
