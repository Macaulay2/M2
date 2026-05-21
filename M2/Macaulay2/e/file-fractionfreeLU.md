# `fractionfreeLU.{cpp,hpp}` — `FF_LUComputation` (Bareiss-style LU over a domain)

`fractionfreeLU.cpp` implements **fraction-free LU decomposition** of a
matrix over an integral domain. It uses the Bareiss algorithm to keep
all intermediate computations within the ring — no temporary fractions,
even when the base ring is not a field.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "mat.hpp"

class FF_LUComputation {
    // This is a class encapsulating the LU decomposition
    // over a domain, using fraction free Gaussian elimination.
    const Ring     *R;          // R should be a domain
    MutableMatrix  *M;
    int            *col_perm;
    bool           *need_div;
    int             pivot_col;
    ring_elem       lastpivot;
    ring_elem       pivot;
    // ...
};
```

Key invariant in the header comment: **`R` should be a domain**. The
class doesn't verify this — passing a non-domain produces silent garbage.

## Why fraction-free LU

A standard LU over `ZZ` produces fractions in the intermediate
matrices. Over the polynomial ring `ZZ[x_1, …, x_n]`, the fractions
would have polynomial denominators — computing them would force a
detour through the fraction-field machinery.

**Bareiss's algorithm** sidesteps this: at each elimination step, the
formula

```
M[i, j] := (M[i, j] · M[k, k] - M[i, k] · M[k, j]) / lastpivot
```

is guaranteed to produce an exact integer (resp. polynomial)
quotient, with no remainder, provided `R` is a domain. The
`lastpivot` is the **previous** pivot; the algorithm tracks it via
`lastpivot` and uses it as the denominator in the next step.

The result: every intermediate value lives in `R`. No `frac(R)`
needed. For polynomial-coefficient matrices, this can be orders of
magnitude faster.

## State elaborated

- **`R`** — the domain.
- **`M`** — the in-place matrix being decomposed.
- **`col_perm`** — column permutation tracking pivots.
- **`need_div`** — per-step flag: does this step actually need a
  division by `lastpivot`? (The first step does not.)
- **`pivot_col`, `pivot`, `lastpivot`** — pivot tracking.

## Output

After completion:

- `M` contains the LU factors in compact form (`L` strictly below the
  diagonal, `U` on and above).
- `col_perm` records the column permutation applied.
- The determinant is the final `pivot` (up to a sign from `col_perm`).

## Used by

- M2-level `LUdecomposition` over polynomial coefficient rings.
- Sub-routines of [`file-det.md`](file-det.md) when the
  `DET_BAREISS` strategy is selected.
- Resultant computations.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-det.md`](file-det.md) — Bareiss is one of three determinant
  strategies.
- [`file-mutablemat.md`](file-mutablemat.md) — `MutableMatrix*` input.
- [`file-gauss.md`](file-gauss.md), [`file-hermite.md`](file-hermite.md)
  — sibling elimination algorithms over different rings.
