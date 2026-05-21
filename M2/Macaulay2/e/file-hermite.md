# `hermite.{cpp,hpp}` — `HermiteComputation` (Hermite normal form over ZZ)

`hermite.cpp` implements **Hermite normal form** of an integer matrix —
the row-equivalent upper-triangular canonical form with positive
diagonal and entries above the diagonal smaller than the diagonal entry.
For ZZ matrices it plays the role that Gaussian elimination
([`file-gauss.md`](file-gauss.md)) plays over a field.

Part of the [Gröbner bases](groebner-bases.md) area (specifically, GB
specialised to ZZ coefficients).

[← per-area: groebner-bases](groebner-bases.md) · [← engine overview](README.md)

## State

```cpp
#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp-gb.hpp"
#include "ZZ.hpp"
#include <vector>

struct hm_elem : public our_new_delete {
    hm_elem *next;
    mpz_t    lead;
    vec      f;
    vec      fsyz;
};

class HermiteComputation : public GBComputation {
    // sorted linked list of hm_elem keyed by lead; ZZ pivot management
};
```

The `hm_elem` is parallel to `gauss.cpp`'s `gm_elem` but adds
`lead` — the leading coefficient of `f` as an `mpz_t`. Storing it
explicitly lets the algorithm sort and pivot without re-walking `f`
each time.

## Hermite normal form vs. Gaussian elimination

| Aspect | `GaussElimComputation` | `HermiteComputation` |
|---|---|---|
| Base ring | Field (mostly Q, Z/p) | ZZ |
| Pivot | Leading coefficient = 1 | Leading coefficient > 0, generator of an ideal |
| Above-diagonal entries | All zero | All in `[0, lead)` |
| Reduction | Divide and subtract | gcd / Bezout step + subtract |

Hermite form is the **right** canonical form over ZZ — Gauss-style
division wouldn't preserve integrality. Each pivot step uses an
extended GCD (Bezout) to produce the new pivot, then reduces below
the pivot to lie in the allowed range.

## When this is chosen

When the user requests a GB of a submodule of `ZZ^n` (or a free
module over a ring with ZZ-valued degrees), the dispatcher selects
`HermiteComputation` automatically. The result is **the** unique
Hermite normal form.

## Used in

- Linear-algebra-over-ZZ subroutines (smith normal form follows).
- Lattice computations.
- `Polyhedra` package's integer-lattice operations.

## Related

- [`groebner-bases.md`](groebner-bases.md) — area overview.
- [`file-gauss.md`](file-gauss.md) — field-coefficient analogue.
- [`file-LLL.md`](file-LLL.md) — lattice-reduction sibling (different
  algorithm with different optimality criterion).
- [`file-comp-gb.md`](file-comp-gb.md) — `GBComputation` base.
