# `sagbi.{cpp,hpp}` — `sagbi` (SAGBI / canonical-subalgebra basis helpers)

`sagbi.cpp` contains **helper routines for computing SAGBI bases**
(Subalgebra Analog to Gröbner Bases In). A SAGBI basis for a
subalgebra `A ⊂ R` is a generating set whose leading-term subalgebra
generates the leading-term subalgebra of `A`.

The header is explicit that the implementation is **not currently
functional**:

> @brief Helper routines for computing Sagbi bases. Not currently
> functional?

Part of the [Gröbner bases](groebner-bases.md) area.

[← per-area: groebner-bases](groebner-bases.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "matrix.hpp"
#include "comp-gb.hpp"

class sagbi {
public:
    static ring_elem subduct(int numslots,
                             const PolyRing *R,
                             ring_elem f,
                             /* ... */);
    // ...
};
```

The class is a **namespace-style holder** for static helpers — no
state. The key operation is `subduct(...)`.

## What "subduction" is

The SAGBI counterpart of GB-reduction is called **subduction**: given a
polynomial `f ∈ R` and a SAGBI candidate `{g_1, …, g_n}`, find a
polynomial `h` in the subalgebra such that `f - h` has strictly smaller
leading term (or is zero).

The subduction step is the algorithm's inner loop. SAGBI bases differ
from GBs because:

- The subalgebra need not be a polynomial ring → SAGBI bases need not
  be finite even when their GBs are.
- The "division" step needs combinatorial reasoning about which monomials
  arise as products of the candidate leading terms.

## Status

The "not currently functional?" comment dates back several revisions
of M2. Today there is a more capable SAGBI implementation in the
[`SubalgebraBases`](../packages/SubalgebraBases.m2) M2 package, which
uses the engine's GB infrastructure indirectly rather than the
in-engine `sagbi` helpers.

The engine-side helpers remain in case a future native SAGBI engine
gets revived. They are not on any common code path today.

## Related

- [`groebner-bases.md`](groebner-bases.md) — area overview.
- [`SubalgebraBases`](../packages/SubalgebraBases.m2) — the M2 package
  doing the current SAGBI work.
- [`file-comp-gb.md`](file-comp-gb.md) — `GBComputation` infrastructure
  the package leverages.
