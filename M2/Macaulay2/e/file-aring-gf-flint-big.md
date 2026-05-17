# `aring-gf-flint-big.{cpp,hpp}` — `M2::ARingGFFlintBig` (large GF via FLINT `fq_nmod`)

`aring-gf-flint-big.cpp` implements **Galois fields with arbitrary
extension degree** using FLINT's `fq_nmod_t` value type. Where
[`file-aring-gf-flint.md`](file-aring-gf-flint.md) builds Zech logarithm
tables (fast but limited to small `q = p^k`), this file represents
elements as polynomials modulo a primitive polynomial — slower per
operation, but no size limit.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include <vector>

#include <M2/gc-include.h>
#include <M2/math-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fq_nmod.h>
#include <flint/nmod_poly.h>
#pragma GCC diagnostic pop

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "exceptions.hpp"  // for division_by_zero_error
```

The full preamble dance: `<M2/gc-include.h>` so FLINT's malloc routes
through bdwgc, then the FLINT headers with warnings suppressed.

## Representation

A `GF(p^k)` element is a degree-less-than-`k` polynomial over `Z/p`,
stored as FLINT's `fq_nmod_t` (which wraps an `nmod_poly_t`).
Arithmetic operations:

- **Addition** — coefficient-wise add in `Z/p`. O(k).
- **Multiplication** — polynomial multiply followed by reduction modulo
  the primitive polynomial. O(k^2) via schoolbook or O(k log k) via
  FFT for large k.
- **Inversion** — extended Euclidean over `Z/p[t]`. O(k^2).

No Zech tables — every operation runs the polynomial arithmetic. The
trade-off: per-operation cost grows with `k`, but `q = p^k` can be
arbitrarily large with no memory penalty.

## Class structure

```cpp
namespace M2 {

class ARingGFFlintBig : public SimpleARing<ARingGFFlintBig> {
public:
    static const RingID ringID = ring_GFFlintBig;
    // typedef fq_nmod_t elem;
    // ... arithmetic via fq_nmod_* ...
};

}
```

The CRTP base [`SimpleARing`](file-coeffrings.md) supplies the
boilerplate; this class overrides the ring-specific arithmetic with
calls into FLINT's `fq_nmod` API.

## When this beats `aring-gf-flint`

The Zech-tables path in
[`file-aring-gf-flint.md`](file-aring-gf-flint.md) is faster *per
operation* but requires `O(q)` storage. For `q > ~2^16` the Zech tables
become impractical, and `ARingGFFlintBig` becomes the default.

The choice is made automatically at construction by the dispatcher in
[`file-aring.md`](file-aring.md).

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-gf-flint.md`](file-aring-gf-flint.md) — small-GF sibling.
- [`file-aring-m2-gf.md`](file-aring-m2-gf.md) — non-FLINT alternative.
- [`file-aring-tower.md`](file-aring-tower.md) — iterated extensions.
- FLINT submodule under [`submodules/`](../../submodules/README.md).
