# `aring-m2-gf.{cpp,hpp}` — `M2::ARingGFM2` (native M2 Galois field)

`aring-m2-gf.cpp` implements **Galois fields** entirely in native M2 /
engine code without depending on FLINT, Givaro, or any external GF
library. It is the **portable fallback** when those libraries are
unavailable, and the reference path other GF implementations compare
against.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "polyring.hpp"

class GF;

namespace M2 {

typedef int GFElement;

/// ingroup rings
class ARingGFM2 : public SimpleARing<ARingGFM2> {
public:
    static const RingID ringID = ring_GFM2;
    typedef GFElement elem;
    // ... arithmetic, tables, conversion ...
};

}
```

`GFElement` is a plain `int` — interpreted as the index of `α^n` in a
log table where `α` is a primitive root, with `0` representing the
field's `0` element. Same convention as
[`file-aring-zzp.md`](file-aring-zzp.md), generalised to extension
fields.

## How extension is built

A `GF(p^k)` is constructed by:

1. Pick a base field `Z/p` (using legacy `Z_mod` or
   [`file-aring-zzp.md`](file-aring-zzp.md)).
2. Pick a primitive polynomial `f(t)` of degree `k` over `Z/p`.
3. Build `GF(p^k) = (Z/p)[t]/f(t)` and find a primitive element `α` of
   the multiplicative group.
4. Precompute log / exp / Zech tables of size `p^k`.

For `p^k` up to ~32000 (the standard ceiling), the table-based approach
is fast. Above that limit the engine falls back to one of the FLINT-
backed paths.

## Why have a native path

Three reasons:

1. **No external dep** — minimal builds need a working GF.
2. **Reference** — when adding a new GF back end, comparing against this
   one validates correctness.
3. **Integration with PolyRing** — the header pulls in `polyring.hpp`,
   meaning `ARingGFM2` knows about the M2 polynomial-ring layer in a
   way the FLINT paths don't. This matters when the GF is built from a
   user-supplied primitive polynomial expressed in M2's natural form.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-gf-flint.md`](file-aring-gf-flint.md) — FLINT Zech
  alternative.
- `aring-gf-flint-big.{cpp,hpp}` — FLINT-based large extension.
- `GF.{cpp,hpp}` — older non-aring counterpart (similar encoding).
- [`file-aring-zzp.md`](file-aring-zzp.md) — base-field analogue.
