# `GF.{cpp,hpp}` — legacy `GF` (`Ring`-based Galois field)

`GF.hpp` declares the **legacy `GF`** — the engine's original
Galois-field class before the [`aring`](file-aring.md) refactor. It
is still constructed by some M2 paths but new code prefers the
FLINT-backed [`file-aring-gf-flint.md`](file-aring-gf-flint.md) and
[`file-aring-gf-flint-big.md`](file-aring-gf-flint-big.md) or the
portable [`file-aring-m2-gf.md`](file-aring-m2-gf.md).

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "relem.hpp"

class GF : public Ring {
    // int P; // this is defined in class Ring
    const PolynomialRing *_originalR;     // ((Z/p)[t])/f(t)
    const RingElement *_primitive_element; // a generator of K^*
    // ... log / exp / Zech tables ...
};
```

A `GF` carries:

- A pointer to the **original presenting polynomial ring**
  `(Z/p)[t]/f(t)` — needed when M2 code asks "what's the defining
  polynomial?" or wants to do conversions.
- A **primitive element** of the multiplicative group, used to
  generate the log/exp tables.
- Pre-computed **log and exp tables** indexed by power of the
  primitive element.

The implementation matches [`file-aring-m2-gf.md`](file-aring-m2-gf.md)
in spirit — the same tabulated arithmetic. The difference is
plumbing: `GF` inherits from `Ring` directly with virtual dispatch,
while `ARingGFM2` uses the templated `SimpleARing` CRTP for inlining.

## When constructed

- The interpreter when the user creates a GF via `GF(q)` or
  `GF(p^k, Variable => …)` — depending on the path through
  `m2/galois.m2`, this may produce a `GF` or an aring variant.
- Code that needs the *defining-polynomial* introspection that the
  aring path doesn't expose.

## Coexistence

Three GF implementations in the engine today:

| Class | File | Backing |
|---|---|---|
| `GF` (legacy) | this file | Native M2 + log/exp tables |
| `ARingGFM2` | [`file-aring-m2-gf.md`](file-aring-m2-gf.md) | Native, aring-based |
| `ARingGFFlint` | [`file-aring-gf-flint.md`](file-aring-gf-flint.md) | FLINT Zech (small q) |
| `ARingGFFlintBig` | [`file-aring-gf-flint-big.md`](file-aring-gf-flint-big.md) | FLINT `fq_nmod` (large q) |

The user-visible distinction is small; performance and feature
trade-offs are listed in each individual deep-dive.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-m2-gf.md`](file-aring-m2-gf.md) — modern native
  equivalent.
- [`file-aring-gf-flint.md`](file-aring-gf-flint.md), [`file-aring-gf-flint-big.md`](file-aring-gf-flint-big.md)
  — FLINT-backed alternatives.
- [`file-ZZp.md`](file-ZZp.md), [`file-ZZ.md`](file-ZZ.md) — sibling
  legacy rings.
- [`m2/galois.m2`](../m2/README.md) — M2-side `GF` constructor.
