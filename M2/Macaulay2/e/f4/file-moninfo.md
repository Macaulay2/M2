# `moninfo.{cpp,hpp}` — `MonomialInfo` (F4 monomial layout)

`MonomialInfo` is the **F4-internal monomial layout descriptor** — it
encapsulates everything F4 needs to know about a monoid's encoded
monomials: how many ints they occupy, where the degree slot lives, how
to compare two of them, how to multiply, how to test divisibility.

Part of the [`f4/`](README.md) subdirectory.

[← f4 overview](README.md) · [← engine overview](../README.md)

## What `MonomialInfo` carries

```cpp
#include "interface/m2-types.h"
#include "f4/ntuple-monomial.hpp"
#include "f4/varpower-monomial.hpp"
#include "interface/monomial-ordering.h"
#include "skew.hpp"  // for SkewMultiplication
```

The header pulls in:

- The two F4-internal monomial encodings
  ([`file-ntuple-monomial.md`](file-ntuple-monomial.md) for dense,
  [`file-varpower-monomial.md`](file-varpower-monomial.md) for sparse).
- The user-facing `MonomialOrdering` enum
  ([`../interface/file-monomial-ordering-interface.md`](../interface/file-monomial-ordering-interface.md)).
- The skew-multiplication descriptor for exterior-style rings.

Putting these together, `MonomialInfo` is the F4-side compiled form of
a [`Monoid`](../file-monoid.md). The engine constructs one
`MonomialInfo` per `PolynomialRing` at F4 startup; it's shared across
all F4 operations on that ring.

## Compared to the engine's main `Monoid`

[`Monoid`](../file-monoid.md) is the general-purpose monoid class for
the whole engine — it supports arbitrary monomial orderings, multi-degree
gradings, skew/Weyl/solvable multiplication, etc.

`MonomialInfo` is **F4's specialised cousin**: it strips out everything
F4 doesn't need (multi-degrees beyond the simple one, generic ordering
walkers) and adds F4-specific accelerators (packed monomial layout,
preferred encoding choice). The two coexist; F4 builds a `MonomialInfo`
from a `Monoid` at construction.

## Operations exposed

- `monomial_size()` — the number of ints in one encoded monomial.
- `compare(a, b)` — encoded comparison under the chosen ordering.
- `multiply(a, b, out)`, `divide(a, b, out)` — encoded arithmetic.
- `is_divisible(a, b)` — divisibility test (cheap mask plus walk).
- `to_varpower(a, out)`, `from_varpower(in, out)` — convert between
  encodings.

The conditional `#if 0` block at the top of the header has stale
configuration code; the live implementation is below it.

## Used by

- [`file-f4-computation.md`](file-f4-computation.md) — constructs a
  `MonomialInfo` from the input ring.
- [`file-f4-spairs.md`](file-f4-spairs.md) — uses it for S-pair lcm.
- [`file-f4-m2-interface.md`](file-f4-m2-interface.md) — translation
  routines take a `MonomialInfo *` everywhere.
- [`file-monhashtable.md`](file-monhashtable.md) — the trait classes use
  `MonomialInfo` to define `hash_value` / `is_equal`.

## Related

- [`README.md`](README.md) — f4 overview.
- [`../file-monoid.md`](../file-monoid.md) — general-purpose `Monoid`.
- [`file-varpower-monomial.md`](file-varpower-monomial.md), [`file-ntuple-monomial.md`](file-ntuple-monomial.md)
  — supported encodings.
- [`../skew.hpp`](../polynomial-rings.md) (top-level) — `SkewMultiplication`
  descriptor.
