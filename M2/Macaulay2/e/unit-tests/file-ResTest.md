# `ResTest.cpp` — Schreyer-resolution monoid tests

`ResTest.cpp` tests the **Schreyer-resolution monoid backend** —
specifically `ResMonoidDense`, the dense-exponent monoid used by
the modern `comp-res2` resolution engine.

Part of the [engine unit-tests suite](README.md).

[← back to unit-tests overview](README.md) · [← engine overview](../README.md)

## What's tested

```cpp
#include "schreyer-resolution/res-moninfo.hpp"

TEST(ResMonoidDense, create)
{
  ResMonoidDense M1(4,
                    std::vector<int>{1, 1, 1, 1},
                    std::vector<int>{},
                    MonomialOrderingType::GRevLex);
  ...
}
```

The constructor takes:

- **Number of variables**.
- **Weight vector** (for degree computation).
- **Heft vector** (for term-degree bounding).
- **Ordering type** (GRevLex, Lex, ...).

Tests verify:

- Construction with various weight/heft combos.
- Monomial multiplication via `mult`.
- Comparison under each ordering.
- Encoding / decoding to packed form.

## Why a dedicated monoid for resolutions

The general-purpose `Monoid` (in [`file-MonoidTest.md`](file-MonoidTest.md))
handles many cases — but a free-resolution computation needs a
**stream-friendly** monoid where:

- Exponents are dense `int` arrays (no Schreyer-overhead in the
  monoid itself).
- Comparison is fast inside tight inner loops.
- Component info is integrated into the monoid for free modules.

`ResMonoidDense` is that specialised monoid.

## Where the resolution engine lives

The actual resolution computation is in
[`../schreyer-resolution/`](../schreyer-resolution/README.md);
this test just verifies the monoid building block works. End-to-end
resolution tests live in the M2-level test suite, where the engine
boundary makes them harder to isolate.

## Used by

- Engine developers iterating on the modern resolution engine.
- CI on every PR.

## Related

- [`README.md`](README.md) — unit-tests overview.
- [`../schreyer-resolution/README.md`](../schreyer-resolution/README.md)
  — the resolution engine under test.
- [`file-MonoidTest.md`](file-MonoidTest.md) — general-purpose
  monoid tests.
- [`../resolutions.md`](../resolutions.md) — area.
