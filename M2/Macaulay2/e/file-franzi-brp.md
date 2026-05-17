# `franzi-brp.{cpp,hpp}`, `franzi-brp-test.cpp`, `franzi-gb.cpp`, `franzi-interface.cpp` — Franziska Hinkelmann's boolean ring polynomials

These five files implement a **boolean ring polynomial (BRP) GB
engine** contributed by Franziska Hinkelmann. Used for fast
Gröbner-basis computation in `F_2[x_1,...,x_n] / (x_i^2 - x_i)`
— polynomial rings over `F_2` with squares vanishing (the
characteristic-2 boolean polynomial ring).

Part of the [engine](README.md) — Gröbner bases.

[← engine overview](README.md) · [Gröbner bases](groebner-bases.md)

## Header

```cpp
/* This code written by Franziska Hinkelmann is in the public domain */

#include <set>
#include <vector>
#include <iostream>
#include <list>
#include <map>
#include <string>

//////// CAREFUL ////////
// addition is not const! It changes this
//////// CAREFUL ////////

typedef unsigned long brMonomial;
```

The "**CAREFUL**" comment captures the file's most surprising
behaviour: `BRP::operator+=` and `BRP::operator+` *mutate*
their LHS argument (efficiency trade-off — avoids a copy).
Callers must know.

The `brMonomial` type — an `unsigned long` — packs an entire
boolean monomial into 64 bits, one bit per variable. This is the
**key trick**: any boolean monomial is a subset of variables,
and a 64-bit ulong directly encodes a 64-variable subset.

## The five files

| File | Role |
|---|---|
| `franzi-brp.hpp` | `BRP` class declaration |
| `franzi-brp.cpp` | `BRP` class implementation |
| `franzi-brp-test.cpp` | Standalone unit tests (built as a separate binary) |
| `franzi-gb.cpp` | GB algorithm using `BRP` |
| `franzi-interface.cpp` | M2 ↔ Franzi-engine bridge |

## Why a specialised boolean GB

Over `F_2[x_i] / (x_i^2 - x_i)`:

- Coefficients are 0/1 → addition is XOR.
- Squares vanish → exponents are 0/1 → monomials are subsets.
- All these compress beautifully: one `ulong` = up to 64-variable
  monomial; one `vector<ulong>` = one polynomial.

This makes BRP-GB **drastically faster** than the general F4
algorithm for this ring. Use cases:

- Cryptanalysis / SAT-style polynomial systems.
- Biological-network modelling (Hinkelmann's research domain).

## `franzi-interface.cpp`

```cpp
brMonomial exponentsToLong(int nvars, const_exponents exp)
{
  brMonomial result = 0;
  for (int i = 0; i < nvars; i++)
    if (exp[i] != 0)
      ...
}
```

Bridges from M2's general exponent-vector representation to the
packed `brMonomial`. Once translated, the BRP-GB engine runs;
results get translated back to M2's matrix format.

## Used by

- M2 user code targeting boolean polynomial rings (typically the
  `BooleanGB` package).
- Research / benchmarking workloads.

## Related

- [`README.md`](README.md) — engine overview.
- [`groebner-bases.md`](groebner-bases.md) — area.
- [`file-bibasis.md`](bibasis/file-bibasis.md) — sister
  involutive-basis engine.
- [`gb-f4/README.md`](gb-f4/README.md) — modern general F4 engine.
- [`../packages/BooleanGB`](../packages/) (if installed) —
  user package consuming this engine.
