# `NewF4Test.cpp` — newer F4 engine tests

`NewF4Test.cpp` exercises the **newer F4 Gröbner-basis engine**
(in [`../gb-f4/`](../gb-f4/README.md)) — primarily its
`MonomialHashTable` and `PolynomialList` infrastructure.

Part of the [engine unit-tests suite](README.md).

[← back to unit-tests overview](README.md) · [← engine overview](../README.md)

## Header

```cpp
// In BUILD tree in Macaulay2/e:
// ./M2-unit-tests --gtest_filter="*F4*"

#include "polyring.hpp"
#include "util-polyring-creation.hpp"
#include "gb-f4/MonomialHashTable.hpp"
#include "gb-f4/MonomialLookupTable.hpp"
#include "VectorArithmetic.hpp"
#include "BasicPolyListParser.hpp"
#include "gb-f4/PolynomialList.hpp"

TEST(NewF4, hashstats)
{
  newf4::HashTableStats stats;
  ...
```

The opening comment (`--gtest_filter="*F4*"`) shows the developer
invocation. The test naming convention `TEST(NewF4, ...)` makes
filtering trivial.

## What gets tested

| Component | Tests |
|---|---|
| `HashTableStats` | Statistics accumulator |
| `MonomialHashTable` | Insertion, lookup, resize |
| `MonomialLookupTable` | Divisibility-based lookup |
| `PolynomialList` | Construction from `BasicPolyList`, iteration |
| `VectorArithmetic` | Modular arithmetic primitives |

## Why test `MonomialHashTable` so heavily

In the F4 algorithm, every monomial that appears across all
polynomials gets a unique integer ID via a hash table. The hash
table is hit *millions* of times for big inputs — bugs here are
catastrophic (wrong IDs → wrong reductions → wrong GB).

The tests verify:

- Same monomial always maps to same ID.
- Different monomials get different IDs.
- Resize preserves ID consistency.
- Concurrent insertions (in parallel F4 variants) stay correct.

## `util-polyring-creation`

```cpp
#include "util-polyring-creation.hpp"
```

A small helper library (defined in
[`file-util-polyring-creation.md`](file-util-polyring-creation.md))
that builds polynomial rings from convenient string-based specs:

```cpp
const PolynomialRing* R = createPolyRing(...);
```

Without this, every test would repeat a 10-line setup; the helper
keeps each test focused on what it's verifying.

## Used by

- Engine developers working on F4.
- CI on every PR.
- The performance-tracking benchmark suite (some of these double
  as benchmarks).

## Related

- [`README.md`](README.md) — unit-tests overview.
- [`../gb-f4/README.md`](../gb-f4/README.md) — the F4 engine
  under test.
- [`../file-VectorArithmetic-hpp.md`](../file-VectorArithmetic-hpp.md)
  (if added) — vector-arithmetic primitives.
- [`file-util-polyring-creation.md`](file-util-polyring-creation.md)
  — test helper.
- [`../groebner-bases.md`](../groebner-bases.md) — area.
