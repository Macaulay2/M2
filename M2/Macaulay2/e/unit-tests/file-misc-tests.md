# `PointArray.cpp`, `SubsetTest.cpp`, `basics-test.cpp` — misc engine tests

Three tests covering smaller engine components:

- **`PointArray.cpp`** — Numerical AG's point-clustering data structure.
- **`SubsetTest.cpp`** — combinatorial subset encoding.
- **`basics-test.cpp`** — tiny "is the test harness alive?" test.

Part of the [engine unit-tests suite](README.md).

[← back to unit-tests overview](README.md) · [← engine overview](../README.md)

## `PointArray.cpp`

```cpp
#include "NAG.hpp"

TEST(PointArray, constructor)
{
  PointArray p(0.001, {0.3, 0.7});
  PointArray q(0.001, 10);
}
```

`PointArray` is the data structure NumericalAlgebraicGeometry uses
to **cluster numerical points** under approximate equality
(`epsilon = 0.001` in the test). It's a hash-bucket structure
where points within `epsilon` of each other land in the same
bucket, regardless of which order they arrive in.

Tests verify:

- **Constructor** — building with an explicit list or just a size
  hint.
- **Lookup** — `nearestPoint(x)` returns the right cluster.
- **Insertion** — clustering is order-independent.

## `SubsetTest.cpp`

```cpp
TEST(Subsets, encode1)
{
  Subsets C(5, 2);
  Subset a(2, 0);
  for (int i = 0; i < 10; i++)
    {
      C.decode(i, a);
      EXPECT_TRUE(C.isValid(a));
    }
}
```

`Subsets` encodes the C(n, k) k-subsets of `{0, ..., n-1}` as
integers and vice versa — used by:

- Free-resolution algorithms.
- Some Hilbert-function paths.
- Combinatorial enumeration in `comb.hpp`.

Tests verify encode/decode round-trips and that `decode(i)` gives
a valid subset for every `i ∈ [0, C(n,k))`.

## `basics-test.cpp`

```cpp
bool testfcn() { return true; }
TEST(Nothing, ideal)
{
  EXPECT_EQ(true, testfcn());
  EXPECT_FALSE(!(testfcn()));
}
```

A trivial test that does almost nothing. Its purpose: **verify
the test harness itself works**. If `basics-test` fails, the
gtest binary is broken (linking issue, missing library, ABI
mismatch); no point running the real tests.

CI sees `basics-test` fail and immediately reports "engine test
infrastructure broken" rather than "13,000 test failures
buried in unrelated noise."

## Used by

- Engine developers verifying these specific components.
- CI on every PR (basics-test runs first by alphabetic order
  bonus).

## Related

- [`README.md`](README.md) — unit-tests overview.
- [`../file-NAG.md`](../file-NAG.md) (if added) — Numerical AG
  source.
- [`../file-comb-hpp.md`](../file-comb-hpp.md) (if added) — subset
  combinatorics.
- [`../utilities.md`](../utilities.md) — broader utility area.
