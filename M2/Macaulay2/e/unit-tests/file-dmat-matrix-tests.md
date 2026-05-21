# `DMatTest.hpp`, `DMatZZpTest.cpp`, `MatrixIOTest.cpp`, `PolyRingTest.cpp` — dense matrix and polynomial-ring tests

These files test the **dense matrix templates** (`DMat<R>`), the
**matrix I/O machinery**, and the **polynomial-ring constructors**.

Part of the [engine unit-tests suite](README.md).

[← back to unit-tests overview](README.md) · [← engine overview](../README.md)

## `DMatTest.hpp`

```cpp
#ifndef __dmat_test_hpp__
#define __dmat_test_hpp__

#include "dmat.hpp"

#endif
```

A trivial header that brings in `dmat.hpp`. It exists as a
**convention placeholder** — all matrix tests `#include
"DMatTest.hpp"` so future shared fixtures can be added here.

## `DMatZZpTest.cpp`

```cpp
TEST(DMatZZp, create)
{
  typedef M2::ARingZZp RingZZp;
  typedef DMat<M2::ARingZZp> MatZZp;

  RingZZp* R = new RingZZp(101);
  MatZZp M(*R, 5, 5);

  EXPECT_TRUE(&M.ring() == R);
  ...
}
```

Tests dense matrices over `Z/p`:

- **Construction** — `DMat<R>(ring, rows, cols)`.
- **Element access** — `M(i, j)`, `set_entry(i, j, val)`.
- **Basic ops** — `mult`, `add`, `transpose`.
- **Reduction** — row reduce, rank.

`DMat<ARingZZp>` is the workhorse for fast linear algebra over
finite fields — this test ensures it works on the simplest
non-trivial example before stressing it via the F4 GB engine.

## `MatrixIOTest.cpp`

```cpp
#include "BasicPolyListParser.hpp"
#include "gb-f4/PolynomialList.hpp"
#include "gb-f4/GBF4Interface.hpp"

#define EXAMPLE_DIR "/Users/mike/src/git-from-others/msolve/MES-examples/"
```

Tests the matrix / polynomial-list I/O machinery, including
reading benchmark files from msolve. The hardcoded path is a
**developer-machine convention** — the tests are disabled by
default and only run when the developer explicitly enables them
(too slow / large for CI).

The file comments:

```cpp
// These are more benchmark examples, and the files to be read are quite large
// So we can't run these by default.
```

## `PolyRingTest.cpp`

```cpp
// In BUILD tree in Macaulay2/e:
// ./M2-unit-tests --gtest_filter="*F4*"
```

Tests polynomial-ring construction:

- `MonomialOrdering` construction.
- `Monoid::create` with weight vectors.
- `PolynomialRing::create` (and `BasicPolyList` variants).
- Round-trips: build a ring, multiply two monomials, expect the
  result.

The `--gtest_filter="*F4*"` hint at the top tells developers how
to run just these tests when iterating.

## Used by

- Engine developers iterating on matrix or polynomial-ring code.
- The F4 GB engine — relies on these primitives working.
- CI on every PR.

## Related

- [`README.md`](README.md) — unit-tests overview.
- [`file-NewF4Test.md`](file-NewF4Test.md) — bigger F4 test
  exercising the polynomial-ring path.
- [`../matrices.md`](../matrices.md) — area `DMat` is part of.
- [`../polynomial-rings.md`](../polynomial-rings.md) — polynomial
  ring area.
