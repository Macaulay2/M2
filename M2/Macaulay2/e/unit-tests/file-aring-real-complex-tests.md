# `ARingRRTest.cpp`, `ARingRRRTest.cpp`, `ARingRRiTest.cpp`, `ARingCCTest.cpp`, `ARingCCCTest.cpp` — real / complex / interval ring tests

These five files test the **floating-point ring family**:

| File | Ring | Backend |
|---|---|---|
| `ARingRRTest.cpp` | `ARingRR` | C `double` (53-bit) |
| `ARingRRRTest.cpp` | `ARingRRR` | MPFR (arbitrary precision) |
| `ARingRRiTest.cpp` | `ARingRRi` | MPFI (intervals) |
| `ARingCCTest.cpp` | `ARingCC` | `complex<double>` |
| `ARingCCCTest.cpp` | `ARingCCC` | MPC (arbitrary precision complex) |

Part of the [engine unit-tests suite](README.md).

[← back to unit-tests overview](README.md) · [← engine overview](../README.md)

## The `almostEqual` problem

Floating-point tests can't use exact equality — every test file
defines its own `almostEqual`:

```cpp
bool almostEqual(const M2::ARingRR& R,
                 unsigned long nbits,
                 const M2::ARingRR::ElementType& a,
                 const M2::ARingRR::ElementType& b)
{
  M2::ARingRR::ElementType epsilon = pow(2, static_cast<double>(-nbits));
  M2::ARingRR::ElementType c;
  R.subtract(c, a, b);
  R.abs(c, c);
  return c < epsilon;
}
```

Same shape across all five files, scaled to each backend's
representation:

- **`ARingRR`** — `pow(2, -nbits)` directly.
- **`ARingRRR`** — `mpfr_set_ui_2exp` for arbitrary-precision
  epsilon.
- **`ARingRRi`** — interval containment instead of subtraction.
- **`ARingCC`/`ARingCCC`** — separate epsilon checks on real and
  imaginary parts.

The `nbits` parameter lets each individual test tune tolerance —
tests of addition need ~50 bits, tests of `sqrt(2)^2 == 2` only
~40, tests of long sequences of operations might allow fewer.

## What gets tested

Per-file, the same operations the integer rings test, plus:

- **Inverse** of non-zero values.
- **Comparison** when values are close.
- **Conversion** to/from `double` and to/from MPFR.
- **Special values** — zero, one, NaN, ±infinity (where
  representable).

## Interval-specific tests (`ARingRRi`)

```cpp
#include "aring-RRi.hpp"
```

For intervals, additional tests:

- **Containment** — `(a + b)` contains `a' + b'` for any
  `a' ∈ a, b' ∈ b`.
- **Width** — wide operations widen the result.
- **Empty intervals** — operations preserve emptiness.

## Cross-backend agreement

The implicit-but-important contract: `ARingRR` (53 bits) and
`ARingRRR` (set to 53 bits) should agree on every operation
within `2^-50` tolerance. Same for `ARingCC` vs. `ARingCCC`. The
tests in each file check this convergence as you raise precision.

## Used by

- Engine developers verifying floating-point changes.
- CI on every PR.

## Related

- [`README.md`](README.md) — unit-tests overview.
- [`file-ARingTest-hpp.md`](file-ARingTest-hpp.md) — fixture.
- [`../file-aring-RR.md`](../file-aring-RR.md),
  [`../file-aring-RRR.md`](../file-aring-RRR.md),
  [`../file-aring-RRi.md`](../file-aring-RRi.md),
  [`../file-aring-CC.md`](../file-aring-CC.md),
  [`../file-aring-CCC.md`](../file-aring-CCC.md) — rings under
  test.
- [`../coefficient-rings.md`](../coefficient-rings.md) — area.
- MPFR, MPFI, MPC — external linked libraries.
