# `RingZZTest.cpp`, `RingZZpTest.cpp`, `RingQQTest.cpp`, `RingRRRTest.cpp`, `RingCCCTest.cpp`, `RingTowerTest.cpp` — legacy `Ring` API tests

These files test the **legacy `Ring` interface** (the
`ring_elem`-based API that predates the templated `aring`
framework).

Part of the [engine unit-tests suite](README.md).

[← back to unit-tests overview](README.md) · [← engine overview](../README.md)

## Files at a glance

| File | Ring under test | Source |
|---|---|---|
| `RingZZTest.cpp` | `RingZZ` (integers via `globalZZ`) | `ZZ.hpp` |
| `RingZZpTest.cpp` | `Z_mod` (legacy modular) | `ZZp.hpp` |
| `RingQQTest.cpp` | `RingQQ` (legacy rationals) | `QQ.hpp` |
| `RingRRRTest.cpp` | `RingRRR` (MPFR via `ConcreteRing<ARingRRR>`) | `aring-glue.hpp` |
| `RingCCCTest.cpp` | `RingCCC` (MPC via `ConcreteRing<ARingCCC>`) | `aring-glue.hpp` |
| `RingTowerTest.cpp` | `Tower` (iterated extensions) | `tower.hpp` |

## Common pattern

Each file specialises `getElement<R>`:

```cpp
template <>
ring_elem getElement<RingZZ>(const RingZZ& R, int index)
{
  if (index < 50) return R.from_long(index - 25);
  ...
}
```

Returns a `ring_elem` — the legacy tagged-union type.

Then gtest cases verify:

- `R.add`, `R.subtract`, `R.mult`, `R.power`, `R.invert`.
- `R.is_zero`, `R.is_unit`, `R.is_equal`.
- Stream input/output via `fromStream` and `R.elem_text_out`.

## The `aring-glue` wrapper

For the modern floating-point rings, the legacy test uses
`ConcreteRing<ARingXXX>` (defined in `aring-glue.hpp`) which
wraps a templated `aring` ring in the legacy `Ring` interface:

```cpp
typedef M2::ConcreteRing<M2::ARingRRR> RingRRR;
```

This tests the **glue layer** — the bridge from the modern
templated rings to the legacy `ring_elem` interface that the
interpreter still uses. Glue bugs (incorrect `from_long`
mappings, double-frees during conversion) show up here.

## `RingTowerTest.cpp`

```cpp
#include "tower.hpp"
```

Tests `Tower` — the iterated polynomial-extension construction
that supports finite-field tower constructions like
`GF(2^256) = GF(2)[x]/(x^256 + ...)`.

The tower-input format is also unusual:

```cpp
// First: we need a routine to read a polynomial from a string.
// Format:  variables are a..zA..Z, and then [1], [2], ...
// Need both input and output routines for reading/writing polynomials in this
// format.
```

A custom parser sits in this file to make test cases readable.

## `RingZZpTest.cpp` — stream parsing

`RingZZpTest.cpp` includes an explicit `fromStream` test:

```cpp
TEST(RingZZmod32003, fromStream)
{
  std::istringstream i("+1234 +345 -235*a");
  Z_mod* R = Z_mod::create(32003);
  ring_elem a;
  while (fromStream(i, *R, a))
    {
      buffer o;
      ...
    }
}
```

`fromStream` is the engine's stream-based input reader, common
across many file-format paths (matrix I/O, polynomial input,
serialisation).

## Used by

- Engine developers verifying legacy-ring changes.
- CI on every PR.
- Indirectly: any change to `aring-glue.hpp` should be sanity-checked
  here.

## Related

- [`README.md`](README.md) — unit-tests overview.
- [`file-RingTest-hpp.md`](file-RingTest-hpp.md) — fixture.
- [`file-aring-zz-tests.md`](file-aring-zz-tests.md) — sibling
  `aring`-based tests.
- [`../coefficient-rings.md`](../coefficient-rings.md) — area.
