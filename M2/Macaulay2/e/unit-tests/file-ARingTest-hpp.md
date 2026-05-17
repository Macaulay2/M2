# `ARingTest.hpp` — common fixture for `aring` tests

`ARingTest.hpp` is the **shared gtest infrastructure** for every
`ARing*Test.cpp` file — generator templates, comparison helpers,
and the `ntrials` knob that controls how many random elements
each test exercises.

Part of the [engine unit-tests suite](README.md).

[← back to unit-tests overview](README.md) · [← engine overview](../README.md)

## What's declared

```cpp
#ifndef __ring_test_hpp__
#define __ring_test_hpp__

#include "interface/random.h"

const int ntrials = 1000;
// const int ntrials = 1000000; // not good for the ssd - system swaps
// memory....

template <typename RingType>
void getElement(const RingType& R,
                int index,
                typename RingType::ElementType& result);

template <typename RingType>
class ARingElementGenerator
{
 public:
  ARingElementGenerator(const RingType& R) : mRing(R), mNext(0) {}
  void nextElement(typename RingType::ElementType& result)
  {
    getElement<RingType>(mRing, ++mNext, result);
  }
  ...
};
```

Two things:

1. **`getElement<RingType>(R, index, result)`** — a *template
   primary declaration* with no body. Each `ARing*Test.cpp` provides
   the specialisation for its ring (e.g. `ARingZZTest.cpp` provides
   `getElement<M2::ARingZZ>`). This decouples the test fixtures
   from per-ring construction.
2. **`ARingElementGenerator`** — a stateful generator wrapping
   `getElement`. Tests pull one element at a time without caring
   how it's produced.

## Why the index-based pattern

```cpp
if (index < 50)
  R.set_from_long(result, index - 25);
else
  ... random ...
```

The first 50 elements are deterministic (`-25..24`), the rest are
random. This catches:

- **Edge cases** — zero, negative one, max int.
- **Random fuzzing** — the bulk of trials.

Without the deterministic prefix, the random sample might miss
`0` for many tests in a row.

## The `ntrials` knob

```cpp
const int ntrials = 1000;
```

Each ring's full test loop runs `ntrials` iterations. The commented
`1000000` line records the original developer's experiment: a
million trials swamped memory on the test machine. `1000` is the
balance: enough to catch most bugs, fast enough for CI.

## Used by

- Every `ARing*Test.cpp` in this directory.
- The fixtures in [`file-test-harness.md`](file-test-harness.md).

## Related

- [`README.md`](README.md) — unit-tests overview.
- [`file-RingTest-hpp.md`](file-RingTest-hpp.md) — sister fixture
  for legacy-ring tests.
- [`../file-aring.md`](../file-aring.md) — engine-side `aring`
  framework.
- [`../coefficient-rings.md`](../coefficient-rings.md) — area
  these tests cover.
