# `RingTest.hpp` — common fixture for legacy-ring tests

`RingTest.hpp` is the **gtest infrastructure** for the legacy-ring
tests (`Ring*Test.cpp`) — the older `ring_elem`-based interface
that predates the templated `aring` framework.

Part of the [engine unit-tests suite](README.md).

[← back to unit-tests overview](README.md) · [← engine overview](../README.md)

## Header

```cpp
// Copyright 2013 Michael E. Stillman

#ifndef __ring_test_hpp__
#define __ring_test_hpp__

#include <cstdio>
#include <string>
#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "interface/random.h"
#include "ZZ.hpp"
#include "exceptions.hpp"

const int ntrials = 100;  // 5000

template <typename T>
std::istream& fromStream(std::istream& i,
                         const T& R,
                         typename T::ElementType& result);

template <typename T>
```

The shape mirrors [`file-ARingTest-hpp.md`](file-ARingTest-hpp.md)
but for the older API:

- **`getElement<RingType>(R, index)`** returns a `ring_elem` (not
  a typed `ElementType`).
- **`fromStream<T>(istream, R, result)`** — stream-parsing helper
  for input deserialisation.
- **`ntrials = 100`** (vs `1000` for `aring` tests) — these tests
  go through more machinery per call and are slower; `100` keeps
  CI snappy.

## Why "legacy"

The engine has two parallel ring interfaces:

- **Legacy `Ring`** — pre-2012; uses `ring_elem` (a tagged-pointer
  union); virtual dispatch.
- **Modern `aring`** — 2012+; uses templates and per-ring
  `ElementType`.

New rings target the modern interface, but the legacy interface is
still used by:

- Polynomial rings (most user-facing rings).
- The interpreter boundary (`ring_elem` is what crosses the
  C ABI to the M2 layer).

Both must stay correct — hence both test suites.

## Where each interface fits

```
ARing tests (templated)             Ring tests (legacy)
ARingZZTest.cpp                     RingZZTest.cpp
ARingZZpTest.cpp                    RingZZpTest.cpp
ARingQQFlintTest.cpp                RingQQTest.cpp
ARingQQGmpTest.cpp                  ...
ARingRRRTest.cpp                    RingRRRTest.cpp
ARingCCCTest.cpp                    RingCCCTest.cpp
```

The `Ring*Test.cpp` files often re-use the `getElement<RingZZ>`
specialization (since polynomial rings build on it).

## Used by

- Every `Ring*Test.cpp` (the legacy suite).
- [`file-test-harness.md`](file-test-harness.md) — provides
  specialisations.

## Related

- [`README.md`](README.md) — unit-tests overview.
- [`file-ARingTest-hpp.md`](file-ARingTest-hpp.md) — sister
  fixture for the templated `aring` tests.
- [`../file-ring.md`](../file-ring.md) — legacy `Ring` API.
- [`../coefficient-rings.md`](../coefficient-rings.md) — area
  these tests cover.
