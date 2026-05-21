# `testMain.cpp`, `M2-cpp-replacement.cpp`, `fromStream.cpp`, `util-polyring-creation.{cpp,hpp}` — test harness and helpers

These files are the **scaffolding** that makes the gtest suite
runnable in isolation — `main()`, missing-symbol stubs, stream
helpers, and ring-construction shortcuts.

Part of the [engine unit-tests suite](README.md).

[← back to unit-tests overview](README.md) · [← engine overview](../README.md)

## `testMain.cpp`

```cpp
#include <gtest/gtest.h>
#include <M2/gc-include.h>
#include <engine.h>

int main(int argc, char **argv)
{
  IM2_initialize();
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
```

The `M2-unit-tests` binary's `main()`:

1. **`IM2_initialize()`** — boots the engine's static state
   (global rings, monoid prefab tables, etc.).
2. **`InitGoogleTest`** — parses gtest flags from `argv`.
3. **`RUN_ALL_TESTS()`** — gtest's actual test driver.

The order matters: `IM2_initialize` must come first so engine
calls don't crash on uninitialised globals.

## `M2-cpp-replacement.cpp`

```cpp
bool system_interrupted() { return false; }
```

A **one-line stub**. The engine's general code calls
`system_interrupted()` (from the interpreter's `interrupts.d`) in
inner loops to honor Ctrl+C. Linking the test binary doesn't pull
in the interpreter, so we need a stub. The stub always returns
`false` — tests should never get interrupted.

This file demonstrates the **engine/interpreter separation**: the
engine can build without the interpreter for testing purposes
because only this one symbol needed stubbing.

## `fromStream.cpp`

```cpp
#include "RingTest.hpp"
#include "aring-zzp.hpp"
#include "ZZp.hpp"

template <typename T>
std::istream& fromStream(std::istream& i,
                         const T& R,
                         typename T::ElementType& result);
```

Specialisations of `fromStream<T>` — the stream-parsing helper
declared in [`RingTest.hpp`](file-RingTest-hpp.md). Each ring type
that wants to support stream parsing in tests provides its
specialisation here.

The unified file (rather than per-test specialisations) keeps
test files focused on test logic and centralises the parser
quirks.

## `util-polyring-creation.{cpp,hpp}`

```cpp
const Monoid* degreeMonoid(const std::vector<std::string>& names);
const PolynomialRing* degreeRing(const std::vector<std::string>& names);
const PolynomialRing* degreeRing(int ndegrees);
```

Helpers for **building polynomial rings concisely in tests**:

```cpp
// Without this helper, every test repeats 10 lines of setup.
const PolynomialRing* R = degreeRing({"a", "b", "c"});
```

The helpers handle the boilerplate of:

- Creating a degree monoid.
- Picking an ordering.
- Wiring up the coefficient ring.

Used heavily by [`NewF4Test.cpp`](file-NewF4Test.md),
[`MatrixIOTest.cpp`](file-dmat-matrix-tests.md),
[`NCGroebnerTest.cpp`](file-NCGroebnerTest.md), and
[`PolyRingTest.cpp`](file-dmat-matrix-tests.md).

## Used by

- The `M2-unit-tests` binary itself (via `testMain.cpp`'s
  `main()`).
- Every other test file in this directory.
- The CMake `M2-unit-tests` target.

## Related

- [`README.md`](README.md) — unit-tests overview.
- [`file-RingTest-hpp.md`](file-RingTest-hpp.md) — `fromStream`
  template declarations.
- [`../file-engine-h.md`](../file-engine-h.md) (if added) —
  `IM2_initialize` declaration.
- [`../file-interrupted.md`](../file-interrupted.md) — the
  `system_interrupted` function that `M2-cpp-replacement.cpp`
  stubs out.
