# Engine gtest suite architecture

This document is the **architectural reference** for
`M2/Macaulay2/e/unit-tests/` — M2's C++ unit-test suite for the
engine. Built on [googletest](https://github.com/google/googletest);
links against the engine library and runs as a standalone binary.

[← unit-tests/ overview](README.md) · [← engine architecture](../architecture.md) · [← testing reference](../../../../TESTING.md)

## What this suite tests

**Direct C++ engine API**. No interpreter, no `.d` translation
layer. Each test calls engine classes (`Matrix`, `Ring`,
`ARingZZpFlint`, etc.) directly and asserts on their behaviour.

Compared to other test infrastructures:

| Suite | Layer | Speed | Catches |
|---|---|---|---|
| **`e/unit-tests/`** (this) | Engine internals | < 1 min | C++ class-level bugs |
| `M2 --check N` | Full stack | 1-30 min | End-to-end bugs |
| `tests/normal/` | M2 user API | ~10 min | User-API-visible bugs |
| `tests/engine/` (skipped) | `raw…` boundary | varies | Boundary regression |

See [`TESTING.md`](../../../../TESTING.md) for the cross-cutting
overview.

## Four-layer architecture

```
┌──────────────────────────────────────────────────────┐
│   Harness                                              │
│   testMain.cpp, M2-cpp-replacement.cpp, fromStream.cpp │
├──────────────────────────────────────────────────────┤
│   Fixtures                                             │
│   ARingTest.hpp, RingTest.hpp, DMatTest.hpp            │
├──────────────────────────────────────────────────────┤
│   Helpers                                              │
│   util-polyring-creation.{hpp,cpp}                     │
├──────────────────────────────────────────────────────┤
│   Per-area test files                                  │
│   ARing*Test.cpp, Ring*Test.cpp, DMat*Test.cpp,        │
│   MonoidTest.cpp, PolyRingTest.cpp, NewF4Test.cpp,     │
│   NCGroebnerTest.cpp, ResTest.cpp, etc.                │
└──────────────────────────────────────────────────────┘
```

## Layer 1: Harness

The test binary's entry point and dependency stubs.

### `testMain.cpp`

```cpp
#include <gtest/gtest.h>
#include <M2/gc-include.h>
#include <engine.h>

int main(int argc, char **argv) {
  IM2_initialize();
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
```

The `M2-unit-tests` binary's `main()`. Three steps:

1. `IM2_initialize()` — boots engine globals (singleton rings,
   monoid prefabs, etc.). Must come first, else any test
   creating a `Ring` crashes.
2. `InitGoogleTest()` — parses gtest flags from `argv`.
3. `RUN_ALL_TESTS()` — gtest's standard driver.

**Source**: [`file-test-harness.md`](file-test-harness.md).

### `M2-cpp-replacement.cpp`

```cpp
bool system_interrupted() { return false; }
```

A **one-line stub**. The engine has inner-loop code that checks
`system_interrupted()` (from the interpreter's
[`d/file-interrupts.md`](../../d/file-interrupts.md)). The test
binary doesn't link the interpreter, so we stub this symbol.
Always returns `false` — tests never get interrupted by user
input.

The stub demonstrates the **clean engine/interpreter separation**:
the engine has only one runtime dependency on the interpreter, and
that one is easy to stub.

### `fromStream.cpp`

Stream-parsing helper specialisations for `Reader<T>` (see
[`../file-reader.md`](../file-reader.md)). Each ring type that
wants to parse from streams provides its specialisation here.
Used by tests that read input matrices / polynomials.

## Layer 2: Fixtures

Shared infrastructure for parameterised tests.

### `ARingTest.hpp` — templated aring fixture

```cpp
template <typename RingType>
void getElement(const RingType& R, int index,
                typename RingType::ElementType& result);

template <typename RingType>
class ARingElementGenerator { ... };
```

Each `ARing*Test.cpp` provides a specialisation of `getElement`
that knows how to construct elements of its specific ring. The
generator then walks through them deterministically (first 50 are
small integers, then random).

Used by every `aring`-framework test
([`file-aring-zz-tests.md`](file-aring-zz-tests.md),
[`file-aring-gf-tests.md`](file-aring-gf-tests.md),
[`file-aring-real-complex-tests.md`](file-aring-real-complex-tests.md)).

### `RingTest.hpp` — legacy-Ring fixture

Same idea for the older `Ring`-based API (where elements are
`ring_elem` instead of typed `ElementType`):

```cpp
template <typename T>
ring_elem getElement(const T& R, int index);
```

[`file-RingTest-hpp.md`](file-RingTest-hpp.md) +
[`file-ring-tests.md`](file-ring-tests.md).

### `DMatTest.hpp` — dense matrix fixture

Placeholder header for dense-matrix tests. Currently small but
positioned to grow. [`file-dmat-matrix-tests.md`](file-dmat-matrix-tests.md).

## Layer 3: Helpers

Shared constructors for test inputs.

### `util-polyring-creation.{hpp,cpp}`

```cpp
const Monoid* degreeMonoid(const std::vector<std::string>& names);
const PolynomialRing* degreeRing(const std::vector<std::string>& names);
```

Concise polynomial-ring construction from variable-name strings.
Without these helpers, each test would repeat ~10 lines of
`Monoid::create(...)` boilerplate.

Used by:
- [`file-NewF4Test.md`](file-NewF4Test.md) (F4 GB tests)
- [`file-NCGroebnerTest.md`](file-NCGroebnerTest.md) (NC GB tests)
- [`file-dmat-matrix-tests.md`](file-dmat-matrix-tests.md) (matrix
  I/O tests)

## Layer 4: Per-area test files

Tests grouped by what they exercise.

### Abstract rings (`aring` framework)

```
ARingZZTest.cpp          - FLINT integers
ARingZZpTest.cpp         - Z/p
ARingQQGmpTest.cpp       - GMP rationals
ARingQQFlintTest.cpp     - FLINT rationals
ARingGFTest.cpp          - FLINT Galois fields
GivaroTest.cpp           - Givaro fields
ARingRRTest.cpp          - double-backed reals
ARingRRRTest.cpp         - MPFR reals
ARingRRiTest.cpp         - MPFI intervals
ARingCCTest.cpp          - complex<double>
ARingCCCTest.cpp         - MPC complex
```

Pattern: each tests its ring against the shared
`ARingElementGenerator` from `ARingTest.hpp`. Tests cover
construction, arithmetic identities, comparison, edge cases.

### Legacy rings (`Ring` framework)

```
RingZZTest.cpp           - Integers
RingZZpTest.cpp          - Z/p (legacy Z_mod)
RingQQTest.cpp           - Rationals
RingRRRTest.cpp          - MPFR via glue
RingCCCTest.cpp          - MPC via glue
RingTowerTest.cpp        - Iterated extension rings
```

Same idea but using the legacy `ring_elem` API. Importantly,
the floating-point ring tests use `ConcreteRing<ARingXXX>` from
`aring-glue.hpp` — they're testing the **glue layer** that
bridges modern templated `aring` rings into the legacy `Ring`
interface.

### Matrices and modules

```
DMatZZpTest.cpp          - Dense matrices over Z/p
MatrixIOTest.cpp         - Matrix serialisation
PolyRingTest.cpp         - Polynomial rings
MonoidTest.cpp           - Monoid + ExponentVector primitives
```

### Gröbner bases and resolutions

```
NewF4Test.cpp            - Newer F4 GB engine
NCGroebnerTest.cpp       - Non-commutative GB
ResTest.cpp              - Schreyer-resolution monoid
```

### Misc

```
PointArray.cpp           - Numerical AG point arrays
SubsetTest.cpp           - Subset encoding combinatorics
basics-test.cpp          - Test-harness sanity (runs first)
```

## Test patterns

### Standard gtest pattern

```cpp
TEST(SuiteName, TestName) {
  // Arrange
  M2::ARingZZpFlint R(101);
  M2::ARingZZpFlint::ElementType a, b, c;
  R.init(a); R.init(b); R.init(c);

  // Act
  R.set_from_long(a, 50);
  R.set_from_long(b, 60);
  R.add(c, a, b);

  // Assert
  EXPECT_EQ(R.get_int(c), 9);  // (50 + 60) mod 101
}
```

Conventions:
- `EXPECT_*` for soft assertions (test continues on failure).
- `ASSERT_*` for hard assertions (test stops).
- `SCOPED_TRACE` for adding context when a helper fails.

### Parameterised over rings

```cpp
template <typename RingType>
class ARingZZpTest : public ::testing::Test { };

using RingTypes = ::testing::Types<
    M2::ARingZZpFlint,
    M2::ARingZZpFFPACK,
    M2::ARingZZpM2
>;

TYPED_TEST_SUITE(ARingZZpTest, RingTypes);

TYPED_TEST(ARingZZpTest, basicArithmetic) {
  // Runs once per RingType
  TypeParam R(101);
  // ...
}
```

(Approximate — actual patterns vary per file.)

The templated approach catches **backend-specific bugs**: if
`ARingZZpFlint` and `ARingZZpFFPACK` should be observably
equivalent, the same test running against both catches
divergences.

### Cross-backend validation

Two implementations of the same mathematical object (e.g.,
`ARingQQGMP` vs `ARingQQFlint`) should produce **identical
results**. The test suite cross-validates them:

```cpp
TEST(QQ, gmpAndFlintAgree) {
  M2::ARingQQGMP gmp;
  M2::ARingQQFlint flint;
  // ... construct same input in both ...
  // ... compute same operation in both ...
  EXPECT_EQ(toString(gmpResult), toString(flintResult));
}
```

This is the **canonical cross-check** that catches bugs in either
backend.

## Floating-point testing

Real and complex rings need special handling because
floating-point equality is fragile. Each `ARing*Test.cpp` for a
float type defines an `almostEqual` helper:

```cpp
bool almostEqual(const M2::ARingRR& R, unsigned long nbits,
                 const M2::ARingRR::ElementType& a,
                 const M2::ARingRR::ElementType& b) {
  ElementType epsilon = pow(2, -(int)nbits);
  ElementType c;
  R.subtract(c, a, b);
  R.abs(c, c);
  return c < epsilon;
}
```

The `nbits` parameter lets each test calibrate tolerance per
operation (`add` needs ~50 bits, `sqrt(2)^2 == 2` needs ~40).

See [`file-aring-real-complex-tests.md`](file-aring-real-complex-tests.md).

## Build wiring

```cmake
add_executable(M2-unit-tests
    testMain.cpp
    M2-cpp-replacement.cpp
    fromStream.cpp
    util-polyring-creation.cpp
    ARingZZTest.cpp
    # ... etc ...
)
target_link_libraries(M2-unit-tests
    PRIVATE M2-engine gtest pthread
)
```

(Approximate — see actual `CMakeLists.txt`.)

The binary links against:
- `M2-engine` — the engine library.
- `gtest` — Google Test (from the
  [submodule](../../../submodules/file-submodules.md)).
- `pthread`, math libs (transitively).

**Not** linked: the interpreter, the supervisor. That's why
`M2-cpp-replacement.cpp` is needed.

## Why test the engine in C++ directly

Several reasons:

1. **Catches engine bugs at their source.** A `ARingZZpFlint::add`
   bug is found in this test, not three layers up in an M2
   integration test.
2. **Fast iteration.** A C++ unit test is seconds to compile and
   sub-second to run.
3. **No interpreter overhead.** Tests can stress engine paths the
   interpreter doesn't normally exercise (e.g., specific
   `EXPECT_*` patterns).
4. **Templated tests cover many backends.** Hard to do at the
   M2 level.

The tradeoff: tests here can pass even when M2-level operations
fail (e.g., if the engine is correct but the boundary is buggy).
That's why M2 also has `tests/normal/` (M2-level) and `tests/engine/`
(`raw…` boundary) tests. Together they form a defense in depth.

## Adding a new test

1. **Pick the right file** — match the existing convention. New
   ring → `MyNewRingTest.cpp`. New algorithm → group with sister
   algorithms.
2. **Use the right fixture** — `ARingTest.hpp` for templated,
   `RingTest.hpp` for legacy.
3. **Write the test** following the standard `TEST(Suite, Name)`
   pattern.
4. **Add to `CMakeLists.txt`** if a new file.
5. **Run locally**:
   ```sh
   cmake --build M2/BUILD/build --target M2-unit-tests
   ./Macaulay2/e/unit-tests/M2-unit-tests --gtest_filter='Suite.*'
   ```

Per-file conventions:
[`file-test-harness.md`](file-test-harness.md),
[`file-ARingTest-hpp.md`](file-ARingTest-hpp.md).

## Running

```sh
# All tests, via CMake:
cd M2/BUILD/build
ctest --output-on-failure -R "unit-tests"

# Directly:
./Macaulay2/e/unit-tests/M2-unit-tests

# Filtered:
./Macaulay2/e/unit-tests/M2-unit-tests --gtest_filter='ARingZZ*'

# Under valgrind:
valgrind --suppressions=$M2/files/M2-suppressions.supp \
    ./Macaulay2/e/unit-tests/M2-unit-tests --gtest_filter='RingTower.*'
```

CI runs the full suite on every PR.

## Test ordering

`basics-test.cpp` is named to **sort alphabetically first**. It
runs a trivial check first; if it fails, the binary is broken
(linking issue, missing symbol, ABI mismatch). CI reports
"infrastructure broken" rather than "13,000 unrelated failures."

## File-by-file

See [`file-test-harness.md`](file-test-harness.md),
[`file-ARingTest-hpp.md`](file-ARingTest-hpp.md),
[`file-RingTest-hpp.md`](file-RingTest-hpp.md),
[`file-aring-zz-tests.md`](file-aring-zz-tests.md),
[`file-aring-gf-tests.md`](file-aring-gf-tests.md),
[`file-aring-real-complex-tests.md`](file-aring-real-complex-tests.md),
[`file-ring-tests.md`](file-ring-tests.md),
[`file-dmat-matrix-tests.md`](file-dmat-matrix-tests.md),
[`file-MonoidTest.md`](file-MonoidTest.md),
[`file-NewF4Test.md`](file-NewF4Test.md),
[`file-NCGroebnerTest.md`](file-NCGroebnerTest.md),
[`file-ResTest.md`](file-ResTest.md),
[`file-misc-tests.md`](file-misc-tests.md).

## Related

- [`README.md`](README.md) — unit-tests/ navigation hub.
- [`../architecture.md`](../architecture.md) — engine architecture.
- [`../../../../TESTING.md`](../../../../TESTING.md) — end-to-end
  testing reference.
- [`../../packages/file-EngineTests.md`](../../packages/file-EngineTests.md)
  — sister engine test suite at the M2 level.
- [`../../tests/engine/file-engine-tests-catalogue.md`](../../tests/engine/file-engine-tests-catalogue.md)
  — historical engine tests via `raw…` API.
- [`../coefficient-rings.md`](../coefficient-rings.md) — area
  being tested most heavily.
- googletest submodule under
  [`M2/submodules/`](../../../submodules/README.md).
