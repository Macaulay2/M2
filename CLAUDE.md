# Macaulay2 Engine Development Guide

## Project Overview

Macaulay2 is a software system for algebraic geometry research. This guide focuses on the
C++ engine in `M2/Macaulay2/e/`, which is the computational core.

## Repository Structure

```
M2/                              Top-level build directory
  CMakeLists.txt                 CMake entry point (requires 3.24+, C++17)
  configure.ac                   Autotools configuration
  Macaulay2/
    e/                           Engine (C++ computational core, ~500 source files)
      CMakeLists.txt             Builds M2-engine static library and M2-unit-tests
      Makefile.in                Autotools build for engine
      unit-tests/                GoogleTest-based unit tests
        Makefile.in              Autotools build for unit tests
        Makefile.files           List of test source files (shared by autotools)
      interface/                 Public C/C++ API (ring, matrix, groebner, etc.)
      f4/                        Faugere F4 algorithm
      gb-f4/                     GB F4 computation interface
      NCAlgebras/                Non-commutative algebra support
      schreyer-resolution/       Schreyer resolution (F4-style linear algebra)
      bibasis/                   Boolean Involutive Groebner Bases
    d/                           Interpreter
    m2/                          M2 language libraries
    packages/                    User packages
  submodules/                    Git submodules (googletest, flint, bdwgc, etc.)
  cmake/                         CMake modules (check-libraries, build-libraries, etc.)
  libraries/                     External library sources
```

## Engine Unit Tests

### Location and Framework

Tests are in `M2/Macaulay2/e/unit-tests/` using **GoogleTest** (gtest).

### Key Files

- `testMain.cpp` - Test entry point; calls `IM2_initialize()` then `RUN_ALL_TESTS()`
- `M2-cpp-replacement.cpp` - Stub for `system_interrupted()` (avoids linking interpreter)
- `ARingTest.hpp` - Templated test helpers for arithmetic ring operations (negate, add,
  subtract, multiply, divide, reciprocal, power, axioms, coercions)
- `RingTest.hpp` - Test helpers for Ring interface
- `DMatTest.hpp` - Test helpers for dense matrices
- `RingElem.hpp/cpp` - Lightweight value-semantics wrapper for ring elements (see below)
- `util-polyring-creation.hpp/cpp` - Helpers for creating rings in tests:
  - `simplePolynomialRing(p, names)` - polynomial ring over ZZ/p (or QQ if p=0)
  - `simpleWeylAlgebra(p, names, comms, derivs)` - Weyl algebra
  - `degreeRing(n)` - degree ring with n variables

### Existing Test Files

| Test File | What It Tests |
|-----------|---------------|
| `ARingZZTest.cpp` | Integers (flint) |
| `ARingZZpTest.cpp` | Z/p (multiple implementations) |
| `ARingQQFlintTest.cpp` | Rationals (flint) |
| `ARingQQGmpTest.cpp` | Rationals (GMP) |
| `ARingRRTest.cpp` | Machine reals |
| `ARingCCTest.cpp` | Machine complex |
| `ARingRRRTest.cpp` | Arbitrary-precision reals (MPFR) |
| `ARingRRiTest.cpp` | Arbitrary-precision real intervals (MPFI) |
| `ARingCCCTest.cpp` | Arbitrary-precision complex |
| `RingZZTest.cpp` | ZZ via Ring interface |
| `RingZZpTest.cpp` | ZZp via Ring interface |
| `RingQQTest.cpp` | QQ via Ring interface |
| `RingRRRTest.cpp` | RRR via Ring interface |
| `RingCCCTest.cpp` | CCC via Ring interface |
| `RingTowerTest.cpp` | Tower of polynomial rings |
| `DMatZZpTest.cpp` | Dense matrices over ZZp |
| `MonoidTest.cpp` | Monoid operations |
| `PolyRingTest.cpp` | Polynomial ring operations |
| `NCGroebnerTest.cpp` | Non-commutative Groebner bases |
| `WeylAlgebraTest.cpp` | Weyl algebra creation, commutators, binomial, multinomial, fromString |
| `NewF4Test.cpp` | New F4 algorithm |
| `ResTest.cpp` | Resolutions |
| `MatrixIOTest.cpp` | Matrix I/O |
| `SubsetTest.cpp` | Subset operations |
| `PointArray.cpp` | Point array operations |
| `basics-test.cpp` | Buffer, utility functions |
| `fromStream.cpp` | Stream parsing |

### Excluded Test Files

| Test File | Why Excluded |
|-----------|-------------|
| `ARingGFTest.cpp` | ARingGFFlint API changed: constructor now requires `PolynomialRing` + primitive element (was `(int p, int n)`), and `cardinality()` method is missing |

### Linking Requirements

The unit tests link against the M2 engine library. The only additional file needed is
`M2-cpp-replacement.cpp`, which stubs out `system_interrupted()` — a function normally
supplied by the Macaulay2 executable. `testMain.cpp` calls `IM2_initialize()` to
handle engine initialization (including GC).

### Writing New Tests

Pattern for arithmetic ring tests (standalone `TEST` macros):
```cpp
#include <gtest/gtest.h>
#include "aring-zz-flint.hpp"  // or whichever ring
#include "ARingTest.hpp"        // templated test helpers

TEST(MyRing, create) {
  M2::ARingZZ R;
  // ... test ring properties
}

TEST(MyRing, arithmetic) {
  M2::ARingZZ R;
  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  // ...
}
```

Pattern for tests using polynomial rings (use `util-polyring-creation.hpp`):
```cpp
#include "util-polyring-creation.hpp"
const PolynomialRing* R = simplePolynomialRing(101, {"a", "b", "c"});
const WeylAlgebra* W = simpleWeylAlgebra(0, {"x","y","Dx","Dy"}, {0,1}, {2,3});
```

Pattern for test fixtures with shared setup (use `TEST_F`):
```cpp
class MyTest : public ::testing::Test {
 protected:
  SomeRing* R = nullptr;
  void SetUp() override {
    R = /* create ring */;
  }
};

TEST_F(MyTest, someTest) {
  // R is available here, freshly created for each test
}
```

### Testing Private/Protected Members

Use the friend class pattern. Add `friend class FooTestAccessor;` to the class header,
then define a test accessor in the test file with static forwarding methods:
```cpp
// In the .hpp file:
class Foo {
  friend class FooTestAccessor;
  // ...
};

// In the test .cpp file:
class FooTestAccessor {
 public:
  static int privateMethod(const Foo* f, int arg) {
    return f->privateMethod(arg);
  }
};
```
See `WeylAlgebraTest.cpp` and `weylalg.hpp` for a concrete example.

### RingElem — Lightweight Value Wrapper for Tests

`RingElem` (in `unit-tests/RingElem.hpp`) wraps `const Ring*` + `ring_elem` with
value semantics. Operators return values (not pointers), making test code concise:
```cpp
#include "RingElem.hpp"
auto x  = RingElem::var(R, 0);
auto y  = RingElem::var(R, 1);
auto one = RingElem::fromInt(R, 1);
EXPECT_EQ(x * y - y * x, one);     // value comparison, prints elements on failure

// Parse from string (requires explicit ^ and * in polynomial syntax):
auto f = RingElem::fromString(R, "x^2+3*x*y-1");

// Scalar multiplication:
auto g = x * 3;       // RingElem * long
auto h = 3 * x;       // long * RingElem
```

Factories: `RingElem::var(R, i)`, `RingElem::fromInt(R, n)`, `RingElem::fromString(R, s)`.
Arithmetic: `+`, `-`, `*`, `/`, unary `-`, `.power(n)`.
Output: `to_string()`, `operator<<` for gtest diagnostics.

**fromString format**: Uses `parseBasicPoly` from `BasicPoly.hpp`. Requires `*` between
factors and `^` for exponents (e.g. `"3*x^2*y-1"`). Note: `to_string()` outputs a
different format (`x2y-1` without `*` or `^`), so round-tripping is not yet supported.

**Naming convention**: Member fields use `m` prefix (e.g., `mRing`, `mValue`).

### RingElement (Legacy Interface)

`RingElement` operators (`*`, `+`, `-`, `/`) return `RingElement*` (pointers), not values.
Prefer `RingElem` for new test code. `RingElement` is still used in the interpreter interface.
```cpp
RingElement *x = new RingElement(W, W->var(0));
RingElement *product = (*x) * (*y);   // dereference, then multiply
EXPECT_TRUE(product->is_equal(*expected));
```
`is_equal` checks ring pointer equality first — both elements must come from the same
ring object. Use `IM2_Ring_QQ()` (QQGMP) consistently, not `rawARingQQFlint()`, when
creating rings whose elements will be compared.

### M2_arrayint in Tests

Many engine functions take `M2_arrayint` (a GC-allocated array). Convert from
`std::vector<int>` using:
```cpp
#include "util.hpp"
M2_arrayint arr = stdvector_to_M2_arrayint(std::vector<int>{2, 3, 5});
```

## Building and Running Tests

Build configurations are defined in `M2/BUILD/mike/Makefile`. Existing builds live
under `M2/BUILD/mike/builds.tmp/`.

### CMake Build

The active cmake build directory is:
`M2/BUILD/mike/builds.tmp/cmake-appleclang` (RelWithDebInfo, Ninja)

```sh
# Build unit tests:
ninja -C M2/BUILD/mike/builds.tmp/cmake-appleclang M2-unit-tests

# Run all engine unit tests:
ctest --test-dir M2/BUILD/mike/builds.tmp/cmake-appleclang -R unit-tests

# Run a specific test by name:
ctest --test-dir M2/BUILD/mike/builds.tmp/cmake-appleclang -R "WeylAlgebra" --output-on-failure

# Or run the executable directly:
M2/BUILD/mike/builds.tmp/cmake-appleclang/Macaulay2/e/M2-unit-tests
# With a gtest filter:
M2/BUILD/mike/builds.tmp/cmake-appleclang/Macaulay2/e/M2-unit-tests --gtest_filter="WeylAlgebra*"
```

The CMake build uses `gtest_discover_tests()` with prefix `unit-tests:`.

### Autotools Build

The active autotools build directory is:
`M2/BUILD/mike/builds.tmp/arm64-appleclang`

```sh
# From the autotools build directory, in Macaulay2/e/unit-tests/:
gmake -k check
# Or run the executable directly:
./testMain
```

If `e/unit-tests/Makefile.in` changes, regenerate the build Makefile:
```sh
# From the autotools build root:
./config.status Macaulay2/e/unit-tests/Makefile
```

### Adding a New Test File

1. Add the `.cpp` file to `e/unit-tests/`
2. Add it to `e/CMakeLists.txt` in the `add_executable(M2-unit-tests ...)` section
3. Add it to `e/unit-tests/Makefile.files` in the `UNITTEST_CCFILES` list
4. Build and run tests in **both** cmake and autotools to verify

### Keeping Builds in Sync

The CMake and autotools builds maintain separate lists of test files:
- CMake: `e/CMakeLists.txt` — the `add_executable(M2-unit-tests ...)` block
- Autotools: `e/unit-tests/Makefile.files` — the `UNITTEST_CCFILES` variable

Both should include the same set of test files. Currently 199 tests pass in both builds.

## Engine Dependencies

All of the following are linked to the `M2-engine` target (see `e/CMakeLists.txt`).

### Submodule Dependencies (built as part of engine)
- **memtailor** - Special-purpose memory allocators
- **mathic** - Symbolic algebra data structures
- **mathicgb** - Signature Groebner bases library
- **googletest** - Unit testing framework (for M2-unit-tests only)

### Header-Only Libraries
- **Eigen3** (3.4.0+) - Linear algebra templates

### Libraries (found via pkg-config)
- **FFLAS_FFPACK** (2.4.3+) - Finite field linear algebra routines (needs LAPACK, GIVARO)
- **GIVARO** (4.1.1+) - Prime field and algebraic computations

### Libraries (found via find_package / FindXxx.cmake)
- **GMP** (6.0.0+) - GNU multiprecision arithmetic
- **MPFR** (4.0.1+) - Multiprecision floating-point (needs GMP)
- **MPFI** (1.5.1+) - Multiprecision floating-point intervals (needs GMP, MPFR)
- **FLINT** (3.0.0+) - Fast library for number theory (needs GMP, MPFR)
- **NTL** (10.5.0+) - Number theory library (needs GMP)
- **FACTORY** (4.4.0+) - Polynomial factorization (needs GMP, FLINT, NTL)
- **BDWGC** (7.6.4+) - Boehm-Demers-Weiser garbage collector
- **LAPACK** - Linear algebra (BLAS/LAPACK)
- **MPSOLVE** (3.2.0+) - Multiprecision polynomial solver
- **FROBBY** (0.9.0+) - Computations with monomial ideals
- **NORMALIZ** (3.8.0+) - Discrete convex geometry (needs GMP, Nauty)
Note: READLINE, HISTORY, and GDBM are in the top-level `LIBRARY_LIST` but are only
used by the interpreter (`d/`), not the engine.

### Optional
- **OpenMP** - Parallel processing

## Code Conventions

- C++17 standard (`-std=gnu++17`)
- Arithmetic ring types live in `M2` namespace (e.g., `M2::ARingZZ`, `M2::ARingZZp`)
- Ring elements use init/clear pattern: `R.init(a); ... R.clear(a);`
- `buffer` class (from `buffer.hpp`) used for string building and text output
- Interface functions prefixed with `IM2_` (e.g., `IM2_initialize()`)
- Source files generally come in `.cpp`/`.hpp` pairs
- Use `IM2_Ring_QQ()` for the rationals (QQGMP) in tests, not `rawARingQQFlint()`
- New classes should use `m` prefix for member fields (e.g., `mRing`, `mValue`)

## Branch Info

- Current branch: `unit-testing` (improving engine unit test infrastructure)
- PR target: `stable`
