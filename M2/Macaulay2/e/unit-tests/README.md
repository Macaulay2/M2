# `M2/Macaulay2/e/unit-tests/` — engine gtest suite

C++ unit tests for the engine, built with [googletest](https://github.com/google/googletest)
(vendored as a [submodule](../../../submodules/README.md)). These tests exercise
the engine **directly** in C++ — no interpreter, no `.d` translation layer.

## Coverage by area

### Abstract rings (`aring`)

| File | Tests |
|---|---|
| `ARingTest.hpp` | Common gtest fixtures |
| `ARingZZTest.cpp` | Integers |
| `ARingZZpTest.cpp` | Z/p |
| `ARingQQGmpTest.cpp`, `ARingQQFlintTest.cpp` | Rationals via GMP and FLINT |
| `ARingGFTest.cpp`, `GivaroTest.cpp` | Galois fields |
| `ARingRRTest.cpp`, `ARingRRRTest.cpp`, `ARingRRiTest.cpp` | Real rings (double, MPFR, interval) |
| `ARingCCTest.cpp`, `ARingCCCTest.cpp` | Complex rings |

### Legacy rings

| File | Tests |
|---|---|
| `RingTest.hpp` | Fixtures |
| `RingQQTest.cpp`, `RingRRRTest.cpp`, `RingCCCTest.cpp` | QQ, RR, CC |
| `RingTowerTest.cpp` | Iterated extension rings |

### Matrices and modules

| File | Tests |
|---|---|
| `DMatTest.hpp` | Common fixtures for dense-matrix tests |
| `DMatZZpTest.cpp` | Dense matrices over Z/p |
| `MatrixIOTest.cpp` | Matrix serialisation |
| `PolyRingTest.cpp` | Polynomial rings |

### Gröbner bases and resolutions

| File | Tests |
|---|---|
| `NewF4Test.cpp` | Newer F4 GB engine |
| `NCGroebnerTest.cpp` | Non-commutative GB |
| `ResTest.cpp` | Schreyer-frame resolution |

### Other

| File | Tests |
|---|---|
| `MonoidTest.cpp` | Monoid construction |
| `PointArray.cpp` | Numerical AG point arrays |
| `M2-cpp-replacement.cpp` | Tiny replacement implementations needed when linking against gtest in isolation |

### Build glue

| File | Role |
|---|---|
| `Makefile.in`, `Makefile.files` | Autotools build |
| `README` | Notes on running |

(`CMakeLists.txt` for this target lives one level up, alongside the rest of
the engine.)

## Running

```sh
cd M2/BUILD/build
cmake --build . --target M2-unit-tests
ctest --output-on-failure -R "unit-tests"
```

For a single test file:

```sh
./Macaulay2/e/unit-tests/M2-unit-tests --gtest_filter='ARingZZ*'
```

## Adding tests

Per the engine's design rule (see [`../README.md`](../README.md)), every
interface routine added in [`../interface/`](../interface/README.md) should
ship with a matching gtest here.

## Related

- googletest submodule under [`M2/submodules/`](../../../submodules/README.md).
- Integration test suites (CTest) in [`Macaulay2/tests/`](../../tests/README.md).

[← back to engine overview](../README.md)
