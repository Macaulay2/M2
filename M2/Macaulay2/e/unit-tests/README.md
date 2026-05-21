# `M2/Macaulay2/e/unit-tests/` — engine gtest suite

**See [`architecture.md`](architecture.md)** for the standalone architectural reference (four-layer design: harness → fixtures → helpers → per-area tests; test patterns including templated cross-backend validation; floating-point `almostEqual` helpers; build wiring; why test the engine in C++ directly).

C++ unit tests for the engine, built with [googletest](https://github.com/google/googletest)
(vendored as a [submodule](../../../submodules/README.md)). These tests exercise
the engine **directly** in C++ — no interpreter, no `.d` translation layer.

## Coverage by area

### Fixtures and harness

| File | Tests | Deep dive |
|---|---|---|
| `ARingTest.hpp` | Common gtest fixture for `aring` tests | [`file-ARingTest-hpp.md`](file-ARingTest-hpp.md) |
| `RingTest.hpp` | Fixture for legacy-ring tests | [`file-RingTest-hpp.md`](file-RingTest-hpp.md) |
| `DMatTest.hpp` | Fixture for dense-matrix tests | [`file-dmat-matrix-tests.md`](file-dmat-matrix-tests.md) |
| `testMain.cpp` | `main()` for `M2-unit-tests` | [`file-test-harness.md`](file-test-harness.md) |
| `M2-cpp-replacement.cpp` | Stubs for engine-only build | [`file-test-harness.md`](file-test-harness.md) |
| `fromStream.cpp` | Stream-parsing helper specialisations | [`file-test-harness.md`](file-test-harness.md) |
| `util-polyring-creation.{cpp,hpp}` | Ring-construction helpers | [`file-test-harness.md`](file-test-harness.md) |

### Abstract rings (`aring`)

| File | Tests | Deep dive |
|---|---|---|
| `ARingZZTest.cpp` | FLINT integers | [`file-aring-zz-tests.md`](file-aring-zz-tests.md) |
| `ARingZZpTest.cpp` | Z/p | [`file-aring-zz-tests.md`](file-aring-zz-tests.md) |
| `ARingQQGmpTest.cpp` | GMP rationals | [`file-aring-zz-tests.md`](file-aring-zz-tests.md) |
| `ARingQQFlintTest.cpp` | FLINT rationals | [`file-aring-zz-tests.md`](file-aring-zz-tests.md) |
| `ARingGFTest.cpp` | FLINT Galois fields | [`file-aring-gf-tests.md`](file-aring-gf-tests.md) |
| `GivaroTest.cpp` | Givaro fields | [`file-aring-gf-tests.md`](file-aring-gf-tests.md) |
| `ARingRRTest.cpp` | `double`-backed reals | [`file-aring-real-complex-tests.md`](file-aring-real-complex-tests.md) |
| `ARingRRRTest.cpp` | MPFR reals | [`file-aring-real-complex-tests.md`](file-aring-real-complex-tests.md) |
| `ARingRRiTest.cpp` | MPFI intervals | [`file-aring-real-complex-tests.md`](file-aring-real-complex-tests.md) |
| `ARingCCTest.cpp` | `complex<double>` | [`file-aring-real-complex-tests.md`](file-aring-real-complex-tests.md) |
| `ARingCCCTest.cpp` | MPC complex | [`file-aring-real-complex-tests.md`](file-aring-real-complex-tests.md) |

### Legacy rings

| File | Tests | Deep dive |
|---|---|---|
| `RingZZTest.cpp` | Integers | [`file-ring-tests.md`](file-ring-tests.md) |
| `RingZZpTest.cpp` | Z/p (legacy `Z_mod`) | [`file-ring-tests.md`](file-ring-tests.md) |
| `RingQQTest.cpp` | Rationals | [`file-ring-tests.md`](file-ring-tests.md) |
| `RingRRRTest.cpp` | MPFR via glue | [`file-ring-tests.md`](file-ring-tests.md) |
| `RingCCCTest.cpp` | MPC via glue | [`file-ring-tests.md`](file-ring-tests.md) |
| `RingTowerTest.cpp` | Iterated extension rings | [`file-ring-tests.md`](file-ring-tests.md) |

### Matrices and modules

| File | Tests | Deep dive |
|---|---|---|
| `DMatZZpTest.cpp` | Dense matrices over Z/p | [`file-dmat-matrix-tests.md`](file-dmat-matrix-tests.md) |
| `MatrixIOTest.cpp` | Matrix serialisation | [`file-dmat-matrix-tests.md`](file-dmat-matrix-tests.md) |
| `PolyRingTest.cpp` | Polynomial rings | [`file-dmat-matrix-tests.md`](file-dmat-matrix-tests.md) |
| `MonoidTest.cpp` | `Monoid` + `ExponentVector` | [`file-MonoidTest.md`](file-MonoidTest.md) |

### Gröbner bases and resolutions

| File | Tests | Deep dive |
|---|---|---|
| `NewF4Test.cpp` | Newer F4 GB engine | [`file-NewF4Test.md`](file-NewF4Test.md) |
| `NCGroebnerTest.cpp` | Non-commutative GB | [`file-NCGroebnerTest.md`](file-NCGroebnerTest.md) |
| `ResTest.cpp` | Schreyer-frame resolution monoid | [`file-ResTest.md`](file-ResTest.md) |

### Misc

| File | Tests | Deep dive |
|---|---|---|
| `PointArray.cpp` | Numerical AG point arrays | [`file-misc-tests.md`](file-misc-tests.md) |
| `SubsetTest.cpp` | Subset encoding | [`file-misc-tests.md`](file-misc-tests.md) |
| `basics-test.cpp` | Test-harness sanity | [`file-misc-tests.md`](file-misc-tests.md) |

**Coverage:** every test source file in this directory now has a dedicated deep-dive doc (grouped by topic for cohesion).

### Build glue

| File | Role |
|---|---|
| `Makefile.in`, `Makefile.files` | Autotools build |
| `README` | Notes on running |

(`CMakeLists.txt` for this target lives one level up, alongside the rest of
the engine.)

## What gets tested where

Mapping engine area → test files. Use this when adding tests after touching engine code:

| If you touched … | Run these tests |
|---|---|
| Any `aring-*` ring (`ARingZZ`, `ARingZZpFlint`, `ARingGFFlint`, `ARingRR`, …) | `ARing*Test.cpp` family — grouped by ring kind in [`file-aring-zz-tests.md`](file-aring-zz-tests.md), [`file-aring-gf-tests.md`](file-aring-gf-tests.md), [`file-aring-real-complex-tests.md`](file-aring-real-complex-tests.md) |
| Legacy `Ring`/`Z_mod`/`Tower` (in `ZZ.cpp`, `ZZp.cpp`, `tower.cpp`, …) | `Ring*Test.cpp` family — covered in [`file-ring-tests.md`](file-ring-tests.md) |
| Monoid / monomial-order code (`monoid.cpp`, `monordering.cpp`, `imonorder.cpp`) | `MonoidTest.cpp` — [`file-MonoidTest.md`](file-MonoidTest.md) |
| Polynomial-ring code (`polyring.cpp`, `BasicPoly.cpp`, …) | `PolyRingTest.cpp` — covered in [`file-dmat-matrix-tests.md`](file-dmat-matrix-tests.md) |
| Dense-matrix code (`dmat.cpp`, `dmat-*-flint.cpp`, `mat-arith.hpp`) | `DMatZZpTest.cpp`, `MatrixIOTest.cpp` — [`file-dmat-matrix-tests.md`](file-dmat-matrix-tests.md) |
| `f4/` or `gb-f4/` GB engine | `NewF4Test.cpp` — [`file-NewF4Test.md`](file-NewF4Test.md) |
| `NCAlgebras/` GB engine | `NCGroebnerTest.cpp` — [`file-NCGroebnerTest.md`](file-NCGroebnerTest.md) |
| `schreyer-resolution/` engine | `ResTest.cpp` — [`file-ResTest.md`](file-ResTest.md) |
| Numerical AG (`NAG.cpp`, `SLP.cpp`) | `PointArray.cpp` — [`file-misc-tests.md`](file-misc-tests.md) |
| Combinatorial helpers (`comb.cpp`) | `SubsetTest.cpp` — [`file-misc-tests.md`](file-misc-tests.md) |
| The test harness itself (`testMain.cpp`, `M2-cpp-replacement.cpp`, `fromStream.cpp`, `util-polyring-creation.cpp`) | [`file-test-harness.md`](file-test-harness.md) |

Then run a targeted gtest filter (see below) rather than the whole suite.

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

Common gtest filters when iterating on one area:

```sh
--gtest_filter='ARing*'         # all ARing tests
--gtest_filter='ARingZZ*'        # ARingZZ family
--gtest_filter='*F4*'            # F4 tests
--gtest_filter='*Monoid*'        # MonoidTest
--gtest_filter='*Subset*'        # Subset / combinatorial tests
--gtest_filter='-PointArray*'    # everything EXCEPT PointArray
```

## Adding tests

Per the engine's design rule (see [`../README.md`](../README.md)), every
interface routine added in [`../interface/`](../interface/README.md) should
ship with a matching gtest here.

## Related

- googletest submodule under [`M2/submodules/`](../../../submodules/README.md).
- Integration test suites (CTest) in [`Macaulay2/tests/`](../../tests/README.md).

[← back to engine overview](../README.md)
