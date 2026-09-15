# ARing test retrofit, September 2026

## Scope

Git history identifies the generated tests in `ARingCCCTest.cpp`,
`ARingCCTest.cpp`, `ARingCCiTest.cpp`, `ARingRRTest.cpp`, `ARingRRRTest.cpp`,
`ARingZZTest.cpp`, `ARingZZGmpTest.cpp`, `ARingZZpTest.cpp`,
`ARingQQFlintTest.cpp`, and `ARingQQGmpTest.cpp`, plus `ARingQQTest.hpp`.
The relevant additions run from the CCi draft (`9c32d11464`, August 28) through
September 14's complex (`b064509e41`, `72a594e328`), real (`a2be0f01e2`,
`1763cca6af`, `565e8b04be`), integer (`160abc2433`, `5007b8b87e`), rational
(`ae0ad8c13f`), and finite-field (`f37d001e2e`) changes, merged September 15.
The older RRi and generic Ring tests are outside this retrofit.

Cases now describe their purpose, identify their inputs, and set those inputs
independently. Worked examples and generated properties are separated.
Owning elements handle cleanup where practical. Shared helpers report the trial
and input values, and callers seed the engine random streams. Existing test
names are retained, except for the required `DISABLED_` prefix on regressions.

## Corrected test assumptions

- FLINT integer division returns `false` for an inexact quotient. The GMP
  implementation throws; the tests honor each interface.
- Different values may have the same hash. The tests check equality of hashes
  for equal inputs instead of demanding collision freedom.
- GMP's `mpq_get_d` truncates. The rational reconstruction contract calls for
  nearest rounding, checked with a 53-bit MPFR value and `MPFR_RNDN`.
- The real arithmetic helpers report expected and actual values, error, and
  precision-aware tolerances. Relative scaling handles products above one;
  the existing bit allowances remain unchanged. MPFR comparison helpers reject
  NaN explicitly, since `mpfr_cmp` returning zero does not imply equality there.

## Known defects and exclusions

Both open and closed upstream issues were searched by component, operation,
and symptom before posting these reports. The related FLINT matrix issue
[#4639](https://github.com/Macaulay2/M2/issues/4639) describes a different defect.

| Disabled regression | Report |
| --- | --- |
| GMP integer ring name | [#4695](https://github.com/Macaulay2/M2/issues/4695) |
| FLINT rational const conversion | [#4696](https://github.com/Macaulay2/M2/issues/4696) |
| RR and RRR NaN comparison | [#4697](https://github.com/Macaulay2/M2/issues/4697) |
| ZZ/p constructor narrowing | [#4698](https://github.com/Macaulay2/M2/issues/4698) |
| ZZ/p zero-factor subtraction, a requested contract extension | [#4699](https://github.com/Macaulay2/M2/issues/4699) |

Runtime skips explain why finite-field-only helpers do not apply to ZZ or QQ,
why zero operands are excluded from real/complex syzygies and real reciprocals,
and why RRR storage initialization is not a promise to initialize to zero.

Remaining coverage gaps include `ARingCCi::diameter`, whose uninitialized MPFI
temporary prevents a safe call and whose intended diameter convention needs
clarification. Real hashing still converts negative or oversized floating-point
values to unsigned integers; equality checks do not replace sanitizer coverage
of that conversion. No production changes were made to conceal these gaps.

## Validation

Using the configured CMake Debug build, the full engine suite passes:
391 tests pass, 12 report their exclusions as skips, and six regressions are
disabled. A full shuffled run with seed 915 also passes in about one second.
Explicitly enabling the six disabled regressions makes all six fail, confirming
that they still reproduce their linked issues.

Commands, relative to the corresponding build directory:

```sh
cmake --build . --target M2-unit-tests
ctest -R '^unit-tests:' --output-on-failure --no-tests=error -j 8
./Macaulay2/e/M2-unit-tests --gtest_shuffle --gtest_random_seed=915
./Macaulay2/e/M2-unit-tests --gtest_filter='*.DISABLED_*' --gtest_also_run_disabled_tests
```

The separate `GCOV=ON` build also passes the full suite. Its report is generated
from the full `Macaulay2/e/CMakeFiles` tree, including the translation units that
instantiate inline ARing methods:

```sh
cmake --build . --target coverage-reset
./Macaulay2/e/M2-unit-tests
cmake --build . --target coverage-report
```

Coverage percentages refer to instrumented code; uninstantiated inline methods
such as CCi's diameter still need the explicit gap note above. No before/after
coverage comparison is claimed. Source lists were unchanged, and Autotools was
not run.
