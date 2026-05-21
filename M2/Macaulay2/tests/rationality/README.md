# `M2/Macaulay2/tests/rationality/` — rationality-question regression tests

This directory contains **rationality-question tests** — computations
that decide whether certain algebraic varieties are rational, and the
intermediate computations (Gröbner bases, resolutions, Hilbert
series) those proofs rest on.

[← back to tests overview](../README.md)

## What's in here

| File | Role |
|---|---|
| `CMakeLists.txt` | CTest registration |
| `Makefile.in` | Autotools build glue |
| `README` | Original test-author notes |
| `ScorzaOcta.m2`, `ScorzaOcta-fixed.m2` | Scorza octant rationality |
| `plausibility.m2`, `plausibility1.m2`, …, `plausibility4.m2` | Plausibility checks |
| `proof.m2`, `proof-fixed.m2` | Specific rationality proofs |

The `-fixed` variants of `ScorzaOcta` and `proof` are versions where
the engine path was updated; the originals are kept for
side-by-side comparison.

## What's tested

A rationality question typically reduces to:

1. Compute a Gröbner basis or resolution of a specific ideal.
2. Read off invariants (Betti numbers, Hilbert series, dimensions).
3. Compare against the predicted values from theory.

If the engine drifts on Step 1, Steps 2 and 3 will fail
deterministically — this suite catches those drifts.

## Why a separate directory

The rationality computations are **expensive enough** to slow CI if
mixed into [`normal/`](../normal/README.md), but **mathematically
important** enough that they should run more often than
[`slow/`](../slow/README.md). They live in their own directory so
the test infrastructure can give them a separate budget.

## Triggering

```sh
ctest -R "rationality" --output-on-failure
```

## Related

- [`../README.md`](../README.md) — overall test-suite overview.
- [`../slow/README.md`](../slow/README.md) — slower regression tests.
- [`../normal/README.md`](../normal/README.md) — default tier.
- M2 packages relating to rationality questions (often in
  [`../../packages/`](../../packages/README.md)).
