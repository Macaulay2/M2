# `M2/Macaulay2/tests/goals/` — worked examples and "goal" computations

This directory contains **worked-example tests** — computations
M2 users have asked for or that demonstrate specific capabilities,
preserved here so they keep working as the engine evolves.

[← back to tests overview](../README.md)

## What's in here

| File | Role |
|---|---|
| `ISSAC-97.m2` | Examples from the ISSAC 1997 paper |
| `Makefile.in` | Build glue |
| `README` | Original test-author notes |
| `gbZZ.m2` | A goal Gröbner-basis computation over `ZZ` |
| `joswig.m2` | An example from Michael Joswig's work |

The selection is **demonstrative** rather than exhaustive:

- **`ISSAC-97.m2`** — the ISSAC paper used these as examples; the
  engine should still produce the documented outputs.
- **`gbZZ.m2`** — a GB over `ZZ` that's interesting either because
  it's hard or because it once failed.
- **`joswig.m2`** — a polyhedral example from a published paper.

## How these differ from `normal/`

[`normal/`](../normal/README.md) is the broad regression-test
surface. `goals/` is targeted: each test is "this specific
computation must keep producing this specific answer" — usually
because someone in the community would notice if it changed.

A test ending up here typically follows one of two paths:

1. A user reports "M2 used to do X, now it doesn't"; the fix lands
   plus a test in `goals/` to guard the fix.
2. A paper publishes "M2 can compute Y in T seconds"; the input
   becomes a `goals/` test that prints both result and time.

## Triggering

```sh
ctest -R "goals" --output-on-failure
```

## Related

- [`../README.md`](../README.md) — overall test-suite overview.
- [`../normal/README.md`](../normal/README.md) — broad regression
  suite.
- [`../ComputationsBook/README.md`](../ComputationsBook/README.md) —
  book-derived suite (similar style, different scope).
