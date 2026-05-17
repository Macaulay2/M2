# `M2/Macaulay2/tests/engine/` — engine integration tests (CI-skipped)

This directory contains **engine integration tests** that exercise the
engine through M2-level scripts but with finer-grained coverage than
the [`normal/`](../normal/README.md) suite. Currently the suite is
**skipped in CI** per upstream issue
[#1213](https://github.com/Macaulay2/M2/issues/1213).

[← back to tests overview](../README.md)

## Why these are skipped

The engine tests run M2 scripts that perform low-level engine
checks — direct `raw…()` calls, GB-internal state probes, edge cases
that the higher-level [`normal/`](../normal/README.md) suite cannot
reach. Many of them date back to the early engine and rely on
behaviour that has since drifted; bringing them all back into CI
requires either:

- Rewriting the affected tests against current engine behaviour.
- Adding compatibility shims to the engine.

Neither path has been prioritised because the [`normal/`](../normal/README.md)
suite plus the C++ gtest in [`../../e/unit-tests/`](../../e/unit-tests/README.md)
already cover the regression surface well.

## Files (35 in total)

The directory is a flat list of `.m2` scripts. Names hint at coverage:

| File | Topic |
|---|---|
| `LU.m2` | LU decomposition |
| `gb-2.m2`, `gb-bayes.m2`, `gb-bench.m2`, `gb-quotients.m2` | Gröbner basis edges |
| `normal.m2` | Generic engine tests |
| `raw-decompose.m2`, `raw-freemodule.m2`, `raw-gb.m2` | Engine `raw…()` entry points |

## How to run anyway

Even when CI skips them, you can run an individual test manually:

```sh
M2 --silent --no-debug --check 0 < some-test.m2
```

Or run the full suite:

```sh
cd M2/BUILD/build/Macaulay2/tests/engine
make
```

A subset will likely fail; the failures are documented in #1213 and
adjacent issues.

## Related

- Upstream issue [#1213](https://github.com/Macaulay2/M2/issues/1213).
- [`../README.md`](../README.md) — overall test-suite overview.
- [`../../e/unit-tests/`](../../e/unit-tests/README.md) — engine
  C++ gtest suite (the in-CI engine coverage).
- [`../normal/README.md`](../normal/README.md) — broader regression
  suite that does run in CI.
