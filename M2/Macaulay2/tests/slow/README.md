# `M2/Macaulay2/tests/slow/` — slow regression tests

This directory holds **slower-running regression tests** — ones that
take long enough to be impractical for every CI run but valuable to
exercise periodically. They are typically large GB computations,
deep resolutions, or stress tests of specific engine paths.

[← back to tests overview](../README.md)

## What's in here

14 `.m2` scripts at the time of writing:

| File | Topic |
|---|---|
| `4c.m2` | Engine boundary stress test |
| `complete-intersections.m2` | Resolution of a complete intersection |
| `forms.m2` | Bilinear / multilinear forms |
| `gb-1.aux`, `gb-1.m2` | A large GB computation |
| `gbZZ5.m2`, `gbZZbug3.m2`, `gbZZbug3-a.m2` | ZZ-coefficient GB stress |
| `global.m2` | Global vs. local ordering tests |

(Some files exist as paired `*-a.m2` / `*-b.m2` for parts of a single
test split into modular chunks.)

## When these run

The suite is not part of the default CI run. To trigger:

```sh
ctest -R "slow"        # CMake
make -C tests/slow     # autotools
```

The Macaulay2 maintainers run this periodically on a beefy machine
and check for regressions. Individual maintainers run subsets when
touching code that might affect a known-slow path.

## Why these are separated out

CI runs need to be fast enough that contributors don't lose patience
waiting. A few minutes is acceptable; an hour is not. Tests that
require an hour go here instead.

The slow tests are also more sensitive to environment — they can be
killed by OOM on small machines, can require lots of disk space, etc.
Keeping them out of CI avoids spurious failures.

## Catalogue

[`file-slow-tests-catalogue.md`](file-slow-tests-catalogue.md) — full
walkthrough with per-file topic mapping, approximate runtimes, the
`-a`/`-b` split convention, and `.aux` data-file pattern.

## Related

- [`../README.md`](../README.md) — overall test-suite overview.
- [`../normal/README.md`](../normal/README.md) — default-tier tests
  (must be fast).
- [`../gigantic/`](../) — even-slower stress tests.
- [`../engine/README.md`](../engine/README.md) — engine integration
  tests (currently CI-skipped).
