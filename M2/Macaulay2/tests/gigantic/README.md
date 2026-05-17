# `M2/Macaulay2/tests/gigantic/` — extreme-scale stress tests

This directory contains **the most expensive regression tests** — the
ones that need significant RAM and time. They are **not run in CI**
and not run by default; they are reserved for periodic manual runs on
machines with the resources for them.

[← back to tests overview](../README.md)

## What "gigantic" means here

Tests in this directory may:

- Allocate **gigabytes of memory** during a single computation.
- Run for **hours** on a typical workstation.
- Produce output that's only meaningful after substantial
  post-processing.

Trying to run these on a 4 GB machine will fail with out-of-memory
errors; trying to run them in CI will time out.

## Why keep them at all

They serve as **scaling tests**:

- Confirm that the engine handles inputs at the upper end of what
  the maintainers care about (large Hilbert series, deep
  resolutions, GBs of large ideals).
- Catch quadratic / cubic regressions that small inputs miss.
- Provide a reference for performance comparisons across versions.

The Macaulay2 maintainers run them by hand when a release is being
prepared or when investigating a performance regression report.

## How to run them

There is no automated dispatch; the user invokes the M2 scripts
directly with enough resources:

```sh
ulimit -v unlimited       # remove virtual memory cap
nice -n 19 M2 --silent --no-debug < tests/gigantic/<file>.m2
```

The output (or the timing data) is compared against historical
records the maintainers keep separately.

## Related

- [`../README.md`](../README.md) — overall test-suite overview.
- [`../slow/README.md`](../slow/README.md) — merely slow tests
  (minutes, not hours).
- [`../normal/README.md`](../normal/README.md) — default tier.
- GitHub Issues — long-running performance reports often reference
  these tests.
