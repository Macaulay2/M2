# `chrono.dd` — wall-clock and CPU timing

`chrono.dd` exposes **C++ `std::chrono`** primitives to M2 —
high-resolution wall clock, CPU time, and the `time` operator.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
-- TODO: get core time: number of cpu clock ticks spent in the core/libraries
-- TODO: define time and elapsedTime in the top level instead, and print in the AfterPrint
-- TODO: auto-calibrate spin based on the wall time

use arithmetic; -- for types
use binding;    -- for symbols
use evaluate;   -- for eval
use expr;       -- for timeClass
```

The three TODOs at the top capture in-flight refactors. Until
they're done, `chrono.dd` is responsible for everything
time-related.

## What's exposed

- **`wallTimer()`** — `std::chrono::steady_clock` monotonic timer
  returning nanoseconds.
- **`cpuTimer()`** — process CPU time.
- **`time expr`** — operator that wraps an expression with
  timing.
- **`elapsedTime expr`** — wall-clock variant of `time`.

The `time` operator is what M2 users see most often:

```m2
time computeGroebnerBasis(I)
```

It prints `used X seconds` (wall time) and returns the result.

## Why `.dd` not `.d`

`std::chrono` is C++-only. The `.dd` form lets `scc1` compile this
file to C++ and link against `<chrono>`.

## Precision

`std::chrono::steady_clock` resolution depends on platform:

| Platform | Typical resolution |
|---|---|
| Linux (modern) | nanoseconds (via `clock_gettime`) |
| macOS | nanoseconds (via `mach_absolute_time`) |
| Older systems | microseconds |

Good enough for both per-line profiling
([`file-profiler.md`](file-profiler.md)) and human-scale `time`
output.

## Used by

- [`file-profiler.md`](file-profiler.md) — primary consumer.
- M2's `time` / `elapsedTime` operators.
- The example runner timing.
- Test suite measuring runtime.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-profiler.md`](file-profiler.md) — built on this.
- [`file-system.md`](file-system.md) — sister POSIX time bindings
  (`time(2)`).
- [`../m2/file-time.md`](../m2/file-time.md) — M2-side wrappers
  (if any).
