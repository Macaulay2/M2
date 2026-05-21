# `timing.{cpp,hpp}` — engine-side timing helpers

`timing.hpp` declares **engine-side timing helpers** — small wrappers
around `std::chrono` (or equivalent) that engine code uses to record
elapsed time. It supplies the timestamps the SLP evaluator
([`file-SLP-imp.md`](file-SLP-imp.md)) reports and that benchmarks
across the engine use.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## What it provides

The header declares a small set of free functions / type aliases for:

- **Current time** — capture a high-resolution timestamp.
- **Elapsed time** — compute the difference in seconds (or
  milliseconds) between two timestamps.
- **Conversion helpers** — format the elapsed time for display.

The implementation uses `std::chrono::high_resolution_clock` (or a
platform-specific equivalent where that's slow).

## Why a separate header

The engine could use `std::chrono` directly everywhere, but
centralising the timestamp type in this header lets:

- The engine swap clocks (e.g. for testing — replace the real clock
  with a deterministic mock).
- Per-platform clock-source choices live in one place.
- Future thread-aware timings (per-thread CPU time, etc.) be wired
  in without touching call sites.

## Used by

- [`file-SLP-imp.md`](file-SLP-imp.md) — SLP evaluator reports
  per-evaluation time.
- F4 GB engines — track time per Macaulay-matrix reduction step for
  trace output.
- The [`Computation`](file-computation-framework.md) framework's
  stop conditions — `time_limit` is checked against accumulated
  evaluation time tracked here.

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`file-SLP-imp.md`](file-SLP-imp.md) — primary user.
- [`file-computation-framework.md`](file-computation-framework.md) —
  time-bounded computations.
