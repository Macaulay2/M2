# `profiler.dd` — line-level profiler

`profiler.dd` implements M2's **line-level profiler** — the
machinery behind `profile` / `unprofile` that tracks how much time
the interpreter spends on each line of M2 code.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
-- Copyright 2024 by Mahrud Sayrafi

use binding;  -- for symbols
use evaluate; -- for eval
use chrono;   -- for wallTimer
use sets;     -- for Tally
```

A relatively new file (2024) by Mahrud Sayrafi. The imports tell
the whole story:

- **`binding`** — to map source positions to the symbols /
  declarations that own them.
- **`evaluate`** — to instrument the evaluator with timing hooks.
- **`chrono`** — wall-clock timing primitive.
- **`sets`** — `Tally` for the per-line counters.

## How profiling works

The profiler swaps the evaluator's per-expression dispatch path
for a timing wrapper:

```
Original:   evaluate(code) → ...
Profiled:   t0 = wallTimer(); result = evaluate(code); profileTally += (wallTimer() - t0, code.position)
```

Each line gets a `Tally` entry whose count is total nanoseconds
spent and a separate counter for visit count. After the run,
`profileSummary()` walks the tally and prints a hot-line report.

## Why `chrono` not `time(2)`

`chrono.dd` (built on `std::chrono::steady_clock`) gives
nanosecond resolution and is monotonic — neither of which `time(2)`
provides. Profiling small inner loops needs nanosecond
granularity.

## API surface

- `profile expr` — turn on, run, summary, turn off.
- `profile()` — print the current summary.
- `profile(true)` / `profile(false)` — manual on/off.
- `profileSummary()` — dump as `Tally`.

## Used by

- M2 users debugging performance.
- The benchmark suite in `tests/normal/`.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-chrono.md`](file-chrono.md) (if added) — building block.
- [`file-evaluate.md`](file-evaluate.md) — evaluator hooked into.
- [`file-sets.md`](file-sets.md) — `Tally` for counters.
- [`../m2/file-profile.md`](../m2/file-profile.md) — M2-side
  wrapper.
