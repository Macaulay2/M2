# `profile.m2` — profiling support for the `profile` keyword

`profile.m2` provides the M2-side support for the engine's
**`profile`** keyword — a built-in profiler that logs per-function
call counts and timings.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-- Copyright 2024 by Mahrud Sayrafi

-- 'profile' is an interpreter keyword, defined in d/profiler.dd,
-- which logs statistics of executed M2 code in 'ProfileTable'.
-- TODO: log the relationship between function calls and return
-- in an external format like Graphviz, pprof, etc.

needs "methods.m2"

head := () -> ("#run", "%time", "position")
```

Authored by Mahrud Sayrafi in 2024 — relatively new. The TODO
captures planned future work: exporting the profile data to
Graphviz / pprof for visualisation.

## What `profile` does

When the user wraps code in `profile(...)`:

```m2
profile (
    R = QQ[x, y, z]
    I = ideal(x*y - z, x^2 - y)
    gb I
)
```

M2 records per-function-call statistics:

- **#run** — how many times each function was called.
- **%time** — percentage of total time spent in that function.
- **position** — file + line where the function was defined.

After the `profile` block exits, the results print as a table.

## How the engine cooperates

The actual instrumentation lives in
[`../d/profiler.dd`](../d/README.md) — the interpreter has a hook on
every function call that increments a counter for the current
function. The hook is enabled when the user enters a `profile`
block; the M2 side of the integration (output formatting, table
construction) is in this file.

## Used by

- M2 users diagnosing slow code.
- Package authors optimising hot paths.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../d/profiler.dd`](../d/README.md) — interpreter-side
  instrumentation.
- `time` — simple "how long did this take" alternative.
- [`file-debugging.md`](file-debugging.md) — adjacent diagnostic
  tools.
