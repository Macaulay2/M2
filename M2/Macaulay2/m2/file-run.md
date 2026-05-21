# `run.m2` — example / test execution helpers

`run.m2` provides the **shared execution helpers** used by
[`file-examples.md`](file-examples.md) and
[`file-testing.md`](file-testing.md) — the machinery that spawns M2
subprocesses to execute example / test code in a clean environment.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-- TODO: parallelize testing
-- TODO: merge with capture.m2?

needs "system.m2"

-- see resetCounters and installPackage
numExampleErrors = 0    -- FIXME: this is not reentrant

--test limits
utest := opt -> (
    ...
)
```

The TODOs flag known work:

- Parallelise testing across CPU cores.
- Merge with `capture.m2` (which `examples.m2` uses).

The FIXME on `numExampleErrors` notes that this counter is shared
across the whole session — running two `installPackage`s
concurrently would interleave their error counts.

## What `run.m2` provides

- **`utest(opt)`** — extract test execution limits from options.
- **Subprocess spawning** — fork a fresh M2, write a script, capture
  output, parse results.
- **Time / memory limits** — enforced via shell-level limits or
  M2-level `alarm`.

## Why a separate file

`run.m2` exists so [`file-examples.md`](file-examples.md) and
[`file-testing.md`](file-testing.md) don't duplicate the subprocess
machinery. Both `installPackage` (running examples) and `check`
(running tests) follow the same pattern, just with different
input / output handling. Centralising here keeps the common code in
one place.

## Used by

- [`file-examples.md`](file-examples.md) — example capture.
- [`file-testing.md`](file-testing.md) — test execution.
- `installPackage` — calls into here for example execution.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-system.md`](file-system.md) — supplies `run` / `chkrun`.
- [`file-examples.md`](file-examples.md), [`file-testing.md`](file-testing.md)
  — primary consumers.
- `capture.m2` — the planned merge target.
