# `computations.m2` — the `Computation` framework on the M2 side

`computations.m2` provides the M2-side wrapper around the engine's
[`Computation`](../e/file-computation-framework.md) class family —
the shared infrastructure for long-running, resumable, interruptible
computations (GB, resolutions, Hilbert series, …).

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-- Copyright 2021 Mahrud Sayrafi
-- ...GPL header...
```

Authored by Mahrud Sayrafi (2021). The file is one of the newer
additions to the Core M2 layer.

## What it provides

- **`Computation`** — M2 type. Wraps a `RawComputation*` engine
  pointer plus M2-side metadata.
- **Stop conditions** — `StopConditions`-type options translated
  from M2 to the engine via
  [`../e/interface/file-computation-interface.md`](../e/interface/file-computation-interface.md).
- **`isComputed`, `isReady`** — non-blocking status queries.
- **`status C`** — read back the current status code from the engine.

## How it fits

Every long-running M2 operation (GB, resolution, ...) eventually
calls into a `Computation`:

```m2
gb(I)
-- internally:
C = computation(I, options)
while not isComputed C do (
    set_stop_conditions(C, currentOptions)
    start_computation C
)
result = getGB C
```

The user typically doesn't see the `Computation` object — they just
see `gb I` returning a `GroebnerBasis`. The `Computation` is the
state behind that.

## Reusability

Because `Computation`s store their state, you can:

- Compute partially (`gb(I, DegreeLimit => 5)`).
- Inspect partial results.
- Continue with relaxed conditions (`gb(I, DegreeLimit => 10)`).

The engine reuses the cached state — no recomputation of the work
already done.

## Used by

- [`file-gb.md`](file-gb.md) — primary user.
- [`file-complexes.md`](file-complexes.md) — for resolutions.
- [`file-hilbert.md`](file-hilbert.md) — for Hilbert computations.
- M2 packages that need fine-grained control over expensive
  computations.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-computation-framework.md`](../e/file-computation-framework.md)
  — engine class.
- [`../e/interface/file-computation-interface.md`](../e/interface/file-computation-interface.md)
  — C entry points.
- [`file-gb.md`](file-gb.md), [`file-complexes.md`](file-complexes.md)
  — primary consumers.
