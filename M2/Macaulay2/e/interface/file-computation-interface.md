# `computation.h` (in `interface/`) — Computation status / stop-condition enums

`interface/computation.h` is the **smallest public-interface header** in
the engine: a single enum of `ComputationStatusCode` plus the
`StopConditions` struct. Everything that involves long-running engine
computations passes through these types.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Status codes

```c
enum ComputationStatusCode {
    COMP_NEED_RESIZE   = 1,    /* need resize */
    COMP_ERROR         = 2,    /* error */
    COMP_INTERRUPTED   = 3,    /* interrupted */
    COMP_NOT_STARTED   = 4,    /* not started */
    COMP_INITIAL_STOP  = 5,    /* StopBeforeComputation */
    COMP_DONE          = 6,    /* done */
    // ...
};
```

The enum is the single source of truth for the engine ↔ M2 boundary. The
header notes:

> Keep this enum in sync with `RawStatusCodes` in
> `Macaulay2/m2/gb.m2`

— a hard contract. When adding a new status, update both files in the
same commit.

After every `start_computation()` call on a
[`Computation`](../file-computation-framework.md), the interpreter polls
`status()` and switches on this enum to decide what to do next (resume,
report error, extract partial results, …).

## `StopConditions`

The header also declares the bit-vector / struct of stop conditions a
`Computation` can be configured with:

- `always_stop` — return after one iteration.
- `stop_after_degree`, `degree_limit` — degree-based bound.
- `basis_element_limit` — stop once the GB reaches a given size.
- `syzygy_limit` — for resolutions, bound on number of syzygies.
- `pair_limit` — bound on S-pair queue size.
- `codim_limit`, `subring_limit` — bound on dimension of intermediate
  objects.
- `length_limit` — for resolutions, homological-length bound.
- `just_min_gens` — only compute minimal generators.

The interpreter sets these via `set_stop_conditions(...)` on a
`Computation`. The Computation's `stop_conditions_ok()` virtual
([`file-computation-framework.md`](../file-computation-framework.md))
validates the combination.

## Why this header is so small

`computation.h` is intentionally minimal: it carries only the
**vocabulary** the interpreter and engine share. Concrete computation
classes (GB, resolution, Hilbert, …) build on this vocabulary in their
own interface headers
([`file-groebner-interface.md`](file-groebner-interface.md), …).

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-computation-framework.md`](../file-computation-framework.md) —
  `Computation` base class.
- [`../file-comp-gb.md`](../file-comp-gb.md), [`../file-comp-res.md`](../file-comp-res.md)
  — subclasses that respect these enums.
- [`../../m2/gb.m2`](../../m2/README.md) — keep `RawStatusCodes` in sync.
