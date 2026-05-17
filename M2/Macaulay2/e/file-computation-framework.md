# `comp.{cpp,hpp}` — the `Computation` framework

`Computation` is the abstract base class that every long-running engine
algorithm inherits from: Gröbner bases, free resolutions, Hilbert functions,
LLL — anything that may be paused, resumed, given a time budget, or queried
for partial results.

[← per-area: groebner-bases](groebner-bases.md) · [← engine overview](README.md)

## The protocol

```cpp
class Computation : public MutableEngineObject
{
  enum ComputationStatusCode computation_status;
  StopConditions stop_;

public:
  Computation *set_stop_conditions(...);     // configure
  enum ComputationStatusCode status() const; // observe
  virtual int complete_thru_degree() const = 0;
  // ... start_computation() is the worker
};
```

A typical use cycle:

1. The interpreter creates a concrete subclass (e.g. `GBComputation`) and
   gets back a `Computation*`.
2. The user calls `set_stop_conditions` to configure limits — degree, basis
   size, time, syzygy count, codimension, …
3. The interpreter calls `start_computation()`; the subclass loops until
   either the algorithm completes or one of the stop conditions trips.
4. After return, `status()` tells the caller why we stopped:
   `COMP_DONE`, `COMP_INTERRUPTED`, `COMP_DONE_DEGREE_LIMIT`, …
5. The interpreter can re-call `start_computation()` with relaxed conditions
   to continue from where it left off.

## Stop conditions

`StopConditions` (defined in [`interface/computation.h`](interface/README.md))
is a struct of flags and limits:

- `always_stop` — return after current iteration
- `degree_limit` — stop once a degree bound is hit
- `basis_element_limit` — stop when the basis reaches a size
- `syzygy_limit`, `pair_limit`, `codim_limit`, `subring_limit`, `length_limit`
- `just_min_gens` — only compute minimal generators

These are passed through `set_stop_conditions` and consumed by the subclass.

## Subclasses

| Subclass | Where | Algorithm |
|---|---|---|
| `GBComputation` | [`comp-gb.{cpp,hpp}`](comp-gb.cpp) | Gröbner basis |
| `ResolutionComputation` | [`comp-res.{cpp,hpp}`](comp-res.cpp) | Free resolution |

These dispatch in turn to concrete algorithms — see
[`groebner-bases.md`](groebner-bases.md) and
[`resolutions.md`](resolutions.md).

## Resumability and threading

Because Computations are designed to be resumable, they keep all their working
state on the heap (GC-managed). The supervisor ([`../system/`](../system/README.md))
can run a Computation in a worker thread; the interpreter polls `status()` or
waits on a synchronisation primitive. The proxy machinery for cross-thread
Computations lives in
[`comp-gb-proxy.{cpp,hpp}`](comp-gb-proxy.cpp).

## Related

- [`groebner-bases.md`](groebner-bases.md) — primary subclass family.
- [`resolutions.md`](resolutions.md) — second subclass family.
- [`computations.md`](computations.md) — other consumers of the framework.
- [`interface/computation.h`](interface/README.md) — public-side type
  definitions and `StopConditions`.
