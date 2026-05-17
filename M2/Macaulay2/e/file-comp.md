# `comp.hpp`, `comp.cpp` — `Computation` base class

`comp.hpp` and `comp.cpp` define **`Computation`** — the abstract
base class for **incremental computations** in M2 (Gröbner bases,
resolutions, Hilbert series, etc.). The class encapsulates the
**stop-condition machinery** that lets users interrupt long-running
computations cleanly.

Part of the [engine](README.md) — computations.

[← engine overview](README.md) · [computations](computations.md)

## What's declared

```cpp
class Computation : public MutableEngineObject
{
 private:
  enum ComputationStatusCode computation_status;

 protected:
  StopConditions stop_;
  ...
};
```

Pure-virtual abstract base. The interesting state:

- **`computation_status`** — current status (`COMP_DONE`,
  `COMP_INTERRUPTED`, `COMP_DEGREE_LIMIT_REACHED`, ...).
- **`stop_`** — the `StopConditions` struct (degree limit,
  basis-element limit, pair limit, etc.).

The class is the engine-side anchor for M2's incremental
computation API:

```m2
gb(I, DegreeLimit => 5)
```

becomes:

1. Construct a `GBComputation` (subclass of `Computation`).
2. Call `set_stop_conditions(...)` with `DegreeLimit = 5`.
3. Call `start_computation()`.
4. The computation runs until either complete or hitting a stop
   condition.
5. Inspect `computation_status` to see what happened.
6. Resume later by adjusting stop and calling `start_computation`
   again.

## `comp.cpp` — `set_stop_conditions`

```cpp
Computation /* or null */ *Computation::set_stop_conditions(
    M2_bool always_stop,
    M2_arrayint degree_limit,
    int basis_element_limit,
    int syzygy_limit,
    int pair_limit,
    int codim_limit,
    int subring_limit,
    M2_bool just_min_gens,
    M2_arrayint length_limit)
{
  stop_.always_stop = always_stop;
  stop_.stop_after_degree = (degree_limit != nullptr && degree_limit->len > 0);
  stop_.degree_limit = degree_limit;
  stop_.basis_element_limit = basis_element_limit;
  ...
}
```

The long signature is the **stop-condition vocabulary**: every
condition users can attach to a long-running computation:

| Param | Meaning |
|---|---|
| `always_stop` | stop on every degree boundary (testing) |
| `degree_limit` | max degree to compute |
| `basis_element_limit` | stop once GB has N elements |
| `syzygy_limit` | similar for syzygies |
| `pair_limit` | total S-pair budget |
| `codim_limit` | stop when codim ≥ N |
| `subring_limit` | restrict to elements in a subring |
| `just_min_gens` | only minimal generators |
| `length_limit` | for resolutions: max length |

## Why incremental

Real-world computations can run for hours or days. The
incremental design means:

- The user can **interrupt** cleanly (Ctrl-C → status =
  `COMP_INTERRUPTED`).
- The user can **resume** from where they stopped by raising the
  limit.
- The user can **inspect** partial state (current basis, current
  resolution length) before full completion.

The pattern shows up in every long-running engine path.

## Used by

- [`file-comp-gb.md`](file-comp-gb.md) (top-level Gröbner-basis
  computation).
- [`file-comp-res.md`](file-comp-res.md) (resolution computation).
- [`interface/file-computation-interface.md`](interface/file-computation-interface.md)
  — the C API on top.

## Related

- [`README.md`](README.md) — engine overview.
- [`computations.md`](computations.md) — area overview.
- [`groebner-bases.md`](groebner-bases.md) — primary client.
- [`resolutions.md`](resolutions.md) — secondary client.
