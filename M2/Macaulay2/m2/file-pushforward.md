# `pushforward.m2` — `pushForward` for modules along a ring map

`pushforward.m2` implements **`pushForward`** — the operation that
takes a module over `S` and views it as a module over `R` via a
ring map `f : R → S`. It is the algebraic counterpart of pushing a
sheaf forward along a morphism of schemes.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-- Copyright 1996 Michael E. Stillman
-- TODO: adjust PushForward.m2 package to add a strategy
-- TODO: lift(M, S) should call pushForward in some cases

needs "basis.m2"
needs "modules.m2"
needs "ringmap.m2"
```

The TODOs flag opportunities to:

1. Make the `PushForward` user package strategy-aware.
2. Have `lift(M, S)` (raise a value from a quotient to the parent)
   call into `pushForward` for the cases where this would be a
   speed-up.

Neither is blocking; the file works as-is for the common cases.

## What `pushForward` does

Given:

- A ring map `f : R → S` (e.g. `R → S = R[x_1, …, x_n]/(some
  ideal)`).
- An `S`-module `M`.

Compute `f_* M` — `M` viewed as an `R`-module by restricting
scalars through `f`.

For `M` to be a finitely-generated `R`-module, `S` must be a
finitely-generated `R`-module to begin with. The algorithm computes
this finiteness check plus a presentation of `f_* M`.

## How

The implementation:

1. Compute a generating set for `S` as an `R`-module (via GB +
   `basis`).
2. For each generator `s_i ∈ S` and each generator `m_j ∈ M`,
   compute `s_i · m_j` in `M` and express it in terms of `R`-basis
   elements.
3. Assemble the resulting presentation.

This is heavy: a `pushForward` typically requires a GB plus
multiple `basis` computations.

## Companion: `PushForward` package

There's also a user-loadable `PushForward.m2` package that provides
additional strategies and edge-case handling. The Core file here
provides the simplest (`Default`) strategy.

## Used by

- Algebraic-geometry packages computing pushforwards of sheaves /
  modules.
- Module-theoretic constructions in commutative algebra.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-ringmap.md`](file-ringmap.md) — `RingMap` type.
- [`file-modules.md`](file-modules.md) — `Module` type.
- `basis.m2` — the module-basis computation called internally.
- `PushForward` user package — richer alternative.
