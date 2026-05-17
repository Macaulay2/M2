# Non-commutative free resolutions architecture

This document is the **architectural reference** for
`M2/Macaulay2/e/NCResolutions/` — engine support for computing
free resolutions over non-commutative graded algebras.

[← NCResolutions/ overview](README.md) · [← engine architecture](../architecture.md)

## Current scope

This is the **smallest** of the engine subdirectories (one
`{cpp,hpp}` pair plus a design-notes file) and the **youngest**.
The implementation is still maturing; this doc captures the
*intent* and *integration boundary* rather than the algorithm in
detail.

## What gets computed

Given a finitely-presented graded module over a non-commutative
algebra `R = F<x_1, …, x_n> / I` (where `I` is a two-sided ideal
with a known Gröbner basis), compute the early levels of a free
resolution:

```
… → R^{a_3} → R^{a_2} → R^{a_1} → R^{a_0} → M → 0
```

Same shape as the commutative case
([`../schreyer-resolution/architecture.md`](../schreyer-resolution/architecture.md)),
but with NC arithmetic at every step.

## Two-line architecture

```
┌──────────────────────────────────────────────────────┐
│   Engine boundary                                     │
│   createNCRes(gbModuleMatrix, max_level, strategy)    │
├──────────────────────────────────────────────────────┤
│   Computation                                         │
│   NCResComputation (subclass of ResolutionComputation)│
├──────────────────────────────────────────────────────┤
│   Substrate                                           │
│   NCAlgebras/FreeAlgebraQuotient                      │
│   matrix, matrix-con                                  │
└──────────────────────────────────────────────────────┘
```

## Entry point

```cpp
ResolutionComputation* createNCRes(const Matrix* gbModuleMatrix,
                                   int max_level,
                                   int strategy);
```

(declared in `nc-res-computation.hpp`, implemented in the
matching `.cpp`).

Takes:

- **`gbModuleMatrix`** — a module presentation matrix over an
  `M2FreeAlgebraQuotient` whose ideal has already been
  GB-reduced via [`NCAlgebras`](../NCAlgebras/architecture.md).
- **`max_level`** — how many resolution levels to compute.
- **`strategy`** — algorithm variant (currently a placeholder).

Returns a `ResolutionComputation*` (the common base from
[`comp-res.{cpp,hpp}`](../file-comp-res.md)). The interpreter
holds this and drives it.

## Why a separate subdir from `NCAlgebras/`

[`NCAlgebras/`](../NCAlgebras/architecture.md) builds the
**algebraic infrastructure** — free algebras, two-sided GBs,
suffix-tree-based overlap detection. Once you have a working
quotient with a normal-form algorithm, you can ask for *modules*
and their *resolutions*.

The resolution logic is conceptually separate — it operates *on
top of* the NC GB machinery, not as part of it. Splitting into
its own subdir mirrors the commutative split:

```
NCAlgebras/  ←→  groebner-bases.md, f4/, gb-f4/
NCResolutions/  ←→  resolutions.md, schreyer-resolution/
```

## Algorithmic differences vs commutative

The commutative Schreyer-frame trick
([`../schreyer-resolution/architecture.md`](../schreyer-resolution/architecture.md))
relies on **commutative monomials** to set up an order on free
modules that makes leading-term arithmetic trivial. The NC case:

- **Words instead of monomials.** Schreyer orders work, but with
  word-overlap-aware comparisons.
- **No degree exhaustion termination.** NC resolutions are
  typically infinite. The user provides a `max_level` bound; the
  computation stops when reached.
- **Larger free modules at each step.** NC syzygies have more
  generators than their commutative counterparts.

These differences mean NC resolution isn't just "commutative
resolution with different monomials" — it needs its own algorithm.

## Memory model

Inherited from the engine: GC-managed via
`our_new_delete`/`MutableEngineObject`
([`../file-hash.md`](../file-hash.md)). The
`FreeAlgebraQuotient` and `Matrix` references are held by
`mInputModuleGB`; they survive as long as the
`NCResComputation` does.

## Future work

The current implementation is an **early sketch** — the
constructor stores the inputs but the resolution algorithm
itself is still being filled in. The `notes.txt` file in this
directory tracks the design.

Pending:

- Computing level-0 (the identity) and level-1 (the input
  generators).
- Level-2+ via NC syzygy computation, possibly with NC-F4 sweeps
  reusing [`../NCAlgebras/file-NCF4.md`](../NCAlgebras/file-NCF4.md).
- Termination criteria beyond `max_level`.
- Betti table extraction for NC resolutions.

## Used by

- The `AssociativeAlgebras` user package's resolution paths.
- Researchers needing NC-Tor or NC-Ext computations.

## Related

- [`README.md`](README.md) — NCResolutions/ navigation hub.
- [`../architecture.md`](../architecture.md) — engine architecture.
- [`../NCAlgebras/architecture.md`](../NCAlgebras/architecture.md)
  — NC algebraic substrate this builds on.
- [`../schreyer-resolution/architecture.md`](../schreyer-resolution/architecture.md)
  — commutative analogue.
- [`../file-comp-res.md`](../file-comp-res.md) — `ResolutionComputation`
  base class.
- [`file-nc-res-computation.md`](file-nc-res-computation.md) —
  per-file deep dive.
- `notes.txt` (alongside) — author's design notes.
