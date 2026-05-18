# Resolutions (top-level files)

A **free resolution** of a module `M` is an exact complex of free modules
`… → F_2 → F_1 → F_0 → M → 0`. Computing it is one of the headline operations
in commutative algebra and one of the most expensive things M2 does.

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e) · [per-area docs](README.md#top-level-files-per-area-docs)

## Two implementations

The modern, F4-style implementation lives in its own subdirectory:

- [`schreyer-resolution/`](schreyer-resolution/README.md) — Schreyer-frame
  resolution with F4 reductions. Used for most workloads.

The older series of implementations lives at the top level of `e/`
and is **collectively covered by one deep dive**: [`file-res-old.md`](file-res-old.md).

| File pair / triple | Era | Notes |
|---|---|---|
| `res-a0.{cpp,hpp}`, `res-a0-pair.hpp`, `res-a0-poly.{cpp,hpp}` | "Generation 0" | First-generation resolution code, retained for compatibility |
| `res-a1.{cpp,hpp}`, `res-a1-poly.{cpp,hpp}` | "Generation 1" | Second-generation; some performance improvements |
| `res-a2.{cpp,hpp}`, `res-a2-gb.cpp` | "Generation 2" | Drives an internal GB engine per homological step |
| `Eschreyer.{cpp,hpp}` | Schreyer-style | Predecessor to [`schreyer-resolution/`](schreyer-resolution/README.md). **Deep dive:** [`file-Eschreyer.md`](file-Eschreyer.md) |

These older files are still built and selectable from M2 via the
`Strategy => …` option to `resolution`. They are useful both for regression
testing and for the (rare) cases where the modern implementation is slower on
a given input.

## Strategy selection

From M2:

```m2
resolution(M, Strategy => 0)   -- res-a0 (gen-0)
resolution(M, Strategy => 1)   -- res-a1 (gen-1)
resolution(M, Strategy => 2)   -- res-a2 (gen-2)
resolution(M, Strategy => 3)   -- Eschreyer
resolution(M, Strategy => 4)   -- schreyer-resolution/ (modern F4-style, default)
```

The dispatch logic lives in [`file-comp-res.md`](file-comp-res.md).

The complete cross-engine catalogue (with input characteristics,
relative-speed table, and a "when to use which" decision tree) is in
[`../../../COMPUTATIONS.md`](../../../COMPUTATIONS.md).

The non-commutative analogue lives in
[`NCResolutions/`](NCResolutions/README.md).

## Computation glue

| File pair | Purpose |
|---|---|
| `comp-res.{cpp,hpp}` | Resolution Computation — dispatches to one of the implementations above. **Deep dive:** [`file-comp-res.md`](file-comp-res.md) |
| `betti.{cpp,hpp}` | Betti table extraction (the standard human-readable summary of a resolution). **Deep dive:** [`file-betti.md`](file-betti.md) |

## Anatomy of a Schreyer-frame resolution

1. Start with a presentation `M = R^a / im(f : R^b → R^a)`.
2. Build a **frame**: the syzygies of `f` (computed via GB) become the
   generators of `R^c → R^b`, etc.
3. At each step, the frame supplies a Schreyer order on the new free module
   (see [`free-modules.md`](free-modules.md)). The leading-term arithmetic
   becomes local to the homological degree.
4. F4 reductions ([`schreyer-resolution/`](schreyer-resolution/README.md))
   process all syzygies in a given degree in one Macaulay-matrix sweep.

## Related

- [`schreyer-resolution/`](schreyer-resolution/README.md) — modern implementation.
- [`NCResolutions/`](NCResolutions/README.md) — non-commutative case.
- [`groebner-bases.md`](groebner-bases.md) — GB engines drive each step.
- [`free-modules.md`](free-modules.md) — Schreyer orders live here.
- [`interface/groebner.{h,cpp}`](interface/README.md) — exposes both GB and
  resolution entry points.
