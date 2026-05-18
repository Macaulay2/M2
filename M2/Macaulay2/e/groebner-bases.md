# Gröbner bases (top-level files)

This page documents the Gröbner-basis machinery that lives at the **top level**
of `e/`. Standalone GB engines that live in their own subdirectories have
their own READMEs:

- [`f4/`](f4/README.md) — original F4
- [`gb-f4/`](gb-f4/README.md) — refactored F4
- [`bibasis/`](bibasis/README.md) — Boolean / involutive bases
- [`NCAlgebras/`](NCAlgebras/README.md) — non-commutative GB

There is also an external GB option, **mathicgb** (vendored as a submodule),
reached via `mathicgb-interface.{cpp,hpp}`.

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e) · [per-area docs](README.md#top-level-files-per-area-docs)

## The Computation framework

| File pair | Purpose |
|---|---|
| `comp.{cpp,hpp}` | Generic abstract base — `start`, `step`, `status`, `stop_conditions`. **Deep dive:** [`file-computation-framework.md`](file-computation-framework.md) |
| `comp-gb.{cpp,hpp}` | GB Computation subclass — dispatches to a specific algorithm. **Deep dive:** [`file-comp-gb.md`](file-comp-gb.md) |
| `comp-gb-declared.{cpp,hpp}` | "Declared" GB — pre-supplied basis used to bootstrap |
| `comp-gb-proxy.{cpp,hpp}` | Proxy used when the GB engine runs in a separate thread / supervisor task |

A Computation is **resumable**: the interpreter starts it, may pause and
inspect partial state, set stop conditions (degree limit, basis size, time),
then resume. The interpreter side of this protocol is in
[`d/`](../d/README.md).

## GB algorithms (legacy / general-purpose)

| File pair | Algorithm |
|---|---|
| `gb-default.{cpp,hpp}` | Buchberger-style "default" algorithm — workhorse for general rings. **Deep dive:** [`file-gb-default.md`](file-gb-default.md) |
| `gb-homog2.{cpp,hpp}` | Homogeneous specialisation. **Deep dive:** [`file-gb-variants.md`](file-gb-variants.md) |
| `gb-sugarless.{cpp,hpp}` | A "sugarless" variant of Buchberger. See [`file-gb-variants.md`](file-gb-variants.md) |
| `gb-toric.{cpp,hpp}` | Specialised algorithm for toric ideals. See [`file-gb-variants.md`](file-gb-variants.md) |
| `gb-walk.{cpp,hpp}` | Gröbner walk — convert a GB w.r.t. one order to another. See [`file-gb-variants.md`](file-gb-variants.md) |

## Reduced GB representations

After a GB is computed, the engine canonicalises it to a *reduced* form:

| File pair | Coefficient context |
|---|---|
| `reducedgb.{cpp,hpp}` | Abstract base. **Deep dive:** [`file-reducedgb.md`](file-reducedgb.md) |
| `reducedgb-field.{cpp,hpp}` | Coefficients in a field |
| `reducedgb-field-local.{cpp,hpp}` | Local-ring case |
| `reducedgb-ZZ.{cpp,hpp}` | Coefficients in ZZ |
| `reducedgb-marked.{cpp,hpp}` | Pre-marked leading terms — used when the GB is supplied by the user |

## Supporting machinery

| File pair | Purpose |
|---|---|
| `gbring.{cpp,hpp}` | A polynomial-ring view tuned for GB arithmetic (fast head/tail decomposition). **Deep dive:** [`file-gbring.md`](file-gbring.md) |
| `gbweight.{cpp,hpp}` | Weight-vector tracking during GB (used for early-exit by degree). **Deep dive:** [`file-gbweight.md`](file-gbweight.md) |
| `spair.{cpp,hpp}` | S-pair data structure: the unit of work in Buchberger-style algorithms. **Deep dive:** [`file-spair.md`](file-spair.md) |
| `mathicgb-interface.{cpp,hpp}` | Bridge to the [`mathicgb`](../../submodules/README.md) submodule library. **Deep dive:** [`file-mathicgb-interface.md`](file-mathicgb-interface.md) |

## Choosing an algorithm

The engine doesn't always pick the same algorithm:

- Boolean (`F_2[x]/(x_i^2-x_i)`) ideals → [`bibasis/`](bibasis/README.md)
- Non-commutative → [`NCAlgebras/`](NCAlgebras/README.md)
- Generic commutative ring → `gb-default`, or F4 (one of [`f4/`](f4/README.md)
  / [`gb-f4/`](gb-f4/README.md)), or mathicgb, depending on user request and
  ring shape

The dispatching logic lives in `comp-gb.cpp` and is driven from the M2 layer
([`m2/gb.m2`](../m2/README.md)) through the public interface in
[`interface/groebner.{h,cpp}`](interface/README.md).

## M2 strategy → engine algorithm

What M2 users type when calling `gb` and where the actual computation happens:

| M2 expression | Engine algorithm | Source file | Notes |
|---|---|---|---|
| `gb I` (default) | `GBDefault` | `gb-default.{cpp,hpp}` | Buchberger with sugar; the workhorse |
| `gb(I, Strategy => Homogeneous2)` | `GBhomog2` | `gb-homog2.{cpp,hpp}` | Homogeneous specialisation; uses graded structure to skip degree-tracking |
| `gb(I, Strategy => Sugarless)` | `GBSugarless` | `gb-sugarless.{cpp,hpp}` | Buchberger without sugar; for the rare case sugar hurts more than it helps |
| `gb(I, Algorithm => Toric)` | `GBToric` | `gb-toric.{cpp,hpp}` | Specialised for toric (binomial) ideals |
| `gb(I, Algorithm => Walk, ...)` | `GBWalk` | `gb-walk.{cpp,hpp}` | Gröbner walk: convert GB w.r.t. one order to another |
| `gb(I, Algorithm => LinearAlgebra)` | F4-style: `F4Computation` | [`f4/F4Computation.hpp`](f4/file-f4-computation.md) | Original F4 engine |
| `gb(I, Algorithm => LinearAlgebra, Strategy => NewF4)` | Refactored F4: `GBF4Computation` | [`gb-f4/file-GBF4Computation.md`](gb-f4/file-GBF4Computation.md), [`gb-f4/file-GBF4Interface.md`](gb-f4/file-GBF4Interface.md) | Cleaner separation of concerns |
| `gb(I, Algorithm => MathicGB)` | mathicgb-driven GB | `mathicgb-interface.{cpp,hpp}` | Calls the mathicgb submodule |
| `gb I` over `F_2[x_i]/(x_i^2-x_i)` | `BIBasis` (involutive) | [`bibasis/bibasis.{cpp,hpp}`](bibasis/file-bibasis.md) | Specialised for Boolean rings; auto-selected by ring shape |
| `gb I` in a non-commutative ring | `NCGroebner` | [`NCAlgebras/NCGroebner.{cpp,hpp}`](NCAlgebras/file-NCGroebner.md) | NC algebra GB; auto-selected by ring shape |
| `gb(I, ChangeMatrix => true)` | adds matrix-tracking to any of the above | `comp-gb.cpp` | Returns the matrix expressing the GB in terms of original generators |
| `forceGB I` | `GBDeclared` (no computation, just verify) | `comp-gb-declared.{cpp,hpp}` | Accept user-supplied basis; verifies leading terms only |
| `gb(I, Stop => {…})` | any of the above with stop conditions | `comp.{cpp,hpp}` framework | Bound by `DegreeLimit`, `BasisElementLimit`, `PairLimit`, `CodimensionLimit`, …; resumable |

Construction routes through:

```
M2:  gb(I, Algorithm => LinearAlgebra)
   ↓
m2/gb.m2  →  rawGB(I.generators, ..., flag-bits)
   ↓
d/interface.dd  →  Ccode(RawComputationOrNull, "IM2_GB_make(...)")
   ↓
e/interface/groebner.h  →  IM2_GB_make(matrix, *, strategy, ...)
   ↓
e/comp-gb.cpp  dispatcher:
   if Boolean ring  → bibasis/bibasis  (BIBasis)
   if NC ring       → NCAlgebras/NCGroebner
   else by strategy → gb-default | f4/F4Computation | gb-f4/GBF4Computation |
                      mathicgb-interface | gb-homog2 | gb-toric | gb-walk | …
   ↓
returned as Computation* (resumable, stoppable)
```

## Choosing a GB algorithm

Practical guidance for which `Algorithm =>` / `Strategy =>` to pick:

| Want | Pick |
|---|---|
| Default workflow, any ring | **Don't pass anything** — the default heuristic picks `gb-default` for most inputs and auto-routes to specialised engines for Boolean / NC |
| Very dense input, mod-p coefficients | `Algorithm => LinearAlgebra` (F4); typically 2–10× faster |
| Newer F4 with cleaner code paths | `Algorithm => LinearAlgebra, Strategy => NewF4` (gb-f4) |
| Toric / binomial ideal | `Algorithm => Toric`; specialised path 100× faster than generic |
| Boolean polynomial ring (`F_2[x_i]/(x_i² - x_i)`) | No flag needed; **auto-selected** via [`bibasis/`](bibasis/README.md) |
| Non-commutative algebra | No flag needed; **auto-selected** via [`NCAlgebras/`](NCAlgebras/README.md) |
| Change-of-order from `Lex` to `GRevLex` (or vice versa) | `Algorithm => Walk`; faster than recomputing |
| Comparison / regression testing | Pin `Algorithm => Homogeneous2` (the homogeneous workhorse) for reproducibility |
| Resumable / bounded computation | Pass `StopBeforeComputation => true` then `gb(I, …, Stop => {DegreeLimit => 5, …})` |
| Stand-alone external benchmark | `Algorithm => MathicGB`; uses the mathicgb library for comparison with academic baselines |

When `gb I` hangs:

1. **First**, try `Algorithm => LinearAlgebra` — F4 is usually faster on hard inputs.
2. **If that hangs**, try `Algorithm => MathicGB` — sometimes the algorithmic differences matter.
3. **Try a degree bound**: `gb(I, DegreeLimit => 5)` — partial GB returned in seconds.
4. **Trace it**: `gbTrace = 3` then `gb I` — prints per-step progress to stderr.
5. **Profile it**: `time gb I` shows where the time is going.

## Related

- [`resolutions.md`](resolutions.md) — resolutions drive a GB at each step.
- [`monoids-and-monomials.md`](monoids-and-monomials.md) — monomial orderings
  drive GB.
- [`polynomial-rings.md`](polynomial-rings.md) — what GBs live in.
- mathicgb submodule under [`submodules/`](../../submodules/README.md).
