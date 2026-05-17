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

## Related

- [`resolutions.md`](resolutions.md) — resolutions drive a GB at each step.
- [`monoids-and-monomials.md`](monoids-and-monomials.md) — monomial orderings
  drive GB.
- [`polynomial-rings.md`](polynomial-rings.md) — what GBs live in.
- mathicgb submodule under [`submodules/`](../../submodules/README.md).
