# `Saturation.m2` — quotient, saturation & annihilator

The `Saturation` package implements `saturate`, `quotient` (ideal /
module colon), and `annihilator`. **Auto-loaded** — available in every
M2 session without `needsPackage`. Sibling to
[`MinimalPrimes`](file-MinimalPrimes.md) and
[`PrimaryDecomposition`](file-PrimaryDecomposition.md); together
these three implement the algebraic-decomposition core.

- Main file: `Saturation.m2` (903 lines)
- Auxiliary directory: `Saturation/` (11 files, 1177 lines)
- Authors: Justin Chen, Mahrud Sayrafi, Mike Stillman
- Re-exports: HomologicalAlgebraPackage, [`Elimination`](Elimination.m2)
- Date: November 2021

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

```m2
saturate(I, J)             -- the saturation I : J^∞
saturate(M, J)             -- same, for modules
quotient(I, J)             -- the ideal quotient I : J
quotient(M, N)             -- same, for modules
annihilator M              -- Ann(M)
isSupportedInZeroLocus(M, I)  -- is M supported only on V(I)?
```

`saturate` and `annihilator` are re-exported from `Core` (their
M2-level method-stubs live in `m2/`); this package provides the
actual algorithm implementations and registers them via hooks.

## Strategy / hook architecture

This package is a **case study in M2's `addHook` pattern**. For
each (operation, type1, type2) triple, the package maintains a
table of algorithms keyed by strategy:

```m2
algorithms#(quotient, Ideal, Ideal) = {
    Iterate  => ..., Quotient => ...,
    Linear   => ..., Monomial => ...,
}
scan(strategies, strategy ->
    addHook(key, algorithms#key#strategy, Strategy => strategy))
```

`runHooks` iterates the registered strategies in order, returning
the first non-`null` result. Each strategy:

- Inspects the inputs.
- Returns `null` if its assumptions aren't met (e.g. `Monomial`
  strategy returns `null` unless both ideals are monomial).
- Returns the answer otherwise.

This lets new strategies (e.g. from add-on packages) be **injected
without modifying** `Saturation.m2`.

## Strategy catalogue

The complete set of strategy names used across the 8 hook tables:

| Strategy | Where it applies | What it does |
|---|---|---|
| `Iterate` | quotient / saturate | Compute `(I : J^n)` iteratively until stable; the safe fallback |
| `Quotient` | quotient(Ideal, Ideal) | Syzygy-based: `syz gb(J transpose ⊗ R/I)` |
| `Linear` | quotient(Ideal, Ideal) | (declared but currently a no-op warning; placeholder for linear-form optimisation) |
| `Monomial` | both monomial inputs | Hand off to the engine's [`monideal`](../e/file-monideal.md) (`rawColon`) — very fast |
| `Eliminate` | saturate | Reduce saturation to an elimination ideal in an enlarged ring |
| `Bayer` | saturate | Bayer's algorithm using a generic element |
| `GRevLex` | saturate | Specialised path requiring graded-reverse-lex ordering |

The default strategy depends on input shape; `Strategy => null`
picks; explicit strings select a specific path. The `Unused`
strategy in `saturate(Ideal, RingElement)` is a placeholder for
unimplemented input shapes.

## Algorithms in the auxiliary directory

| File | Role |
|---|---|
| `saturate3.m2`, `saturate4.m2`, `saturate5.m2` | Experimental saturation algorithms — kept around for benchmarking |
| `badsaturations.m2` | Test cases that historically defeated specific strategies |
| `doc.m2`, `saturate-doc.m2`, `quotient-doc.m2`, `annihilator-doc.m2` | Documentation broken out by topic (4 doc files for clean organisation) |
| `saturate-test.m2`, `quotient-test.m2`, `annihilator-test.m2` | Per-operation test suites |

The main `Saturation.m2` file holds all the production algorithms;
the aux dir is dominated by **docs** (`doc.m2` + 3 topic-doc files =
432 lines) and **tests** (477 lines).

## Computation caching

This package uses M2's `Computation` / `Context` cache machinery:

```m2
importFrom_Core { "isComputationDone", "cacheComputation",
                  "fetchComputation", "updateComputation",
                  "cacheHit", "Context", "Computation" }
```

When you call `saturate(I, J, DegreeLimit => 5)` then later
`saturate(I, J, DegreeLimit => 10)`, the second call resumes from
the cached partial state of the first. The same mechanism backs
`gb` / `resolution` resumption (see
[`m2/file-gb.md`](../m2/file-gb.md) for the parallel pattern).

## Boundary with sibling packages

| Package | Relationship |
|---|---|
| [`MinimalPrimes`](file-MinimalPrimes.md) | Independent — neither imports the other; users may invoke both |
| [`PrimaryDecomposition`](file-PrimaryDecomposition.md) | **Re-exports `Saturation`** — primary decomposition algorithms call `saturate` internally |
| [`Elimination`](Elimination.m2) | **Imported** by `Saturation` for the `Eliminate` and `Bayer` strategies |
| [`HomologicalAlgebraPackage`](Complexes.m2) (the `HomologicalAlgebraPackage` global is set in `Core.m2` to point at `Complexes` by default) | **Imported** for `Ext` / `Hom` used in some annihilator paths |

## Engine integration

The fast paths bottom out at engine code:

| Strategy | Engine entry |
|---|---|
| `Monomial` (ideal colon) | `rawColon` → [`e/file-monideal.md`](../e/file-monideal.md) (`MonomialIdeal::quotient`) |
| `Quotient` (via `syz gb`) | [`e/groebner-bases.md`](../e/groebner-bases.md) + [`e/file-comp-gb.md`](../e/file-comp-gb.md) |
| `Eliminate` | Engine elimination = a GB w.r.t. an elimination order; see [`e/file-monordering.md`](../e/file-monordering.md) |

## When this is slow

| Symptom | Try |
|---|---|
| `saturate(I, J)` hangs on a small example | `Strategy => Eliminate` or `Strategy => Bayer` |
| `quotient` slow on monomial ideals | Ensure both ideals are recognised as `MonomialIdeal` — only then does the `Monomial` strategy fire |
| Iterate strategy goes forever | Pass `DegreeLimit => N` to bound the iteration depth |
| Annihilator very slow | Try `annihilator(M, Strategy => Intersection)` instead of the default Ext-based path |

## See also

- [`file-MinimalPrimes.md`](file-MinimalPrimes.md) — sister auto-loaded package
- [`file-PrimaryDecomposition.md`](file-PrimaryDecomposition.md) — sister auto-loaded package; re-exports this one
- [`Elimination.m2`](Elimination.m2) — imported for elimination-based saturation strategies
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [Engine monomial-ideal ops: `e/file-monideal.md`](../e/file-monideal.md) — backs the `Monomial` strategy
- [Engine GB: `e/groebner-bases.md`](../e/groebner-bases.md) — backs the `Quotient` strategy
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — computation-engine catalogue
