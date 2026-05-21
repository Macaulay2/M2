# `MinimalPrimes.m2` — minimal primes & radicals

The `MinimalPrimes` package implements three of the most-used M2
operations on ideals: `minimalPrimes I`, `radical I`, and `isPrime I`.
It is auto-loaded — every M2 session has it available without an
explicit `needsPackage`. Companion-and-successor to the older
[`PrimaryDecomposition`](file-package-conventions.md) package.

- Main file: `MinimalPrimes.m2` (566 lines — orchestration + exported API)
- Auxiliary directory: `MinimalPrimes/` (15 files, 4614 lines —
  algorithm implementations and tests)
- Authors: Frank Moore, Mike Stillman, Franziska Hinkelmann, Justin
  Chen, Mahrud Sayrafi
- Package imports: [`Elimination`](Elimination.m2)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

```m2
minimalPrimes I          -- the minimal primes of an ideal
minprimes I              -- alias for minimalPrimes
radical I                -- the radical √I
radicalContainment(f, I) -- is f ∈ √I ?
isPrime I                -- (override of the base method)
installMinprimes()       -- promote this implementation to be primary
```

Plus the strategy keyword `Hybrid`.

## Architecture

```
MinimalPrimes.m2                            ← main entry / dispatch
   │
   ▼
AnnotatedIdeal.m2     ← invariant data structure: an ideal +
                        what's known about it (square-free, monic-LT,
                        already-radical, …) — the key abstraction
   │
   ▼
splitIdeals.m2        ← the iterative splitting algorithm: take an
                        AnnotatedIdeal, apply a heuristic to split
                        into "easier" ideals
   │     ▲
   │     │
   │     └─── PDState.m2 — bookkeeping for the splitting loop
   ▼
factorTower.m2        ← inner kernel: factor over the residue tower
                        produced by splitting
   │
   ▼
quickGB.m2            ← shortcut GB computation tuned for the
                        ideals that splitIdeals produces
   │
   ▼
radical.m2            ← radical-specific orchestration (built on top
                        of the splitting machinery)
```

The cross-cutting `doc.m2` and `tests.m2` files contribute
documentation and test cases.

## The splitting idea

Computing minimal primes by a single global factorisation /
elimination is expensive. The package's strategy is:

1. Take the input ideal `I` (wrapped in an `AnnotatedIdeal` so we
   track what we've discovered about it).
2. Apply a **heuristic split**: identify a polynomial in `I` that
   factors, and use the factorisation to split `I` into a list of
   simpler `AnnotatedIdeal`s.
3. Some of the resulting pieces will be visibly prime (e.g.
   linear-in-some-variable, or already in a known good form). Set
   those aside.
4. Recurse on the rest.
5. Stop when every annotated piece is in a form we can verify is
   prime.

The intersection of the resulting primes is the radical (or the
collection of minimal primes, depending on which method invoked
the loop).

The **invariant** maintained through the splitting:

> `radical(I) = intersection(annotated pieces collected so far)`

Comments in `MinimalPrimes.m2` flag this as DESIRED but the exact
form depends on the strategy chosen.

## Strategy system

The dispatcher works through `runHooks`:

```m2
runHooks((minimalPrimes, Ideal), (opts, I), Strategy => opts.Strategy)
```

Each registered hook implements a strategy; the first one whose
"assumptions are met" succeeds. Hooks include:

- `Birational` — exported for direct use, splits using birational
  isomorphism arguments
- `Hybrid` — the default; combines several heuristics
- Plus per-shape specialisations (monomial ideals, binomial ideals,
  …) registered by other packages (e.g. `Binomials` adds
  `binomialMinimalPrimes`).

The `installMinprimes()` exported function promotes this
implementation over the legacy one in `PrimaryDecomposition`. It
also exists for backward compatibility — most users no longer need
it.

## Auxiliary file roles

| File | Role |
|---|---|
| `AnnotatedIdeal.m2` (257 lines) | The data structure that travels through the splitting algorithm — an ideal plus discovered metadata |
| `splitIdeals.m2` (670 lines) | The iterative splitting loop, the largest aux file |
| `factorTower.m2` (256 lines) | Factoring polynomials over a tower of residue fields (used by inner split heuristics) |
| `quickGB.m2` (160 lines) | A GB shortcut for the kinds of ideals splitIdeals produces |
| `radical.m2` (435 lines) | Radical-specific entry points |
| `PDState.m2` (66 lines) | State / bookkeeping for the splitting loop |
| `doc.m2` (309 lines) | Documentation (M2 doc DSL) for the exported API |
| `tests.m2` (1 931 lines) | The main test suite (`check "MinimalPrimes"`) |
| `*-test.m2` (six files) | Topic-specific test suites (decompose1–5, minprimes, radical) — split out for faster iteration |

## How it dispatches

Because `MinimalPrimes` ships with M2 and is auto-loaded, the
`minimalPrimes Ideal` method that users call resolves immediately to
this package's implementation. The legacy fallback in
`PrimaryDecomposition` is still available via `Strategy => "Legacy"`
or by not auto-loading this package.

## When this is slow

- **Very large ambient rings** — splitting heuristics often elaborate
  the ring with new variables; ambient-ring size matters.
- **Ideals with many similar generators** — splitting finds few
  factor candidates, falls through to expensive paths.
- **Coefficient rings other than `QQ` or `ZZ/p`** — the underlying
  factorisation depends on the coefficient ring; over a general
  base ring the package may not converge.

For these cases, try `Strategy => null` (autopick), then specific
strategies one at a time to see which makes progress.

## See also

- [`file-package-conventions.md`](file-package-conventions.md) — the conventions every M2 package follows
- [`PrimaryDecomposition.m2`](PrimaryDecomposition.m2) — full primary decomposition, builds on minimal primes
- [`Saturation.m2`](Saturation.m2) — uses `radical` for some
  saturation strategies
- [Engine-side associated-primes computation: `e/file-assprime.md`](../e/file-assprime.md)
- [Engine-side `MonomialIdeal` ops: `e/file-monideal.md`](../e/file-monideal.md) (where monomial-ideal radicals/minimalPrimes shortcut)
- [`Elimination.m2`](Elimination.m2) — the package this one imports
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — full computation-engine catalogue
