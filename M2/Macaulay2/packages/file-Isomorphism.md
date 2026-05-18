# `Isomorphism.m2` — probabilistic module-isomorphism testing

The `Isomorphism` package implements `isIsomorphic(N, M)` —
**probabilistically test whether two modules are isomorphic** — and
the constructive companion `isomorphism(N, M)`, which returns a
concrete isomorphism matrix when one exists. **Auto-loaded** —
every M2 session has these operations available without
`needsPackage`.

Module isomorphism testing is harder than it sounds: two modules can
have identical presentations and yet not be isomorphic (over a
non-trivial ring), or can have wildly different presentations and
yet be isomorphic. The package's approach is **probabilistic**:
generate random homomorphisms in both directions and check whether
they compose to identities. Wrong answers are possible in principle
but extremely unlikely for any reasonable retries budget.

- File: `Isomorphism.m2` (880 lines — single file, no aux dir)
- Authors: David Eisenbud, Mahrud Sayrafi
- Version: 2.0 (April 30, 2025)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

```m2
isIsomorphic(N, M)       -- Boolean: is N ≅ M?
isIsomorphic(n, m)       -- shortcut: isIsomorphic(coker n, coker m)
isomorphism(N, M)        -- the actual isomorphism map (Matrix); error if none
checkDegrees(A, B)       -- (Sequence) consistency check on degree vectors
```

### Options (shared between `isIsomorphic` and `isomorphism`)

| Option | Default | What it does |
|---|---|---|
| `Homogeneous => true` | true | Require a homogeneous isomorphism; set `false` to allow inhomogeneous candidates |
| `Strict => false` | false | If true, demand exact equality of degree vectors (no twist allowed). If false, allow `N ≅ M(d)` for some shift `d` |
| `Verbose => false` | false | Print diagnostic output about each random-map attempt |
| `Strategy => null` | null | Hook for future strategy selection (currently the algorithm is fixed) |
| `Tries => null` | null | Bound on the number of random-map attempts; default chosen by the package |

## How `isIsomorphic` works

The core idea (from `isIsomorphic(Module, Module)` near line 223):

1. **Fast paths first**: if `M === N` (same object) or a previously
   computed isomorphism is cached, return true immediately.
2. **Free-module case**: if both are free, isomorphism iff they
   have matching ranks and degree multisets — checked with
   `checkDegrees` and a sort.
3. **Degree check**: `checkDegrees(M, N)` returns the offset shift
   `d` such that `M` and `N(d)` have matching betti numbers in low
   degrees. If no such shift exists, return false. If `Strict =>
   true`, require `d == 0`.
4. **Random-map attempts**: generate random homomorphisms `f : M
   → N(d)` and `g : N(d) → M` of degree 0; check whether either
   composition is a unit times identity. The randomisation is via
   `randomMinimalDegreeHomomorphism`.
5. **Caching**: on success, store the isomorphism under
   `Y.cache.cache.Isomorphisms#(N, M, {Strict, Homogeneous})`
   where `Y` is the younger (by hash) of `M` and `N`. Future
   `isIsomorphic(M, N)` calls hit the cache.

The asymmetric handling of cache and "younger" object avoids
double-caching the same fact under both `(N, M)` and `(M, N)`
keys.

## The probabilistic gap

Two modules over a non-trivial ring may have identical Hilbert
functions, identical betti tables, identical free resolutions, and
**still** not be isomorphic. The package's random-map test detects
this when an isomorphism exists (with high probability per try) but
**cannot prove non-isomorphism**. If `isIsomorphic` returns `false`
after `Tries` attempts:

- If the modules are not isomorphic, this is correct.
- If they happen to be isomorphic but the random maps all missed,
  this is wrong.

The default `Tries` is set so that the probability of a missed
isomorphism is negligible for the kinds of modules M2 users typically
deal with. To increase confidence, raise `Tries` and re-run.

The package's docstring is explicit about this trade-off.

## `isomorphism` — get the matrix

When you need the actual isomorphism, not just its existence:

```m2
phi = isomorphism(N, M)
target phi == N    -- true
source phi == M    -- true
isIsomorphic phi   -- true (sanity check, very cheap)
```

`isomorphism` shares options with `isIsomorphic` and shares the
cache. If `isIsomorphic` has already returned true for the same
inputs, `isomorphism` returns the cached map without re-computing.

If no isomorphism exists, `isomorphism` raises an error — use
`isIsomorphic` first if you don't know.

## `checkDegrees` — the degree-shift detector

A precondition checker exported for direct use:

```m2
checkDegrees(M, N)
  -- returns (d, status) where:
  --   d = candidate degree shift (or null)
  --   status = (Boolean, message) describing the check's verdict
```

Useful when you want to determine whether `M` and `N` could
possibly be isomorphic up to twist, without committing to the
expensive `isIsomorphic` test. Skip it for `Strict => true` (then
only `d = 0` is acceptable anyway).

## When this is slow

| Symptom | Try |
|---|---|
| `isIsomorphic(N, M)` slow on small modules | Likely `M` and `N` aren't isomorphic; the package tries many candidates. Set `Tries => 5` to fail fast (at the cost of a slightly higher false-negative rate) |
| `isIsomorphic` returns false for modules you know are isomorphic | Raise `Tries` (e.g. `Tries => 100`); or check `Homogeneous => false` if the modules are not graded the same way |
| `isomorphism` errors with "no isomorphism found" | The map exists with low probability per attempt; either raise `Tries` or accept that the modules likely aren't isomorphic |
| Very large modules | Random-map generation is the bottleneck — usually quick, but for thousands of generators each attempt is slow |

## Why probabilistic, not deterministic

Deterministically deciding module isomorphism is, in general, as
hard as deciding **isomorphism of polynomial-ring quotients** —
which subsumes the **graph isomorphism problem** for monomial
ideals. So no efficient deterministic algorithm is known. The
probabilistic approach is sound (false positives are essentially
impossible because the check verifies composition gives the
identity) and fast in the common case.

For provably-correct isomorphism testing under specific
hypotheses (free modules, specific ring shapes, monomial ideals),
use the targeted routines elsewhere in the distribution:

- Free modules: trivially decided by rank + degree multisets.
- Monomial ideals up to permutation:
  [`MonomialOrbits`](MonomialOrbits.m2) (not auto-loaded) or
  specialised packages.
- Over a field: rank + degrees are a complete invariant.

## Where it sits in the ecosystem

| Use case | Use |
|---|---|
| Deciding if `M ≅ N` for general modules | This package |
| Building modules and testing equivalence | This package |
| Iso of free modules only | Inline `rank M == rank N and sort degrees M == sort degrees N` |
| Iso of cohomology modules | Possible (they're just modules) |
| Iso of rings | Different problem; not handled here |

## Auxiliary functions (internal, not exported)

The file defines `randomMinimalDegreeHomomorphism`,
`leadCoefficient'`, `reduceCoefficient`, and a `hasIsomorphism`
cache-check helper. They're all internal but well-commented and
inspectable — search for the `method()` calls in the file.

## Single-file architecture

The 880-line file contains:

| Section | Topic |
|---|---|
| Lines 30-45 | `leadCoefficient'`, `reduceCoefficient` (Core extensions) |
| Lines 48-100 | `randomMinimalDegreeHomomorphism` (the random-map generator) |
| Lines 100-200 | `checkDegrees` (the degree-shift detector) |
| Lines 214-300 | `isIsomorphic` and `isomorphism` (the core API) |
| Lines 300+ | Cache management, `doc ///…///`, and `TEST ///…///` blocks |

## See also

- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [`file-Complexes.md`](file-Complexes.md) — many module-isomorphism tests come up while building resolutions
- [`file-Varieties.md`](file-Varieties.md) — sheaf isomorphism is module isomorphism after `module F`
- Engine modules: [`e/free-modules.md`](../e/free-modules.md), [`e/matrices.md`](../e/matrices.md)
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
