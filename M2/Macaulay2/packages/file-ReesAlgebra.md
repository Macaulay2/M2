# `ReesAlgebra.m2` — Rees algebras & related blowup constructions

The `ReesAlgebra` package implements **Rees algebras**, **associated
graded rings**, **special fibers**, **analytic spreads**, **minimal
reductions**, and the suite of homological invariants that fall out
of them — `multiplicity`, `reductionNumber`, `distinguished`,
`whichGm`, `jacobianDual`. **Auto-loaded** — every M2 session has
these operations available without `needsPackage`.

The package implements the **module Rees algebra** of Huneke,
Eisenbud, and Ulrich ("What is the Rees algebra of a module?",
2003), which extends the classical ideal Rees algebra:

```
R(M) = Sym(M) / (the torsion of Sym(M) along a versal embedding)
```

For an ideal `I ⊂ R`, `R(I) = R[It] = ⊕ I^n t^n` as usual.

- File: `ReesAlgebra.m2` (2 661 lines — single big file, no
  auxiliary directory)
- Authors: David Eisenbud, Amelia Taylor, Sorin Popescu, Mike Stillman
- Version: 2.3 (November 2019)
- Certification: published in
  [JSAG vol. 8, 2018](https://msp.org/jsag/2018/8-1/p05.xhtml)
- Imported by: [`IntegralClosure`](file-IntegralClosure.md) (for
  `integralClosure(Ideal, …)`)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

### Rees-algebra constructions

```m2
reesIdeal(I, a)            -- the defining ideal of R[It] in R[T_0,…,T_n]
reesIdeal(M)               -- module Rees ideal (uses versalEmbedding)
reesAlgebra(I)             -- the Rees algebra R(I) as a quotient ring
symmetricKernel(f)         -- the kernel of Sym(presentation) (an Eisenbud-style ideal)
versalEmbedding(M)         -- a versal embedding of M into a free module
symmetricAlgebraIdeal(M)   -- the defining ideal of Sym(M)
jacobianDual(f)            -- the Jacobian dual matrix (used in Eisenbud-Huneke-Ulrich)
expectedReesIdeal(M)       -- conjectural form; useful when reesIdeal is too slow
```

### Associated graded ring & special fiber

```m2
associatedGradedRing(I)    -- gr_I(R) = ⊕ I^n / I^(n+1)
specialFiber(I)            -- R/m ⊗ R(I) (the special fiber)
specialFiberIdeal(I)       -- its defining ideal
```

### Invariants of the Rees algebra

```m2
analyticSpread(I)          -- dim(specialFiber I)
multiplicity(I)            -- Hilbert-Samuel multiplicity
reductionNumber(I, J)      -- the reduction number r_J(I)
minimalReduction(I)        -- a minimal reduction J ⊂ I (random choice)
isReduction(I, J)          -- is J a reduction of I?
isLinearType(I)            -- is R(I) = Sym(I)?
whichGm(I)                 -- "G_m" condition class (height of fitting ideals)
```

### Geometric applications

```m2
distinguished(I, J)        -- distinguished subvarieties of V(J) wrt I
intersectInP(...)           -- intersection theory primitive
PlaneCurveSingularities    -- helper symbol used in the test suite
```

### Options

| Option | Used by | What it controls |
|---|---|---|
| `Variable => sym` | `reesIdeal`, `symmetricKernel`, … | Name of the new variables `T_0, T_1, …` |
| `VariableBaseName` | `symmetricAlgebraIdeal` | Base name when building the symmetric algebra |
| `Strategy` | various | Algorithm selection |
| `Trim => true` | `reesIdeal` | Trim the result (default: yes) |

## The module Rees algebra idea

For an ideal `I ⊂ R`, the Rees algebra is the **graded ring of
powers**:

```
R(I) = ⊕_{n≥0} I^n t^n  ⊂  R[t]
```

For a **module** `M`, there's no obvious "powers" — `M^n` doesn't
make sense. The Huneke-Eisenbud-Ulrich solution:

1. Pick a **versal embedding** `M ↪ F` into a free module.
2. Form `Sym(M)` — the symmetric algebra (has all "powers" of `M`
   for free).
3. Take the **kernel of `Sym(M) → Sym(F) → R(image of M in F)`**.
4. The result is `R(M)`.

This kernel computation is what `symmetricKernel` does. It's
expensive but produces the right answer regardless of whether `M`
is an ideal, a free module, or something messier.

For ideals specifically, there are faster paths via Gröbner bases
of `(I, T_0 - f_0 t, …, T_n - f_n t)` and elimination of `t`.

## Why `reesIdeal` is the workhorse

Most Rees-algebra operations route through `reesIdeal`:

- `reesAlgebra(I)` is just `quotient(reesIdeal(I))`.
- `associatedGradedRing(I) = R(I) ⊗ R/I`.
- `specialFiber(I) = R(I) ⊗ R/m`.
- `analyticSpread(I) = dim(specialFiber(I))`.
- `multiplicity(I)` reads off the leading coefficient of the
  Hilbert polynomial of `associatedGradedRing(I)`.

So the practical performance of all these = the practical
performance of `reesIdeal` on the relevant input.

## `expectedReesIdeal` — when the real thing is too slow

For some inputs, `reesIdeal` does massive GB elimination work. The
package exports `expectedReesIdeal(M)` — a **conjectural form** of
the Rees ideal based on the symmetric algebra and the Jacobian
dual. It is **fast** but produces only the right answer when the
module satisfies the `G_m` condition (heuristically: when the
analytic spread is `< m`). Use it when:

- You know your module satisfies `G_m`.
- You want a fast upper bound on the Rees ideal.

When in doubt, compute both and compare. The package's test suite
includes cases where `expectedReesIdeal != reesIdeal`.

## The `distinguished` function

`distinguished(I, J)` returns the **distinguished components** of
`V(J)` with respect to `I` — irreducible components in the support
of the normal cone that play a role in intersection theory
(Fulton's *Intersection Theory*, §10). Implements an algorithm of
Eisenbud-Harris via Rees algebras.

This is the package's main link to **algebraic geometry**
(everything else is purely commutative algebra). Consumers in
intersection theory and characteristic classes call this.

## Dependencies and re-exports

ReesAlgebra:
- Imports nothing beyond Core.
- Is imported by `IntegralClosure` (for `integralClosure(Ideal, …)`).

This is unusual for an auto-loaded package — most have at least
one cross-package dependency. ReesAlgebra is **self-contained**
and depends only on engine primitives (Gröbner bases, ring
constructions, the symmetric-algebra primitive `symmetricAlgebra`).

## Single-file architecture

Unlike most large packages, ReesAlgebra has **no auxiliary
directory**. All 2 661 lines — including the entire test suite and
all documentation `doc ///…///` blocks — live in one file. This is
a stylistic choice from the original 2006 authorship that persists
because the package's internal structure is genuinely flat (no
strong sub-modules within it).

Search the single file by section comment for navigation:

| Section | Topic |
|---|---|
| Lines 80-120 | `symmetricAlgebraIdeal`, `symmetricKernel`, `versalEmbedding` |
| Lines 113-225 | `reesIdeal` (the workhorse, ~110 lines) |
| Lines 227-373 | `associatedGradedRing`, `multiplicity` |
| Lines 375-460 | `analyticSpread`, `distinguished`, `intersectInP` |
| Lines 462-560 | `minimalReduction`, `reductionNumber`, `whichGm` |
| Lines 564-650 | `jacobianDual`, `expectedReesIdeal` |
| Lines 650+ | Documentation `doc ///…///` and test `TEST ///…///` blocks |

## When this is slow

| Symptom | Try |
|---|---|
| `reesIdeal(I)` hangs | Pass a known reduction `J` as a hint: `reesIdeal(I, J)`; or check if `expectedReesIdeal` is good enough |
| `multiplicity(I)` slow | Computed via Hilbert polynomial of `associatedGradedRing` — boost via `Strategy => Symbolic` if available, or compute the Hilbert series directly |
| `distinguished(I, J)` very slow | Default is correct but exhaustive; pre-compute `reesIdeal(I)` once and let `distinguished` reuse it from the cache |
| `versalEmbedding(M)` slow | Use a custom embedding if you have one; pass it via the Rees-ideal options |

## See also

- [`file-IntegralClosure.md`](file-IntegralClosure.md) — primary downstream consumer; calls `reesIdeal` for ideal integral closure
- [`file-MinimalPrimes.md`](file-MinimalPrimes.md) — used internally for analyzing the distinguished components
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- Engine GB: [`e/groebner-bases.md`](../e/groebner-bases.md) — every `reesIdeal` call ends in a heavy GB computation
- Engine Hilbert: [`e/file-hilb.md`](../e/file-hilb.md) — backs `multiplicity` and `analyticSpread`
- [JSAG article](https://msp.org/jsag/2018/8-1/p05.xhtml) — Eisenbud-Stillman-Popescu-Taylor, "The ReesAlgebra package in Macaulay2"
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — computation-engine catalogue
