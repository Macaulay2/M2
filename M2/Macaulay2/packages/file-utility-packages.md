# Small utility packages — `Classic`, `ConwayPolynomials`, `OnlineLookup`, `PackageCitations`, `TangentCone`

Five **small auto-loaded utility packages** that each add a focused
capability to every M2 session. Documented together because their
APIs are tiny (one or two exported functions each) and they don't
warrant individual ~150-line deep dives. The trio they complement
(MinimalPrimes, PrimaryDecomposition, Saturation,
Elimination, Complexes, SimpleDoc, Varieties, IntegralClosure,
ReesAlgebra, LLLBases, InverseSystems, Isomorphism) already have
their own files.

| Package | Lines | Exports | Purpose |
|---|---|---|---|
| `Classic` | 171 | `poly` | Parser for the classic-Macaulay polynomial syntax |
| `ConwayPolynomials` | 116 | `conwayPolynomial` | Database backing `GF(q)` |
| `OnlineLookup` | 158 + aux | `oeis`, `isc` | OEIS / Inverse Symbolic Calculator lookups |
| `PackageCitations` | 328 | `cite` | Generate BibTeX entries for citing M2 packages |
| `TangentCone` | 84 | `tangentCone` | Tangent cone of an ideal at the origin |

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

---

## `Classic.m2` — the classic-Macaulay polynomial parser

```m2
poly "x2y - 3xz3 + 2y2z"
  -- ≡ x^2*y - 3*x*z^3 + 2*y^2*z   in the current ring
```

The original Macaulay (1976-1996) used a compact polynomial
syntax — no explicit `*` between adjacent letters, no `^` for
exponents (a small integer right next to a variable was the
exponent). `Classic` revives that syntax for M2 users who learned
it pre-Macaulay 2.

- Built on the [`Parsing`](Parsing.m2) combinator library (small
  utility for grammar specification).
- Single function `poly`, accepts a string, returns a
  `RingElement` in the current ring.
- Useful for **transcribing examples from old textbooks** and for
  **terse interactive input**.

Author: Daniel R. Grayson · June 2006 · Version 1.0.

---

## `ConwayPolynomials.m2` — the GF database

```m2
conwayPolynomial(p, n)         -- the Conway polynomial of GF(p^n)
conwayPolynomial(p, n, Variable => x)
```

A **Conway polynomial** is a canonical irreducible polynomial over
`F_p` of degree `n`, satisfying compatibility relations that make
field extensions consistent: `F_{p^k} ⊂ F_{p^n}` whenever `k | n`,
and the compatibility map between `GF(p^k)` and `GF(p^n)` is fixed
once Conway polynomials are chosen for both.

This package ships M2's **default database of Conway polynomials**
— Frank Lübeck's standard list. When you call
`GF(p^n)` in M2 and the FLINT engine doesn't have a Conway
polynomial cached, M2 falls back to this database.

The database is **finite** (covers what Lübeck published) — for
fields beyond it, M2 picks an arbitrary irreducible polynomial
instead, with the warning that field-extension compatibility may
not hold.

Auto-loading guarantees `conwayPolynomial` is always available
without requiring an explicit `needsPackage`.

---

## `OnlineLookup.m2` — OEIS and ISC

```m2
oeis {1, 1, 2, 3, 5, 8, 13, 21}
  -- Look up this sequence in the OEIS, return matching results
isc 1.6180339887
  -- Look up this real number in the Inverse Symbolic Calculator
```

`oeis L` — queries the [On-Line Encyclopedia of Integer
Sequences](https://oeis.org) with the integers in `L` and returns
parsed results. Useful when an M2 computation produces a numerical
sequence and you want to know what it is.

`isc x` — queries the [Inverse Symbolic
Calculator](https://isc.carma.newcastle.edu.au/) with a real number
and returns candidate closed-form expressions.

Both functions are **network-dependent**: they make HTTPS requests
to the upstream services. Failed lookups (timeout, no match)
return informative messages rather than crashing.

The aux dir contains worked examples but no algorithm code — the
package is thin and delegates entirely to upstream.

Author: Paul Zinn-Justin · 2022 · Version 0.7.

---

## `PackageCitations.m2` — `cite` your favourite package

```m2
cite "Polyhedra"
  -- prints a BibTeX entry for the Polyhedra package
cite Polyhedra
  -- same, with a loaded Package as argument
```

Generates **BibTeX citation entries** for M2 packages. Reads the
package's metadata (`Authors`, `Headline`, `Date`, `Version`,
`Certification` if present) and emits a `@misc{...}` (or
`@article{...}` for JSAG-certified packages) BibTeX entry.

The package handles tricky cases:

- Author names with TeX-incompatible characters (escaped via
  `quotesToTex`, `headlineToTex`, etc. — all internal).
- Multiple authors (joined with `and` per BibTeX convention).
- Certified packages (use the published-article DOI and venue).

Internal helpers like `hasGoodHeadline` decide whether the
package's `Headline` field is in a usable form for the citation
(short, sentence-cased, no terminal period); if not, the citation
is built with a sensible fallback.

The single function `cite` is exported; `citePackage` is imported
from Core to do the heavy lifting.

---

## `TangentCone.m2` — tangent cones at the origin

```m2
tangentCone I
  -- returns the tangent cone of I at the origin (the ideal of
  -- initial forms of elements of I)
```

For an ideal `I ⊂ k[x_1, …, x_n]`, the **tangent cone** at the
origin is the ideal generated by the **homogeneous initial parts**
of elements of `I`. Algebraically: choose a `(t)`-adic filtration
and take the associated graded ring; geometrically: the
"infinitesimal" version of `V(I)` at the origin.

```m2
tangentCone(I, Strategy => ...)
  -- alternative strategies for computing initial forms
```

The package is **tiny** (84 lines total including documentation
and tests) because the actual work delegates to a GB computation
in a homogenized ring. The wrapper picks the right ring shift,
calls `gb`, and extracts the initial forms.

Tangent cones are used by:

- Algebraic-geometry packages computing local invariants
  (multiplicity, dimension at a point).
- `IntegralClosure`'s singular-locus analysis indirectly.
- Resolution-of-singularities recipes.

---

## When to refresh this batch doc

A small-package doc like this is **inherently a snapshot**. If any
of the five packages doubles in size or gains a substantially
larger API, it should be promoted to its own `file-<Name>.md`
deep dive following the pattern of
[`file-Elimination.md`](file-Elimination.md) (similar size and
shape). The packages most likely to grow are `OnlineLookup` (more
upstream services) and `PackageCitations` (more output formats);
`Classic`, `ConwayPolynomials`, and `TangentCone` are essentially
done.

## See also

- [`README.md`](README.md) — packages overview with auto-loaded list
- [`file-package-conventions.md`](file-package-conventions.md) — conventions
- [`file-MinimalPrimes.md`](file-MinimalPrimes.md), [`file-Saturation.md`](file-Saturation.md), [`file-IntegralClosure.md`](file-IntegralClosure.md), [`file-Complexes.md`](file-Complexes.md), [`file-Varieties.md`](file-Varieties.md), [`file-PrimaryDecomposition.md`](file-PrimaryDecomposition.md), [`file-Elimination.md`](file-Elimination.md), [`file-ReesAlgebra.md`](file-ReesAlgebra.md), [`file-LLLBases.md`](file-LLLBases.md), [`file-InverseSystems.md`](file-InverseSystems.md), [`file-Isomorphism.md`](file-Isomorphism.md), [`file-SimpleDoc.md`](file-SimpleDoc.md) — the 12 auto-loaded packages with their own dedicated deep dives
- [`Parsing.m2`](Parsing.m2) — combinator library `Classic` is built on
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `DEPENDENCIES.md`](../../../DEPENDENCIES.md) — FLINT (the `GF(q)` engine `ConwayPolynomials` complements)
