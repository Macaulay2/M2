# `gfanInterface.m2` — interface to Anders Jensen's Gfan software

The `gfanInterface` package is M2's **interface to Anders Jensen's
Gfan** — a high-performance command-line tool for **Gröbner fans**,
**tropical varieties**, and related polyhedral / commutative-algebra
objects. The package exposes ~60+ Gfan subcommands as M2 functions.

Sibling to [`FourTiTwo`](file-FourTiTwo.md), [`Normaliz`](file-Normaliz.md),
[`Bertini`](file-Bertini.md), [`PHCpack`](file-PHCpack.md) — all five
follow the same external-library-wrapper pattern:
`Configuration => { "path" => …, "keepfiles" => true }`, shell-out
to the binary, parse output files into M2 types.

- File: `gfanInterface.m2` (4 864 lines — single file)
- Authors: Mike Stillman, Andrew Hoefel, Diane Maclagan (maintainer), Josephine Yu
- Version: 0.5 (May 2021)
- Re-exports: [`Polyhedra`](file-Polyhedra.md) (the package's output types include `Fan` and `Cone` from there)
- **Requires Gfan installed externally** (binary named `gfan`)
- Configuration: `"path"`, `"fig2devpath"`, `"keepfiles"`, `"verbose"`, `"cachePolyhedralOutput"`

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API (~61 `gfan*` operations)

The package exposes Gfan's command-line subcommands almost
one-to-one. Each `gfan*` function:
1. Takes M2-side inputs (`Ideal`, `Matrix`, `MarkedPolynomialList`, etc.).
2. Writes a Gfan input file in the dedicated format.
3. Shells out to the corresponding Gfan binary (`gfan_<subcommand>` or `gfan` with options).
4. Parses the output back into M2 types.

### Gröbner-basis & lead-term operations

```m2
gfan(I)                            -- compute the Gröbner fan
gfanBuchberger(I)                   -- Buchberger-style GB through Gfan
gfanMarkPolynomialSet(L)            -- attach leading terms (mark) to a polynomial list
gfanIsMarkedGroebnerBasis(L)        -- is the marked list a GB?
gfanLeadingTerms(L)                 -- extract leading terms
gfanGroebnerCone(L)                 -- the Gröbner cone of a marked GB
gfanInitialForms(L, w)              -- in_w(I) — initial forms under weight w
gfanDoesIdealContain(I, f)          -- ideal-membership test through Gfan
gfanHomogenize(f, h), gfanHomogeneitySpace(I)
                                    -- homogenisation operations
```

### Tropical-geometry operations

```m2
gfanTropicalStartingCone(I)         -- starting cone for tropical-variety algorithm
gfanTropicalBasis(I)                -- a tropical basis
gfanTropicalIntersection(I)
gfanTropicalLifting(I)
gfanTropicalLinearSpace(M)
gfanTropicalRank(M)
gfanTropicalTraverse(C0, I)         -- traverse the tropical variety from a starting cone
gfanTropicalVariety(I)              -- the full tropical variety
```

### Fan / polytope operations

```m2
gfanFanCommonRefinement(F, G)       -- common refinement of two fans
gfanFanLink(F)                      -- link of a fan
gfanFanProduct(F, G)                -- product fan
gfanMinkowskiSum(F, G)              -- Minkowski sum of fans
gfanSecondaryFan(M)                 -- secondary fan of a point configuration
gfanResultantFan(...)               -- resultant fan
gfanStats(F)                        -- combinatorial statistics
```

### Specialised invariants

```m2
gfanKrullDimension(I)
gfanMinors(R, n, M)                 -- minors-related computation
gfanLatticeIdeal(M)                 -- lattice ideal of a sublattice
gfanSaturation(I)                   -- saturation w.r.t. all variables
gfanRender(F, fname)                -- render a fan as a TikZ / Postscript / SVG file
gfanRenderStaircase(L)
```

### M2-side types

```m2
MarkedPolynomialList               -- a list of polynomials with marked leading terms
markedPolynomialList(L)            -- constructor
MPLConverter                       -- option for input-format conversion
polymakeFanToFan, polymakeConeToCone
                                    -- convert from polymake output to Polyhedra's types
```

## Why Gfan exists vs other tools

Gfan's specialty is **explicit-fan / explicit-cone-decomposition algorithms**: given an ideal `I ⊂ k[x_1, …, x_n]`, Gfan can compute:

- The **Gröbner fan** of `I` — the polyhedral fan whose cones are equivalence classes of weight vectors that produce the same initial ideal.
- The **tropical variety** of `I` — the closure of `{w : in_w(I) is not monomial}`.
- Various **refinements / products** of fans.

These are **finite combinatorial objects** that classical Gröbner-basis tools don't naturally produce. The closest M2-side rivals are:

| Operation | Best tool |
|---|---|
| Hilbert basis of a single cone | [`Normaliz`](file-Normaliz.md) |
| Toric Markov / Graver / circuits | [`FourTiTwo`](file-FourTiTwo.md) |
| Generic polyhedral operations (convex hull, intersection, Fourier-Motzkin) | [`Polyhedra`](file-Polyhedra.md) |
| **Gröbner fans, tropical varieties, fan refinements** | **Gfan** (this package) |

The 61 `gfan*` operations cover Gfan's user-visible surface essentially completely.

## Polymake integration

Polymake is another external polyhedral tool. Some Gfan operations emit `polymake`-format output; this package's helpers parse that format:

```m2
polymakeFanToFan(...)   -- convert polymake fan → Polyhedra `Fan`
polymakeConeToCone(...) -- convert polymake cone → Polyhedra `Cone`
```

These are mostly internal but exposed for users who want to drive a polymake-Gfan-M2 pipeline manually.

## MarkedPolynomialList — the package's main custom type

```m2
markedPolynomialList(polys, marks)   -- attach a leading-term marking to each polynomial
```

Many Gröbner-basis algorithms in Gfan operate on "marked" lists where leading terms are pre-identified. The `MarkedPolynomialList` type wraps `(polynomials, marks)` together so the Gfan side knows what each polynomial's leading term is.

The marks are a polynomial subset where each entry is a single term (the leading term in the order under consideration).

## Heavy downstream consumers

| Package | What it uses from gfanInterface |
|---|---|
| [`Polyhedra`](file-Polyhedra.md) (re-exported) | `fanFromGfan` (turn Gfan output into a `Fan`) — re-exported as a `Polyhedra` constructor |
| `Tropical` and tropical-geometry packages | The full `gfanTropical*` suite |
| Combinatorial-commutative-algebra add-ons | Gröbner fans for state-polytope analysis |
| `NumericalAlgebraicGeometry` (indirectly) | Some tropical-geometry-flavoured paths reference Gfan tropical fans |

## When this is slow

| Symptom | Try |
|---|---|
| `gfan(I)` on a large ideal | The Gröbner fan grows exponentially in the number of variables; bound the input |
| `gfanTropicalVariety(I)` very slow | Tropical varieties are computationally expensive; try `gfanTropicalIntersection` for an easier intersection problem |
| `gfan` binary not found | Set `Configuration => { "path" => "/abs/path/to/gfan/bin/" }` |
| Output parsing errors | Check `Configuration => { "verbose" => true }` to see what Gfan is producing and at what step parsing fails |
| Fan-renderer (`fig2dev`) missing | Set `Configuration => { "fig2devpath" => …}` to point at the `fig2dev` binary, or skip `gfanRender` |

## Single-file architecture

Like its external-library-wrapper siblings, this package is a single 4 864-line file. Internal structure (by section comment):

| Section | Topic |
|---|---|
| Header | newPackage, exports, configuration |
| File-marshalling | `writeIdealForGfan`, `parseGfanOutput`, format conversions |
| `MarkedPolynomialList` type | Definition + operations |
| GB-flavoured operations | `gfan`, `gfanBuchberger`, `gfanInitialForms`, `gfanGroebnerCone`, etc. |
| Tropical operations | The `gfanTropical*` suite |
| Fan operations | `gfanFan*`, `gfanSecondaryFan`, `gfanResultantFan`, etc. |
| Specialised invariants | `gfanKrullDimension`, `gfanLatticeIdeal`, `gfanMinors`, `gfanSaturation` |
| Render | `gfanRender`, `gfanRenderStaircase` |
| Documentation + tests trailing |

## See also

- [`file-FourTiTwo.md`](file-FourTiTwo.md), [`file-Normaliz.md`](file-Normaliz.md) — sibling polyhedral external-library wrappers
- [`file-Bertini.md`](file-Bertini.md), [`file-PHCpack.md`](file-PHCpack.md) — sibling numerical-AG wrappers
- [`file-Polyhedra.md`](file-Polyhedra.md) — re-exports this package's output types; provides the `fanFromGfan` constructor
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [Gfan](https://users-math.au.dk/~jensen/software/gfan/gfan.html) — the upstream software, by Anders Jensen
- [Repo `DEPENDENCIES.md`](../../../DEPENDENCIES.md) — external-library catalogue
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — Gröbner fan / tropical catalogue
