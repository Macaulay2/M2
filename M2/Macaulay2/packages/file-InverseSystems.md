# `InverseSystems.m2` — Macaulay's inverse systems & divided powers

The `InverseSystems` package implements **Macaulay's inverse systems**
— the classical duality between ideals in the polynomial ring `S =
k[x_1, …, x_n]` and submodules of the **divided power algebra `D`**.
The primary application is constructing **artinian Gorenstein ideals**
from a single dual generator (a polynomial). **Auto-loaded** — every
M2 session has these operations available without `needsPackage`.

- File: `InverseSystems.m2` (1 100 lines — single file, no aux dir)
- Authors: David Eisenbud, Mats Boij
- Version: 1.1 (June 2018)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

```m2
inverseSystem M            -- the ideal annihilating M ⊂ D
inverseSystem(d, M)        -- degree-d truncated version (faster)
inverseSystem(I)           -- the dual: a submodule of D annihilated by I
fromDual M                 -- legacy alias path: build ideal from a "dual generator"
toDual(d, I)               -- truncated dual generator
toDividedPowers p          -- convert from monomial to divided-power basis
fromDividedPowers p        -- inverse conversion
isStandardGradedPolynomialRing R  -- precondition checker
```

### Option symbols

```
DividedPowers => true|false  -- input/output basis convention (default: false = monomial basis)
Gorenstein                   -- marker symbol for artinian-Gorenstein construction recipes
```

## The mathematical setup

For `S = k[x_1, …, x_n]` graded by degree:

- **`D`** = graded Hopf-algebra dual of `S`. Basis dual to the
  monomial basis of `S` is the divided-power basis: monomials
  `x_1^{(m_1)} ⋯ x_n^{(m_n)}`.
- **`D'`** = the local (power-series) version. As an `S`-module,
  `D' = E(k)` is the injective hull of the simple `S`-module `k`.
- In characteristic 0: `S ≅ D` as algebras via `x^a ↔ a! · x^(a)`.
- In characteristic `p > 0`: `D` is **not** finitely generated as
  an algebra, so the two are genuinely distinct.

`D` is an `S`-module: `x_i · x_j^{(m)} = δ_{ij} x_j^{(m-1)}`
(contraction). Any element of `D` is annihilated by some power of
the maximal ideal `m = (x_1, …, x_n)`, so it generates a finite-
dimensional `S`-submodule of `D`.

## Macaulay's correspondence

The duality is:

```
{finitely generated S-submodules of D}  ↔  {m-primary ideals in S}
                  M                    →   Ann(M) ⊂ S
                  D[I]                ←   I  (the "dual" of I)
```

`inverseSystem` realises this in both directions:

- `inverseSystem M` — given an `S`-submodule of `D` (presented as
  a matrix), return its annihilator ideal in `S`.
- `inverseSystem I` — given an ideal `I ⊂ S`, return the dual
  submodule of `D`.

The degree-`d`-truncated form `inverseSystem(d, M)` computes only
the part of the answer up to degree `d` — useful when you know
your input is concentrated in low degrees.

## Why this matters: artinian Gorenstein ideals

A polynomial ring quotient `R = S/I` is **artinian Gorenstein** iff:

- `I` is `m`-primary (so `R` is artinian).
- `I = Ann(f)` for a single element `f ∈ D`.

So **every artinian Gorenstein ring `S/I` arises as `S/Ann(f)`** for
some `f`. The most useful application of this package:

```m2
S = QQ[x, y]
f = x^4 + 2*x*y^3 + y^4
I = inverseSystem matrix{{f}}    -- the Gorenstein ideal
R = S/I
dim R, isHomogeneous R           -- 0-dimensional, homogeneous
```

`R` is automatically artinian (because `f` is annihilated by some
power of `m`) and Gorenstein (because we used a single generator).

The `Gorenstein` symbol exists as a **named marker** for recipes
that produce Gorenstein ideals via this construction; it isn't a
function in itself.

## The `DividedPowers` option

The key user-facing distinction: which basis does the input/output
use?

| `DividedPowers => false` (default) | `DividedPowers => true` |
|---|---|
| User polynomial is interpreted in the **monomial** basis: `x^a ↔ x^a` | User polynomial is interpreted in the **divided-power** basis: `x^a ↔ x^(a)` |
| Convenient in characteristic 0 (and matches first-pass intuition) | Necessary in characteristic `p > 0` for correctness |
| `inverseSystem(matrix{{x^2 + y^2}})` annihilates the "ordinary" `x^2 + y^2` | Same call with `DividedPowers => true` annihilates `x^{(2)} + y^{(2)}` |

The internal converters are exposed:

- `toDividedPowers p` — convert a polynomial from the monomial
  basis (the way you typed it) into the divided-power basis.
- `fromDividedPowers p` — the reverse.

In characteristic 0, these multiply / divide by factorials of the
exponents. In positive characteristic, they're more subtle.

## `fromDual` / `toDual` — the legacy names

Earlier versions of M2 had `fromDual` and `toDual` as separate
methods. They survive as exports for back-compat:

| Legacy name | Modern equivalent |
|---|---|
| `fromDual M` | `inverseSystem M` (ideal from dual gen) |
| `toDual(d, I)` | `inverseSystem(d, I)` (truncated dual) |

Both accept the `DividedPowers` option. New code should prefer
`inverseSystem`.

## How it works under the hood

For `inverseSystem M` (annihilator computation):

1. Compute the **derivative matrix**: `diff(monomial_basis,
   transpose monomial_basis)` — the matrix of all partial
   derivatives.
2. **Contract** against `M`: this produces a matrix whose columns
   are the partial-derivative orbit of `M` under the polynomial
   ring's action on `D`.
3. The **syzygies** of that matrix are the elements of `S` that
   annihilate the orbit — by definition, the annihilator ideal.

The `containsDthPowers` helper is a precondition checker: a
correct `inverseSystem(d, M)` requires that the input has dual
support in degrees `≤ d`.

## Performance characteristics

| Symptom | Cause |
|---|---|
| `inverseSystem(f)` slow for high-degree `f` | The derivative matrix grows polynomially in `deg(f) * n`; use truncated `inverseSystem(d, f)` if you know the answer is concentrated below degree `d` |
| Wrong answer in characteristic `p > 0` | Almost always a missing `DividedPowers => true` — the default basis convention is monomial, which only matches the dual basis in characteristic 0 |
| Hangs on inhomogeneous input | This package assumes graded inputs over a standard-graded polynomial ring; check with `isStandardGradedPolynomialRing R` first |

## Single-file architecture

Like several other auto-loaded packages, `InverseSystems` is a
single 1 100-line file with all production code, documentation
`doc ///…///` blocks, and `TEST ///…///` blocks intermixed.
Search by section:

| Section | Topic |
|---|---|
| Lines 34-58 | `isStandardGradedPolynomialRing`, `toDividedPowers` / `fromDividedPowers` (basis conversion) |
| Lines 75-130 | `fromDual` / `toDualTrunc` / `toDual` (legacy entry points) |
| Lines 97-110 | `containsDthPowers` (precondition checker) |
| Lines 135-160 | `inverseSystem` (the modern unified entry point, 4 overloads) |
| Lines 165+ | Documentation `doc ///…///` and `TEST ///…///` blocks |

## Companions

- The **Gorenstein recipes** in the doc cross-reference the more
  general decomposition packages
  ([`PrimaryDecomposition`](file-PrimaryDecomposition.md),
  [`MinimalPrimes`](file-MinimalPrimes.md)) for when the ideal
  produced by `inverseSystem` needs further analysis.
- The complementary `MaxOrder` / `Gorenstein` packages in the
  add-on ecosystem build on this one's primitives.

## See also

- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [`file-MinimalPrimes.md`](file-MinimalPrimes.md), [`file-PrimaryDecomposition.md`](file-PrimaryDecomposition.md) — natural companions when analysing the Gorenstein ideals produced
- [`file-Complexes.md`](file-Complexes.md) — for studying the resolution of an artinian Gorenstein ring (always self-dual)
- Engine ring construction: [`e/file-polyring.md`](../e/file-polyring.md)
- [Repo `RING-ZOO.md`](../../../RING-ZOO.md) — standard graded polynomial rings (the only input shape this package supports)
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
