# Ring zoo

A **catalogue of every ring M2 supports** — user-facing
constructors, underlying engine type, when to use each, common
operations, and pointers to the deep-dive docs.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Engine: coefficient rings](M2/Macaulay2/e/coefficient-rings.md) · [Engine: polynomial rings](M2/Macaulay2/e/polynomial-rings.md)

## Layers

M2's ring system has **two layers**:

```
User M2 code:    QQ, ZZ, RR, CC, GF(8), QQ[x,y], R/I, ...
                          │
                          ▼
M2-side classes: Ring, PolynomialRing, FractionField, ...
                 (defined in Macaulay2/m2/ring.m2, etc.)
                          │
                          ▼
Engine boundary: rawRing, rawQQ, rawPolynomialRing, ...
                          │
                          ▼
Engine classes:  Ring* / aring framework (ARingXXX)
                 ConcreteRing<ARingXXX> bridges them
```

See [`e/ring-elements-and-maps.md`](M2/Macaulay2/e/ring-elements-and-maps.md)
for the bridge story.

## Integers and rationals

### `ZZ` — integers

```m2
ZZ
1 + 2_ZZ
factor 360
gcd(12, 18)
```

- **User type**: `ZZ` (predefined).
- **Engine ring**: `RingZZ` (legacy) or `ARingZZ` (FLINT-backed,
  modern).
- **Element representation**: arbitrary-precision via GMP (with
  small-int optimisation).
- **Use for**: integer arithmetic, integer GBs, lattice stuff.

**Deep dives**:
[`e/file-ZZ.md`](M2/Macaulay2/e/file-ZZ.md),
[`e/file-aring-zz-flint.md`](M2/Macaulay2/e/file-aring-zz-flint.md),
[`e/file-aring-zz-gmp.md`](M2/Macaulay2/e/file-aring-zz-gmp.md).

### `QQ` — rationals

```m2
QQ
3/4_QQ
factor((x^2-1)/(x-1))    -- can work in QQ[x]
```

- **User type**: `QQ`.
- **Engine ring**: `RingQQ` / `ARingQQ` (GMP or FLINT backend).
- **Element representation**: GMP `mpq_t` or FLINT `fmpq_t`.
- **Use for**: exact rational arithmetic.

**Deep dives**:
[`e/file-aring-qq.md`](M2/Macaulay2/e/file-aring-qq.md),
[`e/file-aring-qq-gmp.md`](M2/Macaulay2/e/file-aring-qq-gmp.md),
[`e/file-aring-qq-flint.md`](M2/Macaulay2/e/file-aring-qq-flint.md).

## Finite fields

### `ZZ/p` (a.k.a. `Z/p`) — prime fields

```m2
kk = ZZ/7
kk = GF 7      -- equivalent
3_kk + 5_kk    -- ==> 1
```

- **User constructor**: `ZZ/p` (any `p` prime).
- **Engine ring**: `Z_mod` (legacy) or `ARingZZp` (modern).
- **Element representation**: packed `unsigned long` (each value
  in `[0, p)`).
- **Backends**:
  - `ARingZZp` (M2 native) — works for any p.
  - `ARingZZpFFPACK` — FFLAS-FFPACK backend; fast for `p ≤ 2^23`.
  - `ARingZZpFlint` — FLINT backend; competitive with FFPACK.
- **Use for**: most computational algebra; default for GB
  computations.

**Deep dives**:
[`e/file-ZZp.md`](M2/Macaulay2/e/file-ZZp.md),
[`e/file-aring-zzp.md`](M2/Macaulay2/e/file-aring-zzp.md),
[`e/file-aring-zzp-ffpack.md`](M2/Macaulay2/e/file-aring-zzp-ffpack.md),
[`e/file-aring-zzp-flint.md`](M2/Macaulay2/e/file-aring-zzp-flint.md).

### `GF(q)` — Galois fields

```m2
kk = GF(2^4)         -- GF(16)
kk = GF(2, 4)        -- same
kk = GF(3^2)         -- GF(9)
a = kk_0             -- primitive element
a^2 + a + 1 == 0_kk  -- usually
```

- **User constructor**: `GF(p^n)` or `GF(p, n)`.
- **Engine ring**: `GF` (legacy) or one of:
  - `ARingGFFlint` (small/medium fields, FLINT backend).
  - `ARingGFFlintBig` (large fields with bit-packing).
  - `ARingGFM2` (pure M2 native).
  - `ARingGFTable` (tiny fields, precomputed log table).
  - `ARingGFGivaro` (when Givaro available).
- **Element representation**: depends on backend; usually a
  packed integer encoding `a^i`.
- **Use for**: finite-field linear algebra, coding theory,
  cryptography research.

**Deep dives**:
[`e/file-GF.md`](M2/Macaulay2/e/file-GF.md),
[`e/file-aring-gf-flint.md`](M2/Macaulay2/e/file-aring-gf-flint.md),
[`e/file-aring-gf-flint-big.md`](M2/Macaulay2/e/file-aring-gf-flint-big.md),
[`e/file-aring-m2-gf.md`](M2/Macaulay2/e/file-aring-m2-gf.md).

### Boolean polynomial ring `F_2[x_i] / (x_i^2 - x_i)`

```m2
needsPackage "BIBasis"
R = ZZ/2[x_1..x_5]
I = ideal(x_1*x_2 + x_3, ...)
-- BIBasis-specific computations
```

- **Underlying engine**: specialised `bibasis/` subdirectory.
- **Use for**: cryptanalysis, SAT-style polynomial systems,
  biological-network modelling.

**Architecture**: [`e/bibasis/architecture.md`](M2/Macaulay2/e/bibasis/architecture.md).

## Floating-point

### `RR` — real numbers (`double`-backed)

```m2
RR_53        -- 53-bit (standard double)
pi_RR
sin(1.5_RR)
```

- **User type**: `RR` (53-bit by default; precision tunable).
- **Engine ring**: `ARingRR` (53-bit), `ARingRRR` (MPFR
  arbitrary).
- **Element representation**: `double` for 53-bit; `mpfr_t` for
  higher.
- **Use for**: numerical computation, machine learning, root
  finding.

**Deep dives**:
[`e/file-aring-RR.md`](M2/Macaulay2/e/file-aring-RR.md),
[`e/file-aring-RRR.md`](M2/Macaulay2/e/file-aring-RRR.md).

### `RR_53` vs `RR_200` etc — variable-precision real

```m2
x = numeric(200, sqrt(2))    -- 200-bit precision
ring x                        -- RR_200
```

- **Precision** is part of the type. Different precisions are
  different rings.
- **Use for**: certified-numerical computation where rounding
  matters.

### `RRi` — interval real

```m2
needsPackage "Intervals"
RRi
interval(1, 2)
```

- **Engine ring**: `ARingRRi` (MPFI-backed).
- **Element representation**: `mpfi_t` (centre + radius).
- **Use for**: certified arithmetic; numerical linear algebra
  with rigorous error bounds.

**Deep dive**: [`e/file-aring-RRi.md`](M2/Macaulay2/e/file-aring-RRi.md).

### `CC` — complex numbers (`complex<double>`-backed)

```m2
ii                     -- imaginary unit
1 + 2*ii
exp(ii * pi)
```

- **Engine ring**: `ARingCC` (53+53 = 106 bits) or `ARingCCC`
  (MPC, arbitrary precision).
- **Element representation**: `std::complex<double>` for 53-bit;
  MPC `mpc_t` for higher.
- **Use for**: numerical AG, polynomial root finding.

**Deep dives**:
[`e/file-aring-CC.md`](M2/Macaulay2/e/file-aring-CC.md),
[`e/file-aring-CCC.md`](M2/Macaulay2/e/file-aring-CCC.md).

### `CCi` — complex interval

```m2
needsPackage "Intervals"
CCi
```

- **Engine ring**: `ARingCCi` (MPC-like interval).

**Deep dive**: [`e/file-aring-CCi.md`](M2/Macaulay2/e/file-aring-CCi.md).

## Polynomial rings

### `R[x_1, ..., x_n]` — polynomial ring

```m2
R = QQ[x, y, z]
S = ZZ/7[a..d]
T = QQ[x_1..x_4]
```

- **User type**: `PolynomialRing`.
- **Engine ring**: `PolyRing` / `PolynomialRing`.
- **Monomial ordering**: defaults to graded reverse-lex; tunable
  via `MonomialOrder => ...`.
- **Element representation**: linked-list of `Nterm`s (term =
  coefficient × monomial).

**Deep dives**:
[`e/file-polyring.md`](M2/Macaulay2/e/file-polyring.md),
[`e/polynomial-rings.md`](M2/Macaulay2/e/polynomial-rings.md).

### Monomial orderings

```m2
R = QQ[x, y, z, MonomialOrder => Lex]
R = QQ[x, y, z, MonomialOrder => {Weights => {1, 2, 3}}]
R = QQ[x, y, z, MonomialOrder => Eliminate 2]
```

- **Options**: `Lex`, `GLex`, `GRevLex` (default), `Weights => ...`,
  `Eliminate ...`, custom block orders.
- **Engine class**: `MonomialOrdering`,
  `Monoid`-stored.

**Deep dives**:
[`e/file-monoid.md`](M2/Macaulay2/e/file-monoid.md),
[`e/file-monordering.md`](M2/Macaulay2/e/file-monordering.md),
[`e/monoids-and-monomials.md`](M2/Macaulay2/e/monoids-and-monomials.md).

### Quotient rings

```m2
R = QQ[x, y, z]
I = ideal(x*y - z, y*z - x)
S = R/I
```

- **User type**: `QuotientRing` (subtype of `PolynomialRing`).
- **Engine ring**: `PolynomialRing` with the quotient ideal
  tracked.
- **Element representation**: same as polynomial; arithmetic
  reduces modulo a precomputed GB of the quotient ideal.

### Fraction fields

```m2
R = QQ[x, y]
F = frac R
```

- **User type**: `FractionField`.
- **Engine ring**: `FractionField`.
- **Element representation**: pair `(numerator, denominator)`
  with canonicalisation.
- **Use for**: rational functions; field of fractions.

**Deep dive**: [`e/file-frac.md`](M2/Macaulay2/e/file-frac.md).

### Local rings

```m2
needsPackage "LocalRings"
R = QQ[x, y]
m = ideal vars R
S = localRing(R, m)
```

- **User type**: `LocalRing`.
- **Engine ring**: `LocalRing` (a specialised quotient).
- **Element representation**: pair (numerator, denominator) where
  the denominator must be outside a fixed maximal ideal.

**Deep dive**: [`e/file-localring.md`](M2/Macaulay2/e/file-localring.md).

### Weyl algebras

```m2
needsPackage "WeylAlgebras"
R = QQ[x_1..x_3, dx_1..dx_3, WeylAlgebra => {x_1 => dx_1, x_2 => dx_2, x_3 => dx_3}]
```

- **Engine ring**: `WeylAlgebra` (a specialised polynomial ring
  with non-commutative variable pairs).
- **Use for**: D-modules, differential algebra.

**Deep dive**: [`e/file-weylalg.md`](M2/Macaulay2/e/file-weylalg.md).

### Skew-commutative (exterior) algebras

```m2
R = QQ[e_1..e_4, SkewCommutative => true]
e_1 * e_2 == -e_2 * e_1      -- yes
e_1^2 == 0                    -- yes
```

- **Engine ring**: `SkewPolynomialRing`.
- **Use for**: exterior algebra computations.

**Deep dive**: [`e/file-skewpoly.md`](M2/Macaulay2/e/file-skewpoly.md).

### Solvable algebras

```m2
needsPackage "SolvableAlgebras"
-- Specialised non-commutative polynomial rings with rules
```

- **Engine ring**: `SolvableAlgebra`.

**Deep dive**: [`e/file-solvable.md`](M2/Macaulay2/e/file-solvable.md).

## Non-commutative algebras

### Free algebras `F<x_1, ..., x_n>`

```m2
needsPackage "AssociativeAlgebras"
R = QQ<|x, y, z|>
x*y      -- not equal to y*x
```

- **User type**: `M2FreeAlgebra`.
- **Engine class**: `FreeAlgebra` (in NCAlgebras subdir).
- **Element representation**: linked list of `(coefficient,
  word)` pairs.

**Architecture**: [`e/NCAlgebras/architecture.md`](M2/Macaulay2/e/NCAlgebras/architecture.md).

**Deep dives**:
[`e/file-M2FreeAlgebra.md`](M2/Macaulay2/e/file-M2FreeAlgebra.md),
[`e/NCAlgebras/file-FreeAlgebra.md`](M2/Macaulay2/e/NCAlgebras/file-FreeAlgebra.md).

### Free-algebra quotients

```m2
needsPackage "AssociativeAlgebras"
R = QQ<|x, y|> / ideal(x*y + y*x)
```

- **User type**: `M2FreeAlgebraQuotient`.
- **Engine class**: `FreeAlgebraQuotient`.
- **Element representation**: reduces to normal form via a
  two-sided GB.

**Deep dive**: [`e/file-M2FreeAlgebraQuotient.md`](M2/Macaulay2/e/file-M2FreeAlgebraQuotient.md).

## Specialised / niche rings

### `SchurRing` — symmetric functions

```m2
needsPackage "SchurRings"
R = schurRing(s, 4)
```

- **Engine ring**: `SchurRing` / `SchurRing2`.
- **Element representation**: linear combinations of partitions.
- **Use for**: representation theory, combinatorics.

**Deep dives**:
[`e/file-schur.md`](M2/Macaulay2/e/file-schur.md),
[`e/file-schur2.md`](M2/Macaulay2/e/file-schur2.md),
[`e/file-schurSn.md`](M2/Macaulay2/e/file-schurSn.md).

### Tower rings (`a[b][c][...]`)

```m2
R = QQ[a]/(a^3)
S = R[b]/(b^2 - a)
T = S[c]
```

- **Engine type**: `Tower` (iterated extension).
- **Element representation**: nested polynomial structure.
- **Use for**: Galois-theoretic constructions, algebraic-number
  computations.

**Deep dives**:
[`e/file-tower.md`](M2/Macaulay2/e/file-tower.md),
[`e/file-aring-tower.md`](M2/Macaulay2/e/file-aring-tower.md).

## Modules and matrices over rings

These aren't rings themselves, but they're built on rings —
included for completeness.

### Free modules

```m2
R = QQ[x, y]
F = R^3      -- the free module of rank 3
```

- **Class**: `FreeModule`.

**Deep dives**:
[`e/file-freemod.md`](M2/Macaulay2/e/file-freemod.md),
[`e/free-modules.md`](M2/Macaulay2/e/free-modules.md).

### Modules

```m2
M = coker matrix{{x, y}, {y, x}}   -- presentation
M = image matrix ...                -- alternative
```

- **Class**: `Module` (presented via a `Matrix`).

### Matrices (immutable)

```m2
M = matrix{{1, 2}, {3, 4}}
```

- **Class**: `Matrix`.

### Mutable matrices

```m2
M = mutableMatrix(R, 3, 4)
M_(0, 0) = 1     -- mutable
```

- **Class**: `MutableMatrix`.
- **Engine variants**: `DMat<R>` dense, `SMat<R>` sparse.

**Deep dives**:
[`e/file-matrix.md`](M2/Macaulay2/e/file-matrix.md),
[`e/file-mat.md`](M2/Macaulay2/e/file-mat.md),
[`e/file-dmat.md`](M2/Macaulay2/e/file-dmat.md),
[`e/file-smat.md`](M2/Macaulay2/e/file-smat.md),
[`e/matrices.md`](M2/Macaulay2/e/matrices.md).

## Decision tree: which ring?

```
Working over an integer-shaped ring?
  ZZ for exact integers
  ZZ/p for finite prime field (default for GBs)
  GF(p^n) for general finite field

Working over a field of characteristic 0?
  QQ for exact rationals
  RR (or RR_N) for floats (with controllable precision)
  CC for complex floats
  RRi / CCi for certified arithmetic with intervals

Building polynomials?
  R[x_1..x_n] for ordinary polynomial ring
  R[x..]/I for quotient
  frac R for rational functions
  R[x_1..x_n, SkewCommutative => true] for exterior algebra
  R[x_1..x_n, WeylAlgebra => ...] for D-module computations
  R<|x_1, ..., x_n|> for free non-commutative algebra

Specialised algebraic structures?
  SchurRing for symmetric-function algebra
  Tower for nested extensions
  LocalRing for local-at-an-ideal
```

## Operations applicable to most rings

```m2
1_R + 2_R               -- arithmetic
1_R * 2_R
1_R - 2_R
ring x                  -- get the ring of x
characteristic R        -- 0 or p
isField R               -- boolean
isCommutative R         -- boolean
class R                 -- the type of R (QuotientRing, etc.)
```

For polynomial rings:

```m2
gens R                  -- generators (list of variables)
numgens R               -- number of variables
coefficientRing R       -- the base ring
monoid R                -- the monomial monoid
options R               -- the options used to create R
```

## Cross-references

| Concept | Where |
|---|---|
| Engine ring class hierarchy | [`e/architecture.md`](M2/Macaulay2/e/architecture.md) |
| Legacy `Ring` vs modern `aring` | [`e/ring-elements-and-maps.md`](M2/Macaulay2/e/ring-elements-and-maps.md) |
| Coefficient ring choices | [`e/coefficient-rings.md`](M2/Macaulay2/e/coefficient-rings.md) |
| Polynomial ring construction | [`e/polynomial-rings.md`](M2/Macaulay2/e/polynomial-rings.md) |
| Monoid / monomial encoding | [`e/monoids-and-monomials.md`](M2/Macaulay2/e/monoids-and-monomials.md) |
| Free modules | [`e/free-modules.md`](M2/Macaulay2/e/free-modules.md) |
| Matrices | [`e/matrices.md`](M2/Macaulay2/e/matrices.md) |
| NC algebras | [`e/NCAlgebras/architecture.md`](M2/Macaulay2/e/NCAlgebras/architecture.md) |
| BIBasis (Boolean involutive) | [`e/bibasis/architecture.md`](M2/Macaulay2/e/bibasis/architecture.md) |
| The Core `rings.m2` source | [`m2/file-rings.md`](M2/Macaulay2/m2/file-rings.md) |

## Operations: ring → ring maps and morphisms

```m2
R = QQ[x, y]
S = QQ[a, b, c]
f = map(S, R, {a^2, b*c})   -- the ring homomorphism R → S
f x                           -- a^2
```

- **Class**: `RingMap`.

**Deep dive**: [`e/file-ringmap.md`](M2/Macaulay2/e/file-ringmap.md).

## Promote / lift between rings

```m2
R = QQ[x]
S = R[y]
y_S * x_R          -- automatically promotes x_R to S
lift(x_S, R)       -- moves x back from S to R when possible
promote(2_ZZ, R)   -- moves 2_ZZ up to R
```

The `promote` / `lift` machinery handles automatic conversion
between related rings. See
[`e/file-aring-translate.md`](M2/Macaulay2/e/file-aring-translate.md).

## Related

- [`README.md`](README.md) — repository TOC.
- [`GLOSSARY.md`](GLOSSARY.md) — definitions of ring-related
  terms.
- [`M2/Macaulay2/e/coefficient-rings.md`](M2/Macaulay2/e/coefficient-rings.md)
  — coefficient ring area overview.
- [`M2/Macaulay2/e/polynomial-rings.md`](M2/Macaulay2/e/polynomial-rings.md)
  — polynomial ring area overview.
- [`M2/Macaulay2/e/ring-elements-and-maps.md`](M2/Macaulay2/e/ring-elements-and-maps.md)
  — value-representation strategy across rings.
- Each `e/file-aring-*.md` deep dive — backend-specific details.
- M2 user documentation — `help "rings"`, `viewHelp` for each
  ring type.
- The book *Computations in Algebraic Geometry with Macaulay 2*
  (Springer 2002) for canonical example computations.
