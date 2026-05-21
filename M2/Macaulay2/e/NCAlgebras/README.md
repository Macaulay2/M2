# `M2/Macaulay2/e/NCAlgebras/` — non-commutative algebras

**See [`architecture.md`](architecture.md)** for the standalone architectural reference (three core abstractions `FreeMonoid`/`FreeAlgebra`/`FreeAlgebraQuotient`, the two NC GB algorithms `NCGroebner` and `NCF4`, suffix-tree-based overlap detection, memory model).

The non-commutative side of the engine: free algebras, free monoids, and
non-commutative Gröbner bases (including a non-commutative F4 variant).

## Per-file deep dives

| File doc | Class |
|---|---|
| [`file-FreeMonoid.md`](file-FreeMonoid.md) | `FreeMonoid` (word side) |
| [`file-FreeAlgebra.md`](file-FreeAlgebra.md) | `FreeAlgebra` (free algebra) |
| [`file-NCGroebner.md`](file-NCGroebner.md) | `NCGroebner` (Buchberger-style NC GB) |
| [`file-NCF4.md`](file-NCF4.md) | `NCF4` (F4-style NC GB) |
| [`file-WordTable.md`](file-WordTable.md) | `WordTable` (leading-word index) |
| [`file-OverlapTable.md`](file-OverlapTable.md) | `OverlapTable` (overlap queue / NC S-pairs) |
| [`file-NCReduction.md`](file-NCReduction.md) | `PolynomialHeap` (NC reduction) |
| [`file-SuffixTree.md`](file-SuffixTree.md) | `SuffixTree` (experimental word index) |
| [`file-FreeAlgebraQuotient.md`](file-FreeAlgebraQuotient.md) | `FreeAlgebraQuotient` |
| [`file-Word.md`](file-Word.md) | `Word` (non-owning word view) |
| [`file-Range.md`](file-Range.md) | `Range<T>` (iterator-pair view) |

## Files

### Algebra structures

| File | Role |
|---|---|
| `FreeMonoid.{cpp,hpp}` | Words in finitely many generators with a chosen ordering |
| `FreeAlgebra.{cpp,hpp}` | Polynomial-ring analogue over a free monoid |
| `FreeAlgebraQuotient.{cpp,hpp}` | Quotient of a `FreeAlgebra` by a two-sided ideal |

### Gröbner machinery

| File | Role |
|---|---|
| `NCGroebner.{cpp,hpp}` | Generic non-commutative Gröbner basis driver |
| `NCF4.{cpp,hpp}` | F4-style non-commutative Gröbner basis algorithm |
| `NCReduction.{cpp,hpp}` | Polynomial reduction tailored to NC setting |
| `OverlapTable.{cpp,hpp}` | Tracks overlaps between leading words (the NC analogue of S-pairs) |

### Word indexing

| File | Role |
|---|---|
| `Word.{cpp,hpp}` | Word data type |
| `WordTable.{cpp,hpp}` | Table of words for membership / lookup |
| `SuffixTree.{cpp,hpp}` | Generalised suffix tree used to accelerate word matching |

### Utilities and inputs

| File | Role |
|---|---|
| `Range.hpp` | Lightweight iterator pair |
| `NCGB.m2`, `roos.m2` | M2-level inputs / examples used while developing the algorithms |

## What triggers this engine

The NC algebras engine is selected when:

| M2 user code | What happens | Why this engine |
|---|---|---|
| `R = freeAlgebra(QQ, {x, y, z})` | Engine builds a `FreeAlgebra` wrapped by `M2FreeAlgebra` (see [`../file-M2FreeAlgebra.md`](../file-M2FreeAlgebra.md)) | The user explicitly asked for a free associative algebra |
| `R = freeAlgebra(QQ, …)/I` | Builds a `FreeAlgebraQuotient` wrapped by `M2FreeAlgebraQuotient` (see [`../file-M2FreeAlgebraQuotient.md`](../file-M2FreeAlgebraQuotient.md)) | Quotient by a two-sided ideal; requires NC GB |
| `gb I` in a free or NC-quotient algebra | Auto-selected → `NCGroebner` (or `NCF4` if requested) | The general-purpose dispatcher in `comp-gb.cpp` recognises NC rings and routes here; **no flag needed** |
| `gb(I, Strategy => NCF4)` in an NC ring | Explicitly route to `NCF4` instead of the default `NCGroebner` | Strategy flag for the F4-style variant; faster on some inputs, slower on others |
| `R^n / I` (`Module` over a free algebra) | Module operations route through `FreeAlgebra` arithmetic plus NC GB for syzygies | Module side of NC theory |

For the full M2-spec → engine mapping see the parent [`groebner-bases.md`](../groebner-bases.md) "M2 strategy → engine algorithm" table — the NC entry is auto-routed by ring shape.

## Where in the engine pipeline this fits

```
M2:  R = freeAlgebra(QQ, {x, y, z}); I = ideal(x*y - y*x, …); gb I
   ↓
M2FreeAlgebra (file-M2FreeAlgebra.md)        — Ring-protocol wrapper
   ↓ holds
FreeAlgebra (file-FreeAlgebra.md)             — internal NC ring
   ↓ uses
FreeMonoid (file-FreeMonoid.md)               — words as monomials
   ↳ Word (file-Word.md)                      — non-owning word view
   ↳ Range<T> (file-Range.md)                 — iterator-pair view
WordTable (file-WordTable.md)                 — leading-word index
   ↳ SuffixTree (file-SuffixTree.md)          — experimental fast index

gb I  →  comp-gb.cpp dispatcher  →  NCGroebner (default)
                                       or NCF4 (if Strategy => NCF4)
   ↓ uses
OverlapTable (file-OverlapTable.md)           — NC S-pair queue
   ↓ each step
PolynomialHeap (file-NCReduction.md)          — NC reduction
WordTable                                       — find divisors of leading words
   ↓ produces
A two-sided NC Gröbner basis, returned via comp-gb
```

The **key distinction from commutative GB**: monomials are now *words* (non-commuting), not exponent vectors. The `WordTable` + `SuffixTree` indexes accelerate divisibility checks; `OverlapTable` replaces S-pair queues (because NC S-pairs come from word overlaps, not LCM of monomials).

## When to use this vs commutative GB

| Want | Pick |
|---|---|
| Polynomials in variables that commute | Stay in `f4/` / `gb-f4/` / `gb-default` — much faster |
| Polynomials in variables that don't commute | This engine — auto-selected |
| Free associative algebra `k<x_1, …, x_n>` | This engine via `freeAlgebra` |
| Path algebras / group algebras | This engine; the path/group quotient ideal goes into `freeAlgebra/I` |
| Quasi-polynomial Weyl-like (variables satisfy `xy - yx = scalar`) | The commutative `WeylAlgebra` is much faster — see [`../file-weylalg.md`](../file-weylalg.md) |
| Solvable algebra (PBW-ordered NC) | The commutative `SolvableAlgebra` is much faster — see [`../file-solvable.md`](../file-solvable.md) |

If your algebra has a known "almost-commutative" structure (Weyl, skew, PBW solvable), it's worth using the commutative-side specialised class. Only fall back to this engine when the algebra has no such structure (true free algebras and their generic quotients).

## Related

- The M2-level wrapper of non-commutative rings lives in
  [`m2/freealgebras.m2`](../../m2/README.md).
- `M2FreeAlgebra.{cpp,hpp}` and `M2FreeAlgebraQuotient.{cpp,hpp}` at the top
  level of `e/` are the M2-facing wrappers for these classes.
- [`../NCResolutions/`](../NCResolutions/README.md) — resolutions in the NC
  setting.
- [`../groebner-bases.md`](../groebner-bases.md) — parent area doc; NC entry in the "M2 strategy → engine algorithm" table.

[← back to engine overview](../README.md)
