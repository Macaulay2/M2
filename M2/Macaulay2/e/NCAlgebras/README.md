# `M2/Macaulay2/e/NCAlgebras/` — non-commutative algebras

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

## Related

- The M2-level wrapper of non-commutative rings lives in
  [`m2/freealgebras.m2`](../../m2/README.md).
- `M2FreeAlgebra.{cpp,hpp}` and `M2FreeAlgebraQuotient.{cpp,hpp}` at the top
  level of `e/` are the M2-facing wrappers for these classes.
- [`../NCResolutions/`](../NCResolutions/README.md) — resolutions in the NC
  setting.

[← back to engine overview](../README.md)
