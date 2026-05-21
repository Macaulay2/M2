# Non-commutative algebras engine architecture

This document is the **architectural reference** for
`M2/Macaulay2/e/NCAlgebras/` — the engine's support for
non-commutative free algebras `R<x_1, …, x_n>` and their quotients.

[← NCAlgebras/ overview](README.md) · [← engine architecture](../architecture.md)

## What this engine handles

A **non-commutative free algebra** `R<x_1, …, x_n>` (or `R<x, y, z>`
in the typical use case) consists of all `R`-linear combinations of
**words** in the generators `x_1, …, x_n` — where words are not
identified up to reordering. Examples:

- `xy + yx` is not the same as `2xy`.
- `xyz` is not the same as `xzy` or `yzx`.

This is the world of associative-algebra calculations: tensor
algebras, universal enveloping algebras, quiver-path algebras,
Cohn localisations, etc. M2's `AssociativeAlgebras` user package
exposes this engine.

## Three core abstractions

### 1. `FreeMonoid` — the monoid of words

Just the **multiplicative structure**: how to multiply and compare
words, with a monomial ordering. No coefficients yet.

[`FreeMonoid.{cpp,hpp}`](file-FreeMonoid.md) — analogous to the
commutative engine's `Monoid` ([`../file-monoid.md`](../file-monoid.md))
but with words instead of exponent vectors.

### 2. `FreeAlgebra` — words plus coefficients

[`FreeAlgebra.{cpp,hpp}`](file-FreeAlgebra.md) — adds a coefficient
ring on top of a `FreeMonoid`, giving the actual algebra.

Polynomials in `FreeAlgebra` are **linked lists of `(coefficient,
word)` pairs**, ordered by the monoid's monomial ordering.

### 3. `FreeAlgebraQuotient` — modding out by an ideal

[`FreeAlgebraQuotient.{cpp,hpp}`](file-FreeAlgebraQuotient.md) —
takes a `FreeAlgebra` plus a finite set of generators of a
two-sided ideal, and computes a Gröbner basis. Then every polynomial
reduces to a unique normal form.

The two-sided constraint makes this much harder than commutative
GB — see "NC Gröbner bases" below.

## Three-layer architecture

```
┌──────────────────────────────────────────────────────┐
│   M2-facing wrappers (top-level e/)                   │
│   M2FreeAlgebra, M2FreeAlgebraQuotient                │
├──────────────────────────────────────────────────────┤
│   NC algebraic structures (this subdir)               │
│   FreeMonoid, FreeAlgebra, FreeAlgebraQuotient,       │
│   Word, Range                                         │
├──────────────────────────────────────────────────────┤
│   NC algorithms                                       │
│   NCGroebner, NCF4, NCReduction,                      │
│   WordTable, OverlapTable, SuffixTree                 │
└──────────────────────────────────────────────────────┘
```

The M2-facing wrappers live at the top of `e/` (because they need
to be visible to the legacy `Ring` framework); the C++ machinery
lives in this subdirectory.

## NC Gröbner bases

The classical Buchberger algorithm doesn't work in NC. Key
differences:

1. **No unique reduction** — `xyz` could be reduced "from the left"
   or "from the right" by `xy → 0` (giving `0`) or by `yz → 0`
   (giving `0`), but in general the order matters.
2. **Infinite GBs are common** — many NC ideals have no finite
   Gröbner basis. We compute up to a degree limit.
3. **Critical pairs are "overlaps" of words** — not S-polynomials.

The engine has **two NC GB algorithms**:

### Classical NC Gröbner

[`NCGroebner.{cpp,hpp}`](file-NCGroebner.md) — Buchberger-style
algorithm adapted for words. Uses:

- [`OverlapTable`](file-OverlapTable.md) for critical-pair
  enumeration (analogous to S-pairs in commutative GB).
- [`SuffixTree`](file-SuffixTree.md) for fast overlap detection.
- [`WordTable`](file-WordTable.md) — leading-word lookup.
- [`NCReduction`](file-NCReduction.md) — actual polynomial reduction.

### NC-F4

[`NCF4.{cpp,hpp}`](file-NCF4.md) — F4-style adaptation. Like the
commutative F4 ([`../f4/architecture.md`](../f4/architecture.md)),
collects S-polynomials into a Macaulay matrix and reduces in linear
algebra. Faster when the matrix is large and dense.

## Algorithm primitives

### `WordTable`

A specialised data structure for "which polynomial in the basis has
this leading word as a prefix?". Used by every reduction step.
Implementation in [`file-WordTable.md`](file-WordTable.md).

### `OverlapTable`

Tracks "overlap pairs" — pairs of basis elements whose leading
words overlap in a way that forms a new critical pair. Analogous
to the S-pair set in commutative GB. [`file-OverlapTable.md`](file-OverlapTable.md).

### `SuffixTree`

For fast string-suffix lookups during overlap detection. Each
basis element's leading word becomes a path in the tree;
overlap-finding becomes tree traversal. [`file-SuffixTree.md`](file-SuffixTree.md).

### `Range<T>`

A C++17-compatible "slice of a container" type — like `std::span`
but written before the engine could rely on C++20.
[`file-Range.md`](file-Range.md).

### `Word`

A non-owning view over a sequence of variable indices. Cheap to
pass; references its parent storage. [`file-Word.md`](file-Word.md).

## Memory model

NC computations stress allocation hard — typical input creates
billions of small polynomial terms. The engine uses:

- **`MemoryBlock`** for transient polynomial terms during reduction.
- **`Word` non-owning views** to avoid copies during overlap
  detection.
- **Boehm GC** for long-lived results returned to the M2 layer.
- **Suffix-tree nodes** in their own pool.

## Why a separate subdir

Three reasons:

1. **Algorithmic distance** — NC GB is genuinely different from
   commutative GB. Sharing `Monoid` / `comp-gb` wouldn't reuse
   much.
2. **Build-time isolation** — heavy template instantiation; isolating
   keeps overall build time manageable.
3. **Active research code** — NC algebras are evolving; the subdir
   pattern lets the code move quickly without affecting the rest of
   the engine.

## Used by

- The `AssociativeAlgebras` user package.
- Research workflows on path algebras, quivers, tensor algebras.
- [`../NCResolutions/`](../NCResolutions/README.md) — NC free
  resolutions built on this.

## Related

- [`README.md`](README.md) — NCAlgebras/ navigation hub.
- [`../architecture.md`](../architecture.md) — engine architectural
  reference.
- [`../NCResolutions/README.md`](../NCResolutions/README.md) —
  resolution engine for NC algebras.
- [`../file-M2FreeAlgebra.md`](../file-M2FreeAlgebra.md),
  [`../file-M2FreeAlgebraQuotient.md`](../file-M2FreeAlgebraQuotient.md)
  — M2-facing wrappers.
- [`../f4/architecture.md`](../f4/architecture.md) — sister F4
  engine for commutative GB.
- [`../groebner-bases.md`](../groebner-bases.md) — engine GB area
  overview.
