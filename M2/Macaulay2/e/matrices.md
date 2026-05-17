# Matrices (`matrix*`, `dmat*`, `smat*`, `mutablemat*`, `mat-*`)

Matrices in M2 come in two broad flavours:

- **Immutable `Matrix`** — the abstract type users see in M2-land. Built from
  a list of columns ("vectors"), with metadata (source / target free modules,
  degrees).
- **Mutable `MutableMatrix`** — dense or sparse, mutable in place. Used for
  numerical linear algebra and for the internal scratch space of GB engines.

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e) · [per-area docs](README.md#top-level-files-per-area-docs)

## Immutable `Matrix`

| File pair | Purpose |
|---|---|
| `matrix.{cpp,hpp}` | The `Matrix` class itself — columns, source, target. **Deep dive:** [`file-matrix.md`](file-matrix.md) |
| `matrix-con.{cpp,hpp}` | `MatrixConstructor` helper for building matrices column-by-column |
| `matrix-kbasis.{cpp,hpp}` | k-basis computation (basis of a module in given degrees) |
| `matrix-ncbasis.{cpp,hpp}` | Non-commutative analogue of k-basis |
| `matrix-sort.{cpp,hpp}` | Column / row sorting |
| `matrix-stream.{cpp,hpp}` | Streaming matrix construction (used by file formats and serialisers) |
| `matrix-symm.{cpp,hpp}` | Symmetric power / symmetrisation |

## `MutableMatrix`

| File pair | Purpose |
|---|---|
| `mutablemat.{cpp,hpp}` | The `MutableMatrix` class, dispatching to either dense or sparse storage. **Deep dive:** [`file-mutablemat.md`](file-mutablemat.md) |
| `mutablemat-defs.hpp`, `mutablemat-imp.hpp` | Template definitions and implementations |
| `mutablecomplex.{cpp,hpp}` | A mutable chain complex (sequence of mutable matrices) |

## Generic dense template

The dense-matrix code is heavily templated so that arithmetic is inlined for
each coefficient ring.

| File | Purpose |
|---|---|
| `mat.hpp` | Generic dense matrix base |
| `mat-arith.hpp` | Arithmetic operations templated on the entry type |
| `mat-elem-ops.hpp` | Elementary row/column ops |
| `mat-linalg.hpp` | Linear algebra: rank, LU, determinant |
| `mat-jordan.hpp` | Jordan form |
| `mat-util.hpp` | Utilities |

## Dense matrix specialisations (`dmat-*`)

Each specialisation pairs a coefficient ring with an optimised back end:

| File | Specialisation |
|---|---|
| `dmat.{cpp,hpp}` | Generic dense matrix entry point. **Deep dive:** [`file-dmat.md`](file-dmat.md) |
| `dmat-ffpack.cpp` | FFLAS-FFPACK back end (Z/p, mostly) |
| `dmat-zz-flint.hpp` | ZZ via FLINT |
| `dmat-zzp-flint.hpp` | Z/p via FLINT |
| `dmat-zzp-ffpack.hpp` | Z/p via FFLAS-FFPACK |
| `dmat-qq-flint.hpp`, `dmat-qq-interface-flint.hpp` | QQ via FLINT |
| `dmat-gf-flint.hpp`, `dmat-gf-flint-big.hpp` | GF via FLINT |
| `dmat-CCC-flint.{cpp,hpp}` | CC arbitrary precision (via FLINT/MPFR) |
| `dmat-lu.hpp`, `dmat-LU.hpp`, `dmat-LU-template.hpp`, `dmat-lu-inplace.hpp`, `dmat-lu-qq.hpp`, `dmat-lu-zzp-ffpack.hpp`, `dmat-lu-zzp-flint.hpp` | LU decomposition per back end |

## Sparse matrix

| File | Purpose |
|---|---|
| `smat.hpp` | Sparse matrix template — column-major linked lists of `(row, value)` |

## Related

- [`free-modules.md`](free-modules.md) — `Matrix` source/target are
  `FreeModule`s.
- [`coefficient-rings.md`](coefficient-rings.md) — entries live in a coeff ring.
- [`groebner-bases.md`](groebner-bases.md) — GB algorithms operate on matrices
  of polynomial entries.
- [`interface/matrix.{h,cpp}`](interface/README.md) and
  [`interface/mutable-matrix.{h,cpp}`](interface/README.md) — public API.
- fflas-ffpack & flint submodules under
  [`submodules/`](../../submodules/README.md).
