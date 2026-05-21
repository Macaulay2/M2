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

## M2 operation → engine matrix backend

The mapping from M2-user matrix expressions to the engine backend that does the work:

| M2 operation | Backend chosen | Source file | Notes |
|---|---|---|---|
| `matrix {{a,b},{c,d}}` (over `ZZ/p`, `QQ`, `ZZ`, …) | Immutable `Matrix` over the coefficient ring | `matrix.{cpp,hpp}` | Stored column-by-column; immutable; carries source/target `FreeModule`s |
| `mutableMatrix M` (dense, default) | `DMat<RingType>` | `dmat.{cpp,hpp}` + per-ring specialisation | Dense storage; ring-templated for inlined arithmetic |
| `mutableMatrix(M, Dense => false)` | `SMat<RingType>` | `smat.hpp` | Sparse column-major linked-list storage |
| `det M`, `rank M`, `inverse M` (over `ZZ/p`) | `DMat<ARingZZpFFPACK>` ops via FFLAS-FFPACK | `dmat-zzp-ffpack.hpp` + `dmat-ffpack.cpp` | BLAS-style dispatch; orders of magnitude faster than generic |
| `det M`, `rank M` (over `QQ`) | `DMat<ARingQQFlint>` via FLINT | `dmat-qq-flint.hpp` | FLINT's `fmpq_mat` operations |
| `det M`, `rank M` (over `ZZ`) | `DMat<ARingZZGMP>` via FLINT or generic | `dmat-zz-flint.hpp` | FLINT `fmpz_mat` when available |
| LU decomposition (Z/p, generic) | `DMatLUtemplate<RingType>` | `dmat-LU-template.hpp` | Templated fallback; in-place |
| LU decomposition (Z/p, FFPACK) | `DMat-lu-zzp-ffpack` | `dmat-lu-zzp-ffpack.hpp` | BLAS-backed LU; fast |
| LU decomposition (Z/p, FLINT) | `DMat-lu-zzp-flint` | `dmat-lu-zzp-flint.hpp` | FLINT-backed LU |
| LU decomposition (QQ) | `DMat-lu-qq` | `dmat-lu-qq.hpp` | FLINT `fmpq_mat_*` |
| `basis(d, R)` (k-basis of a quotient ring) | `matrix-kbasis.cpp` | `matrix-kbasis.{cpp,hpp}` | Enumerates monomials of degree `d`; uses the monoid's basis function |
| `basis(d, R)` over an NC ring | `matrix-ncbasis.cpp` | `matrix-ncbasis.{cpp,hpp}` | Non-commutative analogue; integrates with [`NCAlgebras/`](NCAlgebras/README.md) |
| `symmetricPower(d, M)` | `matrix-symm.cpp` | `matrix-symm.{cpp,hpp}` | Symmetric-power computation on free modules |

## Matrix-backend selection

The runtime picks a dense-matrix backend by inspecting the coefficient ring's type tag. Roughly:

```
M2: M = mutableMatrix M0; rank M
   ↓
m2/mutable.m2   →  rawRank(M)
   ↓
d/interface.dd  →  Ccode(int, "IM2_MutableMatrix_rank(M.p)")
   ↓
e/interface/mutable-matrix.h  →  IM2_MutableMatrix_rank(M)
   ↓
e/dmat.cpp  dispatcher:
   if ring is ZZ/p (small, FFPACK available)   → dmat-zzp-ffpack
   if ring is ZZ/p (FLINT)                      → dmat-zzp-flint
   if ring is ZZ (FLINT)                        → dmat-zz-flint
   if ring is QQ (FLINT)                        → dmat-qq-flint
   if ring is GF (FLINT, small)                 → dmat-gf-flint
   if ring is GF (FLINT, big)                   → dmat-gf-flint-big
   if ring is CC arbitrary precision            → dmat-CCC-flint
   else                                          → templated `mat-arith.hpp` over the ring's `ElementType`
```

The selection happens at construction time and is **stable** for the lifetime of the matrix — once an `MutableMatrix` is built over a specific ring, its operations all route to the same backend.

## Which storage / backend when

When picking a representation for a new algorithm:

| Want | Pick |
|---|---|
| User-visible matrix that will be returned to M2 | `Matrix` (immutable); never `MutableMatrix` |
| Inner-loop scratch space | `MutableMatrix`; specifically `DMat<…>` if dense, `SMat<…>` if sparse |
| Dense linear algebra over `Z/p`, fast | `DMat<ARingZZpFFPACK>` |
| Dense linear algebra over `Z/p`, any `p` | `DMat<ARingZZpFlint>` |
| Dense linear algebra over `ZZ` or `QQ` | `DMat<ARingZZGMP>` / `DMat<ARingQQFlint>` |
| Sparse matrix with many zero rows | `SMat<…>` (column-major linked lists; rare insertion/deletion is cheap) |
| Need both — small but mostly zero | `SMat`; consider hybrid only when profiling shows it matters |
| Numerical linear algebra (`RR`, `CC`) | `DMat<ARingRR>` / `DMat<ARingCC>` (hardware FP) or `DMat<ARingRRR>` / `DMat<ARingCCC>` (MPFR/MPC) |
| Interval arithmetic (rigorous bounds) | `DMat<ARingRRi>` / `DMat<ARingCCi>` |

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
