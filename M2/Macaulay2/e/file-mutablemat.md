# `mutablemat.{cpp,hpp}` — `MutableMatrix`

`MutableMatrix` is the engine's **mutable** matrix abstraction. Unlike the
immutable [`Matrix`](file-matrix.md), entries can be changed in place,
columns swapped, rows reduced. It is the workhorse for numerical linear
algebra (LU, LLL, rank, etc.) and for in-place internal scratch space.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## File split

| File | Role |
|---|---|
| `mutablemat.hpp` | Umbrella include — pulls in defs, imp, SLP bits |
| `mutablemat-defs.hpp` | Class declarations and method signatures |
| `mutablemat-imp.hpp` | Template implementations |
| `mutablemat.cpp` | Non-template glue and registry |

The header / implementation split lets the same class be instantiated for
each [coefficient ring](coefficient-rings.md) without recompiling unrelated
code.

## Two storage strategies

`MutableMatrix` is an **abstract** front for two storage families:

- **Dense** (`DMat<RingType>`) — column-major dense array, one entry per
  matrix cell. Optimal when most cells are non-zero or when the underlying
  arithmetic is fast (e.g. `double`, fixed Z/p via FFLAS-FFPACK).
- **Sparse** (`SMat<RingType>`) — column-major linked list of
  `(row, value)` pairs. Optimal when most cells are zero, as in the input
  to many GB computations.

The choice between dense / sparse is made at construction time, often based
on the input shape. Operations dispatch through virtual methods at the
top-level `MutableMatrix` API and into templated `DMat<R>` / `SMat<R>`
implementations.

## API highlights

- **Construction:** `MutableMatrix::zero_matrix(R, nrows, ncols, is_dense)`,
  `MutableMatrix::identity(R, n, is_dense)`.
- **In-place mutation:** `set_entry`, `row_op`, `column_op`, `swap_rows`,
  `swap_columns`.
- **Read-only queries:** `n_rows`, `n_columns`, `get_entry`, `is_zero`.
- **Higher operations:** `transpose`, `add`, `multiply`, `solve`, `LU`,
  `rank`, `nullspace`, `inverse`.

## Dense back ends

The dense path has multiple specialisations chosen by the entry ring:

| Entry ring | Implementation |
|---|---|
| Z/p (FFLAS-FFPACK) | `dmat-ffpack.cpp`, `dmat-zzp-ffpack.hpp` |
| Z/p (FLINT) | `dmat-zzp-flint.hpp` |
| ZZ (FLINT) | `dmat-zz-flint.hpp` |
| QQ (FLINT) | `dmat-qq-flint.hpp` |
| GF (FLINT) | `dmat-gf-flint.hpp`, `dmat-gf-flint-big.hpp` |
| CC (Flint/MPFR) | `dmat-CCC-flint.{cpp,hpp}` |
| Generic | `dmat.hpp` plus templated `mat-arith.hpp`, `mat-linalg.hpp`, … |

See [`matrices.md`](matrices.md) for the full list.

## Mutable complexes

A `MutableComplex` (`mutablecomplex.{cpp,hpp}`) is a sequence of
`MutableMatrix`es with cohomology operations — used for in-place complex
manipulations during certain resolutions.

## SLP integration

The header pulls in `SLP-defs.hpp` and `SLP-imp.hpp` because
[straight-line programs](file-SLP.md) operate on `MutableMatrix`-shaped
inputs (parameters and evaluation buffers).

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-matrix.md`](file-matrix.md) — immutable counterpart.
- [`file-LLL.md`](file-LLL.md) — operates in place on a `MutableMatrix`.
- [`file-SLP.md`](file-SLP.md) — shares evaluation infrastructure.
- [`interface/mutable-matrix.{h,cpp}`](interface/README.md) — public API.
