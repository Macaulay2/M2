# `mutable-matrix.{h,cpp}` (in `interface/`) — public C entry points for `MutableMatrix`

`interface/mutable-matrix.h` declares the **public C functions** the
interpreter uses to construct and manipulate
[`MutableMatrix`](../file-mutablemat.md) values — the engine's in-place
mutable matrix type.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#if defined(__cplusplus)
class Matrix;
class MutableMatrix;
class Ring;
class RingElement;
#else
typedef struct Matrix        Matrix;
typedef struct MutableMatrix MutableMatrix;
typedef struct Ring          Ring;
typedef struct RingElement   RingElement;
#endif
```

The header bridges between mutable and immutable matrices — a `Matrix`
can be converted to a `MutableMatrix` and back.

## Entry points

- **Construction** — `rawMutableMatrix(R, nrows, ncols, is_dense)`,
  `rawMutableIdentity`, `rawMutableMatrixFromMatrix` (copy an immutable
  matrix).
- **In-place mutation** — `rawSetMatrixEntry`, `rawMatrixRowSwap`,
  `rawMatrixColumnSwap`, `rawMatrixRowOperation`,
  `rawMatrixColumnOperation`, `rawMatrixScaleRow`, `rawMatrixScaleColumn`.
- **Read-only queries** — `rawNumberOfRows`, `rawNumberOfColumns`,
  `rawMatrixEntry`, `rawGetRing`.
- **Linear algebra** — `rawLU`, `rawSolve`, `rawInverse`,
  `rawDeterminant`, `rawRank`, `rawNullSpace`, `rawRowReduce`.
- **Conversion** — `rawMutableMatrixToMatrix` (snapshot to immutable).

## Dispatch into back ends

Each linear-algebra entry point goes through the templated `DMat<R>`
([`../file-dmat.md`](../file-dmat.md)) and chooses the best back end:

- **FFLAS-FFPACK** for Z/p (fast).
- **FLINT** for ZZ, QQ, GF.
- **MPFR / Arb** for RR, CC.
- **Generic** otherwise.

The interpreter sees none of this — it just hands a `MutableMatrix*` to
the entry point.

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-mutablemat.md`](../file-mutablemat.md) — class implementation.
- [`../file-dmat.md`](../file-dmat.md) — dense back end.
- [`../matrices.md`](../matrices.md) — area overview.
- [`file-matrix-interface.md`](file-matrix-interface.md) — immutable
  counterpart.
- [`../file-LLL.md`](../file-LLL.md) — operates in place on
  `MutableMatrix`.
