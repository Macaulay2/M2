# `matrix.{h,cpp}` (in `interface/`) — public C entry points for `Matrix`

`interface/matrix.h` declares the **public C functions** the interpreter calls
to construct, transform, and query the engine's immutable
[`Matrix`](../file-matrix.md) class.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

The same dual-mode pattern as every other `interface/*.h` file:

```c
#if defined(__cplusplus)
class FreeModule;
class Matrix;
class Ring;
class RingElement;
#else
typedef struct FreeModule  FreeModule;
typedef struct Matrix      Matrix;
typedef struct Ring        Ring;
typedef struct RingElement RingElement;
#endif
```

The forward declarations make the header consumable both by engine C++
code and by the `.d`-generated C glue the interpreter uses.

## Entry points

The functions here cover the full life cycle of an engine `Matrix`:

- **Construction** — `rawMatrixFromVecs`, `rawMatrixIdentity`, `rawZero`,
  `rawSparseMatrix`.
- **Inspection** — `rawNumberOfRows`, `rawNumberOfColumns`, `rawTarget`,
  `rawSource`, `rawMatrixEntry`, `rawIsHomogeneous`.
- **Arithmetic** — `rawMatrixAdd`, `rawMatrixSubtract`, `rawMatrixMult`,
  `rawMatrixNegate`, `rawMatrixScalarMult`.
- **Transformation** — `rawMatrixTranspose`, `rawSubmatrix`, `rawHomogenize`,
  `rawMatrixSort`, `rawMatrixDirectSum`, `rawMatrixTensor`.
- **Higher operations** — `rawLeadCoefficients`, `rawLeadMonomial`,
  `rawCoefficients`.

All operations take/return opaque `Matrix*` and follow the same `raw…`
naming convention used throughout `interface/`.

## How operations dispatch

Inside `matrix.cpp` each entry point:

1. Verifies the inputs (ring agreement, dimension compatibility).
2. Calls into [`Matrix`](../file-matrix.md) or `MatrixConstructor` for the
   actual operation.
3. Sets engine errors via `error.{cpp,hpp}` ([`utilities.md`](../utilities.md))
   on failure.

Since `Matrix` is **immutable** ([`file-matrix.md`](../file-matrix.md)),
every operation returns a fresh pointer.

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-matrix.md`](../file-matrix.md) — class implementation.
- [`../matrices.md`](../matrices.md) — matrix area overview.
- [`file-mutable-matrix-interface.md`](file-mutable-matrix-interface.md) —
  mutable counterpart (when it lands).
- [`../../d/engine.dd`](../../d/README.md) — interpreter side.
