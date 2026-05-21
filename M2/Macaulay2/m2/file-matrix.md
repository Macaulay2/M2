# `matrix.m2` — the M2-side `Matrix` type

`matrix.m2` defines the **M2-side `Matrix` type** — the user-facing
wrapper for the engine's [`Matrix`](../e/file-matrix.md) class.
Together with its companions `matrix1.m2` and `matrix2.m2`, it
covers the public matrix API.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "monoids.m2"   -- for degreesMonoid
needs "reals.m2"     -- for inexact number
needs "gateway.m2"   -- for id

-----------------------------------------------------------------------------
-- Matrix

Matrix = new Type of HashTable
Matrix.synonym = "matrix"
raw Matrix := f -> f.RawMatrix
ring Matrix := f -> (
     S := ring target f;
     R := ring source f;
     -- ... ring agreement check ...
)
```

Three pieces of fundamental machinery:

1. **`Matrix = new Type of HashTable`** — `Matrix` is structurally a
   hash table whose fields include `RawMatrix`, `source`, `target`,
   `cache`, etc.
2. **`raw Matrix := f -> f.RawMatrix`** — extract the engine-side
   `Matrix*` pointer from an M2 `Matrix` for engine calls.
3. **`ring Matrix`** — returns the ring of the entries, enforcing
   that source and target free modules use the same ring.

## Three files, one type

The `Matrix` type's implementation is spread over three files:

| File | Approximate scope |
|---|---|
| `matrix.m2` (this file) | Construction, basic arithmetic, `ring`, `source`, `target` |
| `matrix1.m2` | Ideal, kernel, image, submatrices, basis-related ops |
| `matrix2.m2` | LU, determinants, solve, advanced linear algebra |

The split predates the engine refactor and reflects how the original
authors thought about matrix functionality. Combined size: enough to
warrant the split.

## `sameRing` helpers

```m2
notSameRing := (X, Y) -> ...
sameRing = (M, N) -> if ring M === ring N then (M, N) else notSameRing(class M, class N)
notToSameRing := (X, Y) -> ...
toSameRing = (M, N) -> ...
```

A small validation library used throughout the M2 layer:

- **`sameRing(M, N)`** — strict: ring identity required.
- **`toSameRing(M, N)`** — flexible: tries to promote one matrix's
  ring to match the other (e.g., `ZZ → QQ`).

These produce clear error messages when the rings don't match —
much better than silent type coercion.

## Used by

- M2's `matrix`, `map`, `**`, `++`, `*`, etc. operators.
- Every M2 package that constructs matrices.
- The engine boundary in [`../e/interface/file-matrix-interface.md`](../e/interface/file-matrix-interface.md).

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-matrix.md`](../e/file-matrix.md) — engine class.
- `matrix1.m2`, `matrix2.m2`, `genmat.m2`, `mutablemat.m2` — sibling files.
- [`file-modules.md`](file-modules.md) — `Module` type, used as
  source/target.
