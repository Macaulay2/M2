# `matrix-con.{cpp,hpp}` — `MatrixConstructor` (immutable-matrix builder)

`MatrixConstructor` is the **builder** for the engine's immutable
[`Matrix`](file-matrix.md) type. Because `Matrix` is immutable once
constructed, the engine needs a separate, mutable builder that
accumulates state and produces a `Matrix` at the end. That builder is
this class.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Class shape

```cpp
class MatrixGenerator;

class MatrixConstructor {
    const Ring        *R;
    VECTOR(vec)        entries;
    const FreeModule  *rows;        // target
    const FreeModule  *cols;        // source (may be built up)
    bool               cols_frozen; // see below
    // ...
};
```

State:

- `R`, `rows`, `cols` — ring and source/target free modules.
- `entries` — vector of columns (`vec` is the engine's sparse
  column-of-coefficients type).
- `cols_frozen` — once set, no more column-set modifications. Used
  when the caller supplied an existing free module as the source; the
  constructor then refuses operations that would change it.

## Two construction modes

The header comment explains:

> If cols is given at the beginning, this is used. If this is
> immutable, no changes are allowed, other than to replace the entire
> thing.

Two modes:

1. **Caller supplies the source** — `MatrixConstructor(target, source)`.
   `cols_frozen` is set immediately. The caller is committing to a
   specific number of columns with specific degrees.
2. **Constructor builds the source** — `MatrixConstructor(target, 0)`.
   Each `append_column(col)` extends `cols`. `cols_frozen` is set only
   when `to_matrix()` is called.

The two modes correspond to two different usage patterns: building a
matrix to fit a known interface vs. building a matrix exploratively.

## Workflow

```cpp
MatrixConstructor mb(target_freemod, source_freemod_or_0);
mb.set_column(0, col0);                       // mode 1: set existing
mb.set_column(1, col1);
mb.append_column(col2);                       // mode 2: extend source
Matrix *m = mb.to_matrix();
```

`to_matrix()` performs final validation:

- Every column's vector lives in the target.
- Per-column degrees are consistent with the source's degree vector.
- If the source was given, the column count matches its rank.

A validation failure produces an engine error (via
[`file-error.md`](file-error.md)) and returns `nullptr`.

## `MatrixGenerator` (forward-declared)

`MatrixGenerator` is a sibling helper used when building matrices from
streaming inputs (e.g. file parsers). It feeds columns into a
`MatrixConstructor` one at a time.

## Used by

Essentially every engine function that returns a `Matrix*`:

- Arithmetic in [`file-matrix.md`](file-matrix.md) — `add`, `mult`,
  `transpose`, etc.
- [`file-comp-gb.md`](file-comp-gb.md) — `get_gb()`, `get_mingens()`,
  etc. all build their outputs via `MatrixConstructor`.
- [`file-comp-res.md`](file-comp-res.md) — every level's differential.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-matrix.md`](file-matrix.md) — the immutable output.
- [`file-freemod.md`](file-freemod.md) — `FreeModule` source / target.
- `matrix-stream.{cpp,hpp}` ([`file-matrix-stream.md`](file-matrix-stream.md))
  — streaming builder that consumes this.
