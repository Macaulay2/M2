# `matrix.{cpp,hpp}` — the immutable `Matrix` class

`Matrix` is the engine's representation of a homomorphism `F → G` between two
[free modules](file-freemod.md), where each column of the matrix is a vector
in the target free module. It is immutable: every operation that "modifies" a
matrix actually returns a new one.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## State

```cpp
class Matrix : public EngineObject {
    FreeModule *mTarget;       // the codomain
    FreeModule *mSource;       // the domain
    monomial   mDegreeShift;   // an element of the degree monoid
    gc_vector<vec> mEntries;   // one vec per column
    // ...
};
```

The matrix carries:

- A **target** free module (where columns live).
- A **source** free module (the column count plus per-column degrees).
- A **degree shift** — a single monomial offset applied uniformly to all
  entry degrees. Used heavily by graded computations.
- A vector of **columns**, each represented as a `vec` (a sparse linked list
  of `(component, coefficient)` pairs over the target's free-module basis).

`gc_vector<>` is the engine's GC-friendly vector — backed by Boehm allocation.

## Construction

`Matrix` is built through `MatrixConstructor` (in
[`matrix-con.{cpp,hpp}`](matrices.md)), not directly. The constructor is
private; `MatrixConstructor` is the only `friend` allowed to call it.

```cpp
MatrixConstructor mb(target_free_module, source_free_module);
mb.set_column(0, my_vec);
mb.set_column(1, another_vec);
Matrix *m = mb.to_matrix();
```

This pattern lets the matrix be validated for degree compatibility before its
state is frozen.

## Immutability

Adding two matrices, multiplying by a scalar, transposing — all return a new
`Matrix*`. The engine relies on this immutability to share matrices safely
across threads and to memoise expensive derived values (basis, kernel, image).

For mutable matrix arithmetic, use [`MutableMatrix`](matrices.md) instead.

## Operations

Top-level methods of interest (defined in `matrix.cpp`):

- `matrix_add`, `matrix_subtract`, `matrix_mult`, `matrix_negate`
- `transpose`, `submatrix`
- `direct_sum`, `tensor`
- `lead_term` (cw. monomial order on the target), `lead_coefficient`
- `homogenize`, `degrees`, `is_homogeneous`

Higher-level operations have their own files:

| File | Operation |
|---|---|
| [`matrix-kbasis.cpp`](matrices.md) | k-basis (basis of a module in given degrees) |
| [`matrix-ncbasis.cpp`](matrices.md) | non-commutative analogue |
| [`matrix-sort.cpp`](matrices.md) | sort columns / rows |
| [`matrix-stream.cpp`](matrices.md) | streaming construction / serialisation |
| [`matrix-symm.cpp`](matrices.md) | symmetric power |

## Related

- [`file-freemod.md`](file-freemod.md) — source and target types.
- [`matrices.md`](matrices.md) — area overview.
- [`interface/matrix.{h,cpp}`](interface/README.md) — public C interface.
- [`mutablemat.{cpp,hpp}`](matrices.md) — mutable counterpart.
