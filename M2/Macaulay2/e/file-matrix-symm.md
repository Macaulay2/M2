# `matrix-symm.cpp` — `SymmMatrix` (symmetric power of a matrix)

`matrix-symm.cpp` implements the **symmetric power** of a matrix —
the `p`-th symmetric power `Sym^p(M)` viewed as a matrix in its own
right. For a one-row matrix `M = [m_1, …, m_n]` of degree-1 entries,
`Sym^p(M)` is the row of all products of `p` factors taken from `M`'s
columns.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "matrix.hpp"
#include "matrix-con.hpp"

class SymmMatrix {
public:
    static Matrix /* or null */ *symmetricPower(const Matrix *m0, int p) {
        if (m0->n_rows() != 1) {
            ERROR("expected one row");
            return nullptr;
        }

        SymmMatrix s(m0, p);
        // ... enumerate symmetric monomials ...
    }
};
```

Two notes:

- **`m0` must have one row** — `SymmMatrix` is set up for the
  one-row case. For matrices with multiple rows, the algorithm
  reduces to one symmetric power per row, multiplied; this is done at
  the M2 level via the `Symm` built-in.
- **Static factory** — there is no `SymmMatrix` instance the user
  holds; the class is a tiny namespace for `symmetricPower(...)`.

## Output

For `m0 = [m_1, …, m_n]` and `p ≥ 0`:

```
output = [m_{i_1} · m_{i_2} · … · m_{i_p}  :  1 ≤ i_1 ≤ i_2 ≤ … ≤ i_p ≤ n]
```

The number of columns is `binomial(n + p - 1, p)`. The output is a
one-row matrix in the same ring as the input.

## Enumeration

The implementation walks symmetric monomials in lex order on the
multi-index tuples `(i_1, …, i_p)`. For each tuple it computes the
product of input columns and appends to a `MatrixConstructor`
([`file-matrix-con.md`](file-matrix-con.md)). The combinatorial
enumeration shares an algorithm with
[`file-comb.md`](file-comb.md)'s `Subsets` (for the
strictly-increasing index sets), generalised to multisets.

## Used by

- M2's `symmetricPower(p, M)` built-in.
- Algebraic-geometry packages that compute symmetric tensor algebras.
- Schubert calculus when expressing symmetric polynomials in terms of
  power sums.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-matrix.md`](file-matrix.md), [`file-matrix-con.md`](file-matrix-con.md)
  — types involved.
- [`file-comb.md`](file-comb.md) — combinatorial-iteration helpers.
- [`file-schur.md`](file-schur.md) — related symmetric-function ring.
