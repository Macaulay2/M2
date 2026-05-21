# `LLL.{cpp,hpp}` — Lenstra-Lenstra-Lovász basis reduction

`LLL.cpp` implements the classical **LLL algorithm** for reducing an integer
lattice basis. Given a matrix over `ZZ` whose columns span a lattice, it
returns a new basis of the same lattice whose vectors are short and nearly
orthogonal.

Part of the [Other computations](computations.md) area.

[← per-area: computations](computations.md) · [← engine overview](README.md)

## Algorithm

The Lenstra-Lenstra-Lovász (1982) algorithm runs in polynomial time and
produces an "LLL-reduced" basis: each Gram-Schmidt projection coefficient is
bounded by `1/2`, and the squared lengths of consecutive Gram-Schmidt vectors
satisfy the Lovász condition

```
||b*_{k+1}||^2 ≥ (α - μ_{k+1,k}^2) ||b*_k||^2
```

for a parameter `α ∈ (1/4, 1)`.

The implementation here exposes `α` as a rational `(alphaTop, alphaBottom)`.
The closer `α` is to `1`, the stronger the reduction (and the slower).

## Class structure

```cpp
class LLLoperations {
    static bool checkThreshold(ring_elem num, ring_elem den);
    static bool Lovasz(MutableMatrix *lambda, int k,
                       ring_elem alphaTop, ring_elem alphaBottom);
    static void REDI(int k, int ell, ...);
    // ...
};
```

Operations are static — there is no `LLL` object, just a pile of routines
that operate on a `MutableMatrix*`. The matrix is reduced **in place**, which
is why LLL uses `MutableMatrix` rather than the immutable `Matrix`.

`REDI(k, ell)` is the size-reduction step; `Lovasz(...)` checks the swap
condition; the outer loop coordinates them. The `lambda` matrix tracks the
rational Gram-Schmidt coefficients.

## Inputs

- A `MutableMatrix` over `ZZ` whose columns are the basis vectors.
- The rational threshold `α = alphaTop / alphaBottom`.

## Outputs

The same `MutableMatrix`, modified to hold an LLL-reduced basis.

## Use cases in M2

- Lattice problems in cryptography (occasional research use).
- Finding short integer relations between real numbers (combined with
  `MPFR` rounding tricks).
- Pre-processing for integer programming.

## Related

- [`computations.md`](computations.md) — area overview.
- [`matrices.md`](matrices.md) — `MutableMatrix` lives here.
- [`interface/mutable-matrix.{h,cpp}`](interface/README.md) — public API.
- [`m2/matrix2.m2`](../m2/README.md) — M2-side wrapper.
