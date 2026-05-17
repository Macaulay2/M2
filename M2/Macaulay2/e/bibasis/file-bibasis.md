# `bibasis.{cpp,hpp}` — `BIBasis` driver

`bibasis.cpp` is the top-level driver for the **Boolean Involutive Basis**
algorithm in this subdirectory. It coordinates the
[`Monom`](file-monom.md)-typed value layer, the
[`JanetTree`](file-janettree.md) involutive-division data structure, and the
[`Polynom`](file-polynom.md) polynomial type to produce an involutive basis
of an ideal over `F_2[x_1,…,x_n] / (x_i^2 - x_i)`.

Part of the [`bibasis/`](README.md) subdirectory.

[← bibasis overview](README.md) · [← engine overview](../README.md)

## Algorithm

Given a generating set `F ⊂ F_2[x_1,…,x_n]`, the algorithm:

1. **Reduces** every input polynomial modulo the Boolean relations
   `x_i^2 = x_i`. Each polynomial becomes a sum of squarefree monomials.
2. **Inserts** each polynomial into a [`JanetTree`](file-janettree.md), an
   involutive-division data structure indexed by the leading monomial.
3. **Processes prolongations**: for each polynomial and each non-multiplicative
   variable, multiply, reduce, and re-insert. Repeat until no new
   polynomials appear.
4. **Outputs** the resulting involutive basis as a sequence of
   [`Polynom`](file-polynom.md) values.

The output is **larger** than a reduced Gröbner basis but enables certain
later computations (membership tests, certain quotient-ring constructions)
to run in linear time.

## Namespace

All bibasis code lives in `namespace BIBasis` to keep its `Monom`,
`Polynom`, and `Triple` types from colliding with the engine's primary
monomial layer.

## Inputs and outputs

- **Input**: a list of polynomials (each as a sequence of squarefree
  monomials, since arithmetic happens in the Boolean ring).
- **Output**: a list of `Polynom<MonomType>` values forming the involutive
  basis.

The user-facing entry point is exposed through the [`BIBasis`](../../packages/BIBasis.m2)
M2 package.

## Termination

For Boolean ideals, involutive bases are guaranteed to terminate (the ideal
has only finitely many squarefree monomials in `n` variables, bounded by
`2^n`).

## Related

- [`README.md`](README.md) — bibasis overview.
- [`file-monom.md`](file-monom.md), [`file-janettree.md`](file-janettree.md),
  [`file-polynom.md`](file-polynom.md) — supporting types.
- [`launcher.{cpp,hpp}`](README.md) — wrapper that exposes this to M2.
- [`../groebner-bases.md`](../groebner-bases.md) — for general-purpose GB
  contrast.
