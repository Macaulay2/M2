# `f4.{cpp,hpp}` — `F4GB` (the F4 algorithm itself)

`f4.cpp` is **the F4 algorithm** — the inner-loop class `F4GB` that
implements Faugère's linear-algebra Gröbner basis algorithm. The
top-level dispatch class [`F4Computation`](file-f4-computation.md)
delegates here.

Part of the [`f4/`](README.md) subdirectory.

[← f4 overview](README.md) · [← engine overview](../README.md)

## Header design notes

The header opens with a list of the algorithm's requirements:

```text
My implementation of Faugere's linear algebra GB routines. Also includes
free resolution code.

Template parameters include:
    coefficient ring arithmetic
    packed_monomial
    exponents
    varpower_monomial

Types to define:
    MonomialLookupTable
      a. find_divisor(packed_monomial, comp) → integer whose lead term
         divides packed_monomial
      b. insert(packed_monomial, comp, index)
    packed_monomial
      implemented as packed exponent vector, perhaps with weight vector(s)
```

This sketches the **template machinery** that lets `F4GB` instantiate
against any coefficient ring's arithmetic. The choices made here
parallel the refactored gb-f4 design but are looser — the older F4 was
written before the engine had the templated arithmetic infrastructure.

## What F4 does

```text
loop until no new S-pairs:
    select a set of S-pairs (a "batch")
    compute their S-polynomials
    collect all monomials that appear in any S-pair or its reducers
    build a Macaulay matrix:
        rows  = S-polynomials + tail-reducer polynomials
        cols  = all collected monomials, sorted by monomial order
    reduce the matrix to row-echelon form
    extract new basis elements from echelon rows whose leading
        column was previously unrepresented
    generate new S-pairs, prune subsumed ones
```

The class members track:

- The current basis (`gb_array`).
- The S-pair queue ([`F4SPairSet`](file-f4-spairs.md)).
- The monomial table ([`MonomialInfo`](file-moninfo.md)).
- The Macaulay-matrix workspace.
- Hilbert function tracking (`hilb-fcn.cpp`) for early-exit.

## `F4toM2Interface`

All translation between engine `Matrix` / `vec` types and F4's internal
representations goes through
[`F4toM2Interface`](file-f4-m2-interface.md). `F4GB` itself only touches
its native types.

## Difference from the new F4

The newer [`gb-f4/`](../gb-f4/README.md) restructures the same algorithm
with cleaner factoring. The two coexist; the older `F4GB` is the
production path today, with `gb-f4/` slated to replace it.

## Related

- [`README.md`](README.md) — f4 overview.
- [`file-f4-computation.md`](file-f4-computation.md) — top-level glue.
- [`file-f4-spairs.md`](file-f4-spairs.md), [`file-moninfo.md`](file-moninfo.md),
  [`file-f4-m2-interface.md`](file-f4-m2-interface.md) — internals.
- `hilb-fcn.{cpp,hpp}` — Hilbert-function early-exit support.
- [`../gb-f4/README.md`](../gb-f4/README.md) — replacement under
  development.
