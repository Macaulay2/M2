# `GBF4Computation.{cpp,hpp}` — refactored F4 Computation

`GBF4Computation` is the top-level class for the **new F4** implementation
in [`gb-f4/`](README.md). It is the modern counterpart of
[`f4/file-f4-computation.md`](../f4/file-f4-computation.md) — both
subclass [`GBComputation`](../file-comp-gb.md), but with cleaner
separation of concerns.

Part of the [`gb-f4/`](README.md) subdirectory.

[← gb-f4 overview](README.md) · [← engine overview](../README.md)

## Namespace

All `gb-f4/` classes live in `namespace newf4` to avoid name collisions
with their older counterparts at `f4/`. Search the codebase for
`newf4::GBF4Computation` to find call sites.

## State

```cpp
namespace newf4 {

class GBF4Computation {
private:
    MonomialHashTable mBasisMonomials;   // monomials seen in the basis
    PolynomialList    mInput;            // input polynomials
    Basis             mGBSoFar;          // evolving GB
    const FreeModule *mFreeModule;       // debugging info only
    // ...
};

}
```

Three big pieces of state, each in its own file:

- **`MonomialHashTable`** ([`file-MacaulayMatrix.md`](file-MacaulayMatrix.md)
  consumers; see also [`file-MonomialHashTable`](README.md)) — tracks
  all monomials seen across the basis.
- **`PolynomialList`** ([`PolynomialList.cpp`](README.md)) — the original
  generators.
- **`Basis`** ([`file-Basis.md`](file-Basis.md)) — the evolving Gröbner
  basis, with status flags per element.

The clean split between these is what distinguishes `gb-f4/` from
`f4/` — the older code merges concerns across files.

## High-level loop

Same shape as the old F4, factored differently:

```text
for d = 1, 2, …:
    add input polynomials of degree d to basis
    select all overlaps / S-pairs of degree d
    build MacaulayMatrix M (Basis ∪ SPairs ∪ reducers)
    reduce M to row echelon form via VectorArithmetic
    for each new nonzero row:
        promote it to a basis element, update Basis status
    prune subsumed S-pairs
```

The hooks that drive this loop are exposed through the inherited
`GBComputation` API (`start_computation()`, etc.).

## Strategy enum

```cpp
enum class Strategy;
```

Forward-declared in the header; concrete values control variant choices
(buffer size, parallelism, monomial layout). Definitions are in the `.cpp`.

## Related

- [`README.md`](README.md) — gb-f4 overview.
- [`file-Basis.md`](file-Basis.md) — basis storage.
- [`file-MacaulayMatrix.md`](file-MacaulayMatrix.md) — the matrix reduced
  each step.
- [`../file-comp-gb.md`](../file-comp-gb.md) — `GBComputation` base.
- [`../f4/file-f4-computation.md`](../f4/file-f4-computation.md) — older
  F4 counterpart.
