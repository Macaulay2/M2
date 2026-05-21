# `res-monomial-sorter.{cpp,hpp}` — `MonomialSorterObject`

`MonomialSorterObject` is the engine's **sorter for the columns of a
Macaulay matrix** built during a Schreyer-frame resolution. Sorting
monomials in a Schreyer-order-aware way is the slowest single step per
homological level, so this object exists to centralise it.

Part of the [`schreyer-resolution/`](README.md) subdirectory.

[← schreyer-resolution overview](README.md) · [← engine overview](../README.md)

## State

```cpp
class MonomialSorterObject {
private:
    const Monoid             &mMonoid;
    const std::vector<int*>   mMonoms;
    static long               mNumComparisons;
public:
    MonomialSorterObject(const Monoid &M, const std::vector<int*> monoms)
      : mMonoid(M), mMonoms(monoms) {}
    // ...
};
```

The class owns:

- A reference to a `Monoid` ([`../file-monoid.md`](../file-monoid.md)),
  which supplies the encoded monomial order.
- A `vector<int*>` of pointers to encoded monomials. The sorter doesn't
  copy them — it sorts the *indices* into this vector.
- A static counter `mNumComparisons` for benchmarking and trace output.

## Why a separate class

Sorting `n` monomials with `std::sort` requires `n log n` comparator
calls. Each comparator dispatches through the `Monoid`'s monomial-order
walker, with extra steps for the Schreyer-order tiebreaker. Factoring
the sort into `MonomialSorterObject` lets:

- The comparator capture both the `Monoid` and the index vector in one
  place, so the inner loop is local.
- The counter `mNumComparisons` give the developer feedback on whether
  the algorithm is actually making progress.
- The implementation use `std::stable_sort` (preserving relative order
  of equal monomials, which matters for the Schreyer tiebreaker).

## Memtailor integration

The header pulls in `memtailor/Arena.h`. The sorter doesn't store the
encoded monomials — they live in an Arena-managed buffer owned by the
[`SchreyerFrame`](file-res-schreyer-frame.md). The `int*` pointers in
`mMonoms` are stable for the lifetime of that Arena.

## Used by

- [`F4ResComputation`](file-res-f4-computation.md) — called per
  homological degree to sort the columns of the Macaulay matrix.
- [`SchreyerFrame`](file-res-schreyer-frame.md) — uses it when promoting
  a degree's new generators into the frame.

## Related

- [`README.md`](README.md) — schreyer-resolution overview.
- [`file-res-f4-computation.md`](file-res-f4-computation.md) — primary
  caller.
- [`file-res-schreyer-frame.md`](file-res-schreyer-frame.md) — supplies
  the input monomial pool.
- [`../file-monoid.md`](../file-monoid.md) — supplies the comparator.
- memtailor submodule under [`../../../submodules/README.md`](../../../submodules/README.md).
