# `Basis.{cpp,hpp}` — the evolving Gröbner basis (new F4)

`Basis` is the storage for the **current Gröbner basis** during a run of
the refactored F4 algorithm. It is the modern counterpart of `gb_array` in
[`f4/`](../f4/README.md) — same role, cleaner layout.

Part of the [`gb-f4/`](README.md) subdirectory.

[← gb-f4 overview](README.md) · [← engine overview](../README.md)

## Status enum

Each basis element carries a status:

```cpp
namespace newf4 {

enum class GBPolyStatus { Gen, MinGen, MinGB, NonMinGB, Retired };

inline std::string toString(GBPolyStatus s);

}
```

| Status | Meaning |
|---|---|
| `Gen` | Input generator, not yet evaluated as minimal |
| `MinGen` | Confirmed minimal generator of the input ideal |
| `MinGB` | Confirmed minimal Gröbner basis element |
| `NonMinGB` | Was in the GB, but a later element subsumes it |
| `Retired` | Removed from the active basis (kept for history) |

These flags drive the reporters that the interpreter polls — `get_mingens`,
`get_gb`, `get_change`, etc. (See [`file-GBF4Computation.md`](file-GBF4Computation.md).)

## Storage

`Basis` keeps:

- A reference to a [`MonomialHashTable`](README.md) so it can store each
  element's leading monomial by index rather than by value.
- A vector of `(polynomial, leading monomial idx, status)` tuples.
- An index into the active prefix (everything before `mActiveEnd` is
  considered for future S-pairs).

This is enough state for the high-level F4 loop to:

1. Look up the leading monomial of every active basis element.
2. Determine which polynomials should be tail-reducers in the next
   [`MacaulayMatrix`](file-MacaulayMatrix.md).
3. Update statuses when a new element subsumes an old one.

## Parallel safety

`Basis` is currently single-writer. Parallelisation in the new F4 happens
inside the matrix-reduction phase (across rows), not inside basis updates.
TBB primitives ([`m2tbb.hpp`](../README.md)) are pulled into the bigger
classes that use `Basis` rather than into `Basis` itself.

## Related

- [`README.md`](README.md) — gb-f4 overview.
- [`file-GBF4Computation.md`](file-GBF4Computation.md) — top-level driver.
- [`file-MacaulayMatrix.md`](file-MacaulayMatrix.md) — consumer of basis
  monomials.
- `PolynomialList.{cpp,hpp}` — type of polynomial stored.
- [`../f4/README.md`](../f4/README.md) — older F4 with the prior
  `gb_array` design.
