# `monomial-sets.hpp` — fixed-size and variable-size monomial sets

`monomial-sets.hpp` defines **set / hash-set data structures for
monomials**. Each set stores monomials of either a fixed size (passed
as a template parameter) or variable size (with a length prefix).
Hashing is over the full monomial contents — including length, for the
variable-size case.

Part of the [Monoids & monomials](monoids-and-monomials.md) area.

[← per-area: monoids-and-monomials](monoids-and-monomials.md) · [← engine overview](README.md)

## Monomial conventions

The header is explicit about what counts as a monomial here:

```text
There are two types of monomials:
 a. fixed size (and that size is passed in as a parameter)
 b. variable size, in which case the first int is the total length
    (so the range is [m, m + *m).

Here are some assumptions about these monomials:
 1. each monomial is a contiguous sequence of int's.
 2. each monomial is hashed using all entries of the monomial.
    Equality is equality of all elements.
    (and size, if variable length).
```

These conventions mean:

- The data is **opaque ints** — the set doesn't know about variable
  positions, exponents, or orderings. It just compares and hashes
  byte sequences.
- For variable-size monomials, the **length is part of the key** —
  `[3, 4, 5]` and `[5, 4, 5]` (lengths 3 and 5) are distinct keys
  even though they share the first three ints.

## Header includes

```cpp
#include "memtailor.h"
#include <unordered_set>
```

Uses:

- **`memtailor`** — vendored bump allocator
  ([`file-MemoryBlock.md`](file-MemoryBlock.md) wraps it). Monomials
  the set stores live in a memtailor arena.
- **`std::unordered_set`** — standard library hash set, parameterised
  on engine-specific hash and equality predicates declared further in
  the header.

## Two main classes

| Class | Monomial type |
|---|---|
| `FixedSizeMonomialSet<NWords>` | Each monomial is exactly `NWords` ints |
| `VariableSizeMonomialSet`      | Each monomial's first int is its length |

Both expose:

- `insert(monomial) → MonomialIndex` — insert if new; return existing
  index if found.
- `lookup(monomial) → optional<MonomialIndex>` — find without insert.
- `monomial_at(index) → ptr`        — recover the stored monomial.

## Where this is used

- **F4 resolution** — the column-set of a Macaulay matrix is a
  monomial set; columns are indexed by `MonomialIndex`.
- **GB algorithms** — for deduplication of leading monomials.
- **`gb-f4/`** — alternative to the broader
  [`MonomialHashTable`](gb-f4/file-MonomialHashTable.md) for special
  cases.

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — area overview.
- [`file-monoid.md`](file-monoid.md) — produces the monomials this
  set stores.
- [`gb-f4/file-MonomialHashTable.md`](gb-f4/file-MonomialHashTable.md)
  — richer index used by newer F4 code.
- [`f4/file-monhashtable.md`](f4/file-monhashtable.md) — older F4
  monomial-hash traits.
- memtailor submodule under [`../../submodules/README.md`](../../submodules/README.md).
