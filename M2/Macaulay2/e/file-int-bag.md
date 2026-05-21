# `int-bag.{cpp,hpp}` — `int_bag` (small integer payload with varpower monomial)

`int-bag.hpp` declares **`int_bag`** — a tiny utility class that pairs
either a single `int` (or `void*`) value with a monomial in varpower
form. It is the engine's grab-bag combination of payload + monomial
used in scattered places where a `(value, monomial)` tuple needs to
travel together.

Part of the [Monoids & monomials](monoids-and-monomials.md) area.

[← per-area: monoids-and-monomials](monoids-and-monomials.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "newdelete.hpp"

class int_bag : public our_new_delete {
    union {
        int   b_elem;
        void *b_ptr;
    } val;

    gc_vector<int> mon;     // varpower representation
    // ...
};
```

Three components:

1. A **payload union** — either an `int` (`b_elem`) or an opaque
   pointer (`b_ptr`). The user knows which it is.
2. A **monomial** stored in **varpower form** (the sparse
   `(variable, exponent)` encoding from
   [`file-ExponentList.md`](file-ExponentList.md)).
3. GC inheritance — `our_new_delete` ([`file-newdelete.md`](file-newdelete.md))
   so the class is heap-managed automatically.

## Why a union payload

Two callers exist:

- **Index-keyed callers** — store an `int` (e.g. polynomial index in
  a basis). Use `val.b_elem`.
- **Pointer-keyed callers** — store a `void *` (e.g. pointer to a
  `Nterm` or `gbvector`). Use `val.b_ptr`.

The union lets one class type serve both. Slightly unsafe (no tag),
but safe enough in practice because the same call site uses a
consistent payload type.

## Used by

`int_bag` appears in the engine wherever an intermediate structure
needs to ferry `(value, monomial)` pairs around — typically during
some construction step that will read both fields in order. Specific
call sites include:

- [`file-monideal.md`](file-monideal.md) — `MonomialIdeal` build
  helpers.
- [`file-monideal-minprimes.md`](file-monideal-minprimes.md) — passes
  `int_bag`s through the recursion.
- Older parts of the GB infrastructure.

## Newer alternative

`int_bag` predates the engine's modern templated-types approach.
Newer code prefers:

- `std::pair<int, varpower_monomial>` directly when in scope.
- Strongly-typed `Index` + `MonomialIndex` from
  [`gb-f4/file-MonomialTypes.md`](gb-f4/file-MonomialTypes.md).

`int_bag` survives because removing it would touch many call sites; a
straightforward win, but low priority.

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — area overview.
- [`file-monideal.md`](file-monideal.md), [`file-monideal-minprimes.md`](file-monideal-minprimes.md)
  — primary consumers.
- [`file-ExponentList.md`](file-ExponentList.md) — varpower encoding.
- [`gb-f4/file-MonomialTypes.md`](gb-f4/file-MonomialTypes.md) —
  modern strongly-typed alternative.
