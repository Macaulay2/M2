# `monsort.{cpp,hpp}` — generic monomial sorter template

`monsort.hpp` declares a **templated sort routine** for monomial-like
data. It is a thin, ring-agnostic wrapper around `std::sort` that
delegates the per-element comparison to a user-supplied `Sorter` type
matching a small duck-typed interface.

Part of the [Monoids & monomials](monoids-and-monomials.md) area.

[← per-area: monoids-and-monomials](monoids-and-monomials.md) · [← engine overview](README.md)

## Template signature

```cpp
#include <cstdio>
#include <cstdlib>
#include "newdelete.hpp"

#if !defined(SAFEC_EXPORTS)
#include "interface/m2-types.h"
#endif

template <typename Sorter>
// Sorter S, needs to define:
//   - what counts as a "monomial" (the value type)
//   - how to compare two such values
//   - per-element move / swap (defaults usually fine)
// ...
```

The `Sorter` is a tiny adapter: it provides the value type, the
comparison predicate, and (optionally) custom move semantics. The
sort itself dispatches to `std::sort` or `std::stable_sort` depending
on the caller's needs.

## Why a templated wrapper

Engine code sorts monomials in many different places:

- Sort columns of a Macaulay matrix by leading monomial.
- Sort the entries of a polynomial by monomial order.
- Sort basis elements during GB output.

Each call site has slightly different needs: which ordering to use,
whether to keep the original indices, whether stability matters.
`monsort.hpp` parameterises all of this on a `Sorter` adapter so the
sort algorithm itself is one template.

The adapter pattern keeps the comparator local to the call site (and
therefore inlineable), without forcing every consumer to call
`std::sort` directly with a hand-written lambda.

## Used by

- F4 ([`f4/`](f4/README.md)) — sort columns of the Macaulay matrix.
- [`file-comp-res.md`](file-comp-res.md) — sort frame entries by
  Schreyer order.
- Various output paths that need a deterministic monomial order.

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — area overview.
- [`file-monoid.md`](file-monoid.md), [`file-imonorder.md`](file-imonorder.md)
  — supply the comparators that `Sorter`s wrap.
- [`schreyer-resolution/file-res-monomial-sorter.md`](schreyer-resolution/file-res-monomial-sorter.md)
  — a related but resolution-specific sorter.
