# `res-f4-monlookup.{cpp,hpp}` — `ResF4MonomialLookupTableT<Key>`

`res-f4-monlookup.hpp` defines a **templated tree-structured monomial
index** for the F4 resolution. It is the resolution counterpart of
[`f4/file-f4-monlookup.md`](../f4/file-f4-monlookup.md), specialised on
the resolution-side monomial types declared in
[`file-res-monomial-types.md`](file-res-monomial-types.md).

Part of the [`schreyer-resolution/`](README.md) subdirectory.

[← schreyer-resolution overview](README.md) · [← engine overview](../README.md)

## Class shape

```cpp
#include "newdelete.hpp"
#include "schreyer-resolution/res-moninfo.hpp"
#include "schreyer-resolution/res-monomial-types.hpp"

template <typename Key>
class ResF4MonomialLookupTableT : public our_new_delete {
    typedef res_varpower_word          varpower_word;
    typedef res_varpower_monomial      varpower_monomial;
    typedef res_const_varpower_monomial const_varpower_monomial;

    typedef res_ntuple_word            ntuple_word;
    typedef res_ntuple_monomial        ntuple_monomial;
    typedef res_const_ntuple_monomial  const_ntuple_monomial;

    typedef res_packed_monomial        packed_monomial;
    // ...
};
```

The class is **templated on `Key`**, which is the payload type
returned by lookup queries. The internal monomial encoding is fixed
(`res_*_monomial` from
[`file-res-monomial-types.md`](file-res-monomial-types.md)).

## What the class provides

Functionally identical to
[`f4/file-f4-monlookup.md`](../f4/file-f4-monlookup.md) — a tree-
structured index over a monomial ideal:

- **`insert(monomial, key)`** — register a (monomial, payload) pair.
- **`find_divisor(monomial)`** — return any payload whose monomial
  divides the input, or no-match.

The data structure is a binary-tree variant of a Janet tree, with
nodes splitting on `(variable, exponent)` tests.

## Why a separate file from `f4/`

The two trees would functionally be the same template with different
`typedef` choices. They are kept separate because:

- **Independence of evolution** — the F4 path and the resolution path
  evolve at different rates; sharing one file would couple them.
- **Different `Key` usage** — F4 uses `int` (basis index); the
  resolution uses a richer `Key` carrying level + index information.
- **Different monomial encoding choices** — F4 freely mixes
  `ntuple` and `varpower`; the resolution standardises on the
  `res_*` variants.

A future refactor may unify these; the duplicate exists today.

## Used by

- [`file-res-f4-computation.md`](file-res-f4-computation.md) — leading-
  word index for the in-progress basis.
- [`file-res-f4.md`](file-res-f4.md) — same.
- [`file-res-schreyer-frame.md`](file-res-schreyer-frame.md) — frame
  uses lookup queries to find reducers.

## Related

- [`README.md`](README.md) — schreyer-resolution overview.
- [`file-res-monomial-types.md`](file-res-monomial-types.md) — typedef
  source.
- [`../f4/file-f4-monlookup.md`](../f4/file-f4-monlookup.md) — sibling
  F4 implementation.
- [`../file-montable.md`](../file-montable.md) — older non-tree GB
  lookup.
