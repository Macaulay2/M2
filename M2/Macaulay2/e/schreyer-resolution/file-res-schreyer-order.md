# `res-schreyer-order.hpp` — `ResSchreyerOrder`

`res-schreyer-order.hpp` declares **`ResSchreyerOrder`** — the data the
F4 resolution stores per free-module summand to implement the Schreyer
order on the next level. It is the resolution-specialised counterpart
of the engine's general
[`SchreyerOrder`](../file-schorder.md).

Part of the [`schreyer-resolution/`](README.md) subdirectory.

[← schreyer-resolution overview](README.md) · [← engine overview](../README.md)

## Data

```cpp
#include "schreyer-resolution/res-monomial-types.hpp"  // for component_index
#include <vector>

struct ResSchreyerOrder {
    std::vector<res_packed_monomial>  mTotalMonom;
    std::vector<component_index>      mTieBreaker;
    // keep a memory block for these monomials?  Probably...
};
```

For each generator of the current free module:

- **`mTotalMonom[i]`** — the **total monomial** of the *i*-th generator,
  i.e. the encoded "leading monomial" that defines the Schreyer order
  at this level.
- **`mTieBreaker[i]`** — a `component_index` (an int) that resolves
  ties when two `mTotalMonom`s are equal.

## How the order is used

Comparing two basis elements `e_i · m` and `e_j · m'`:

```text
compare(mTotalMonom[i] · m,  mTotalMonom[j] · m')   (ambient monoid order)
   if equal: tiebreak by mTieBreaker[i] vs mTieBreaker[j]
```

This makes the order on the free module "remember" how each generator
arose as a syzygy — the trick that keeps leading-term arithmetic local
to each homological degree.

## Operations (per the header comment)

The header lists the planned operations:

- create total monomials and tie-breakers (or just tie-breakers, given
  total monomials)
- check that a polynomial is in correct descending order w.r.t. this
  order
- sort a polynomial into this order
- provide a comparison operator for two monomials (not in total
  monomial encoding)
- (debug) display data associated to this order

Some of these are implemented in
[`file-res-monomial-sorter.md`](file-res-monomial-sorter.md) and
[`file-res-schreyer-frame.md`](file-res-schreyer-frame.md); others are
planned (the "keep a memory block for these monomials?" comment is
left open).

## Compared to top-level `SchreyerOrder`

| Aspect | `ResSchreyerOrder` (this file) | [`SchreyerOrder`](../file-schorder.md) |
|---|---|---|
| Storage | Plain `std::vector` | `gc_vector<int>` |
| Monomial type | `res_packed_monomial` | Top-level encoded monomial |
| Use | Inside F4 resolution | General-purpose engine free modules |

Same idea, different storage tuned to the resolution path.

## Related

- [`README.md`](README.md) — schreyer-resolution overview.
- [`file-res-schreyer-frame.md`](file-res-schreyer-frame.md) — primary
  consumer.
- [`file-res-monomial-sorter.md`](file-res-monomial-sorter.md) — sorts
  monomials under this order.
- [`../file-schorder.md`](../file-schorder.md) — general-purpose
  counterpart.
- [`file-res-moninfo.md`](file-res-moninfo.md) — `res_packed_monomial`
  type definition.
