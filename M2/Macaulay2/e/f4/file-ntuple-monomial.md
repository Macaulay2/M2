# `ntuple-monomial.hpp` — F4's dense exponent-vector encoding

`ntuple-monomial.hpp` is a **legacy specialisation** of
[`ExponentVector`](../file-ExponentList.md) used inside the F4 GB engine.
It encodes a monomial as a dense `int64_t` array of exponents indexed by
variable.

Part of the [`f4/`](README.md) subdirectory.

[← f4 overview](README.md) · [← engine overview](../README.md)

## Definition

```cpp
#include "ExponentVector.hpp"

// CAVEAT: NO overflow checking is done with this class.
// TODO: make this file obsolete

// Legacy specialization
using ntuple_monomials = ExponentVector<int64_t, false>;

typedef ntuple_monomials::Exponent  ntuple_word;
typedef ntuple_word                *ntuple_monomial;
typedef const ntuple_word          *const_ntuple_monomial;
```

The header is essentially a single `using` plus three `typedef`s. The
template parameters:

- **`int64_t`** — exponent type. 64-bit signed; chosen because monomials
  in F4 can reach high total degrees during reduction.
- **`false`** — the second template flag disables overflow checking. The
  CAVEAT above is explicit: **no overflow protection**.

## The lack of overflow checking

The header's CAVEAT is important. Most engine monomial arithmetic uses the
overflow-checked helpers in [`file-overflow.md`](../file-overflow.md);
this header opts out. The justification is performance — F4's inner loop
multiplies millions of monomials per second, and the overflow path adds a
branch per multiply.

The trade-off is acceptable here because F4 is supposed to fail loud at a
*higher* level — sugar tracking, [`GBWeight`](../file-gbweight.md), and
degree checks catch unreasonably large degrees before they reach the
monomial layer. If you're touching this header, be careful about preserving
that boundary.

## Custom `mask`

The header also overrides one helper from the base template:

```cpp
template <>
inline ntuple_monomials::HashExponent ntuple_monomials::mask(int nvars,
                                                              ConstExponents a)
{
    HashExponent result = 0;
    int i;
    size_t j;
    for (i = 0, j = 0; i < nvars; i++, j++) {
        if (j == 8 * sizeof(HashExponent)) j = 0;
        if (a[i] > 0) result |= (1 << j);
    }
    return result;
}
```

`mask(...)` produces a **bitmask hash** of the exponent vector — one bit
per variable, set iff the exponent is positive. Used by F4 for cheap
divisibility pre-filtering: if `mask(a) & mask(b) != mask(b)`, then `b`
cannot divide `a`. The pre-filter eliminates the majority of negative
divisibility tests before the more expensive exponent comparison.

## TODO

The header marks itself with `// TODO: make this file obsolete` — the
intent is to migrate F4 to use [`ExponentVector`](../file-ExponentList.md)
directly.

## Related

- [`README.md`](README.md) — f4 overview.
- [`file-varpower-monomial.md`](file-varpower-monomial.md) — sibling sparse
  encoding.
- [`../file-ExponentList.md`](../file-ExponentList.md) — `ExponentVector`
  base template.
- [`../file-overflow.md`](../file-overflow.md) — the overflow-checked
  alternative this header bypasses.
