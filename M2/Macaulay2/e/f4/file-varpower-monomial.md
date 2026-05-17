# `varpower-monomial.hpp` — F4's sparse `(variable, exponent)` encoding

`varpower-monomial.hpp` is a **legacy specialisation** of the engine's
[`ExponentList`](../file-ExponentList.md) used inside the F4 GB engine.
It encodes a monomial as a sparse sequence of `(variable, exponent)`
pairs with `long`-sized exponents.

Part of the [`f4/`](README.md) subdirectory.

[← f4 overview](README.md) · [← engine overview](../README.md)

## Definition

```cpp
#include "ExponentList.hpp"

// Legacy specialization
using varpower_monomials       = ExponentList<long, false>;
using index_varpower_monomial  = ExponentListIterator<long, false>;

typedef varpower_monomials::Exponent  varpower_word;
typedef varpower_word                *varpower_monomial;
typedef const varpower_word          *const_varpower_monomial;
```

The header is essentially **two `using` declarations and four `typedef`s**.
The heavy lifting lives in [`ExponentList`](../file-ExponentList.md):

- `ExponentList<long, false>` — exponent type `long`, signed.
- `ExponentListIterator<long, false>` — iteration over such a list.

The four `typedef`s give the F4 code a vocabulary (`varpower_monomial`,
`const_varpower_monomial`, `varpower_word`) so F4 sources don't have to
spell out the template instantiation.

## Why "legacy"

The author marks this header as a **legacy specialisation**. The newer
F4 code in [`gb-f4/`](../gb-f4/README.md) builds on
[`MonomialTypes`](../gb-f4/README.md) and `MonomialView` — both
template-friendly types intended to replace `varpower-monomial`. The old
header survives to keep [`f4/`](README.md) buildable.

## Used by

- [`f4/f4.cpp`](README.md) — main F4 algorithm.
- [`f4/f4-spairs.cpp`](file-f4-spairs.md) — S-pair LCM monomials.
- [`f4/moninfo.cpp`](README.md) — `MonomialInfo` uses the same encoding for
  its packed monomial layout.

## Related

- [`README.md`](README.md) — f4 overview.
- [`file-ntuple-monomial.md`](file-ntuple-monomial.md) — sibling dense
  encoding.
- [`../file-ExponentList.md`](../file-ExponentList.md) — the template this
  specialises.
- [`../gb-f4/README.md`](../gb-f4/README.md) — newer monomial-encoding
  approach.
