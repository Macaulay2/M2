# `montableZZ.{cpp,hpp}` — `MonomialTableZZ` (ZZ-coefficient leading-monomial index)

`MonomialTableZZ` is the **ZZ-coefficient analogue** of
[`MonomialTable`](file-montable.md). It indexes leading monomials of a
Gröbner basis over `ZZ` and answers divisibility queries — including
the **coefficient** check that distinguishes the ZZ case from the
field case.

Part of the [Monoids & monomials](monoids-and-monomials.md) area.

[← per-area: monoids-and-monomials](monoids-and-monomials.md) · [← engine overview](README.md)

## Why a separate ZZ table

Over a field, divisibility of leading terms is purely about the
monomial: `lt(g) | lt(f)` iff `g`'s exponent vector is component-wise
≤ `f`'s. Over `ZZ`, the leading **coefficient** also matters: `lt(g)`
divides `lt(f)` iff its monomial divides AND its integer coefficient
divides.

[`MonomialTable`](file-montable.md) doesn't track coefficients —
suitable for fields. `MonomialTableZZ` adds the coefficient layer:
each entry records `(exponent_vector, coefficient, polynomial_index)`,
and the lookup runs both checks.

## Layout

```cpp
#include <vector>
#include <memory>
#include <algorithm>

#include "ExponentVector.hpp"
#include "buffer.hpp"
#include "newdelete.hpp"
#include "style.hpp"

/* "Tricks" used in this implementation */
```

The header note at the top hints at the encoding choices: monomials
are stored as exponent vectors (variable-length, the same as
`MonomialTable`), with the leading coefficient stored alongside as an
`mpz_t` reference.

## Operations

- **`insert(exp, coeff, polynomial_index)`** — register a (monomial,
  coefficient) pair.
- **`find_divisor(exp, target_coeff)`** — return the polynomial index
  if some entry's `(exp, coeff)` divides `(target_exp, target_coeff)`.
  Returns no-match otherwise.
- **`remove(exp, coeff)`** — drop an entry.

Each operation is `O(n)` in the worst case (linear scan with cheap
predicate); typical case is much faster because of the monomial pre-
filter.

## Used by

- [`file-reducedgb.md`](file-reducedgb.md)'s `ReducedGB_ZZ` — wraps a
  `MonomialTableZZ` to canonicalise GBs over `ZZ`.
- [`file-gb-default.md`](file-gb-default.md) (`gbA`) when the
  coefficient ring is `ZZ` — the inner reduction loop uses the ZZ
  table for finding reducers.

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — area overview.
- [`file-montable.md`](file-montable.md) — field-coefficient sibling.
- [`file-ExponentVector.md`](file-ExponentVector.md) — encoding helper.
