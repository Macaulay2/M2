# `ExponentVector.hpp` — dense exponent-vector template

`ExponentVector` is the engine's **dense exponent-vector encoding** for
monomials: a fixed-length array of `Exponent`s indexed by variable
position. It complements
[`ExponentList`](file-ExponentList.md), the sparse encoding.

Part of the [Monoids & monomials](monoids-and-monomials.md) area.

[← per-area: monoids-and-monomials](monoids-and-monomials.md) · [← engine overview](README.md)

## Template signature

```cpp
#include <assert.h>
#include <string.h>     // for memcpy
#include <type_traits>  // for make_unsigned
#include <vector>

#include "overflow.hpp"  // for add, mult, sub, sub_pos
#include "style.hpp"     // for EQ, GT, LT
#include "buffer.hpp"
#include "util.hpp"

template <typename ExponentT, bool overflowChecked>
class ExponentVector {
public:
    typedef ExponentT  Exponent;
    typedef Exponent  *Exponents;
    typedef const Exponent *ConstExponents;
    // ...
};
```

Two template parameters:

- **`ExponentT`** — the exponent type (commonly `int`, `int32_t`,
  `int64_t`).
- **`overflowChecked`** — whether arithmetic uses
  [`file-overflow.md`](file-overflow.md)'s checked helpers (`true`) or
  bypasses them for speed (`false`).

The class template is the foundation for several concrete monomial
encodings:

| Specialisation | Files |
|---|---|
| `ExponentVector<int64_t, false>` | [`f4/file-ntuple-monomial.md`](f4/file-ntuple-monomial.md) (F4 dense) |
| `ExponentVector<myword, false>` | [`schreyer-resolution/file-res-monomial-types.md`](schreyer-resolution/file-res-monomial-types.md) (resolution dense) |
| `ExponentVector<int, true>` (when explicit checks needed) | Engine top-level monoid code |

## Layout

A dense exponent vector is just `[e_0, e_1, …, e_{n-1}]`:

```text
[e_0, e_1, e_2, …, e_{n-1}]
```

where `e_i` is the exponent of variable `i`. **Length is fixed** at the
ambient ring's variable count; there is no length prefix. Code working
with `ExponentVector`s typically carries `nvars` alongside as context.

## Operations (per the header)

The template uses `overflow.hpp` for safe arithmetic when
`overflowChecked = true`:

```cpp
template <typename E, bool C>
class ExponentVector {
    void multiply(...);    // out = a + b, component-wise
    void divide(...);      // out = a - b
    bool divides(...);     // all(a <= b)?
    int  compare(...);     // LT / EQ / GT under the chosen ordering
    void hash(...);
};
```

The `overflow.hpp` helpers (`add`, `mult`, `sub`, `sub_pos`) come from
[`file-overflow.md`](file-overflow.md). With `overflowChecked = false`,
the template skips them — appropriate inside tight inner loops where
overflow has already been bounded by a higher-level check.

## When to choose dense vs. sparse

| Monomial profile | Choose |
|---|---|
| Many variables, low support per monomial | [`ExponentList`](file-ExponentList.md) |
| Few variables, dense exponents | `ExponentVector` (this file) |
| Mixed | One per subsystem; F4 keeps both |

The engine often instantiates both, picking the dense form for
multiplication-heavy inner loops and the sparse form for storage.

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — area overview.
- [`file-ExponentList.md`](file-ExponentList.md) — sparse counterpart.
- [`f4/file-ntuple-monomial.md`](f4/file-ntuple-monomial.md), [`schreyer-resolution/file-res-monomial-types.md`](schreyer-resolution/file-res-monomial-types.md)
  — concrete specialisations.
- [`file-overflow.md`](file-overflow.md) — checked arithmetic helpers.
