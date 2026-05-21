# `monomDL.{hpp,cpp}`, `monomDRL.{hpp,cpp}`, `monomLex.{hpp,cpp}` — per-order monomial specialisations

The three `monomXXX` file pairs provide **monomial-order-specific
specialisations** of the abstract `Monom` interface. Each is
templated only by its specific comparison logic; the rest of the
class is identical.

Part of [`bibasis/`](README.md).

[← bibasis/ overview](README.md) · [← engine overview](../README.md)

## The three orders

| File pair | Class | Order |
|---|---|---|
| `monomDL.{hpp,cpp}` | `MonomDL` | Degree Lex (degree first, lex tie-break) |
| `monomDRL.{hpp,cpp}` | `MonomDRL` | Degree Reverse Lex (degree first, reverse-lex tie-break) |
| `monomLex.{hpp,cpp}` | `MonomLex` | Pure Lex |

All three inherit from the base [`Monom`](file-monom.md) class
and override the comparison method.

## Common header

```cpp
#include <set>
#include "allocator.hpp"
#include "monom.hpp"
```

Each file pulls in:

- `<set>` — for some sorted-set operations.
- [`allocator.hpp`](file-allocator.md) — `FastAllocator` for
  monomial storage.
- [`monom.hpp`](file-monom.md) — base class.

## Why three separate classes instead of one

Two options for handling multiple orders:

1. **One class with a virtual `compare(a, b)`** — clean OO, but
   virtual dispatch on every comparison kills inner-loop
   performance.
2. **Three classes templated by order** — slight code
   duplication, but full inlining of the comparison.

BIBasis picks option 2. Each `MonomXXX` has its own `compare`
defined statically. The templated
[`Involutive<MonomType>`](file-involutive.md) gets monomorphised
per type, and the comparison inlines into the reduction loop.

## How the orders differ

### Lex (`MonomLex`)

Compare from highest-index variable down:

```
x_3 > x_2 > x_1  →  x_3 wins
x_3 = x_3, x_2^2 > x_2  →  x_3 x_2^2 > x_3 x_2
```

Lex is rarely used directly for GB but commonly used for
elimination orders.

### Degree Lex (`MonomDL`)

First compare total degree, break ties with Lex:

```
x_1 x_2 (deg 2) > x_3 (deg 1)
x_1 x_3 (deg 2, Lex breaks) > x_2 x_3 (deg 2)
```

### Degree Reverse Lex (`MonomDRL`)

First compare total degree, break ties with **reverse** Lex:

```
x_1 x_3 (deg 2) > x_2^2 (deg 2)  [since x_1 has the smaller index, x_2^2 has only x_2]
```

DRL is the **default** for most computational algebra — produces
the smallest GBs in practice.

## Each class's responsibilities

Beyond comparison:

- **Multiplication** (`*=`) — for monomial products.
- **Division** (`Divides`) — for monomial divisibility tests.
- **GCD** / **LCM** — entry-wise min/max.
- **Degree** — total degree query.
- **Hash** — for hash-table keys.

The base [`Monom`](file-monom.md) provides default versions; each
order-class overrides what's order-specific.

## Used by

- [`file-launcher.md`](file-launcher.md) — instantiates each one
  per user-chosen order.
- [`file-involutive.md`](file-involutive.md) — templated
  algorithm.
- [`file-polynom.md`](file-polynom.md) — polynomials carry
  monomials of one of these types.

## Related

- [`README.md`](README.md) — bibasis/ overview.
- [`file-monom.md`](file-monom.md) — base class.
- [`file-involutive.md`](file-involutive.md) — templated consumer.
- [`file-launcher.md`](file-launcher.md) — dispatch hub.
