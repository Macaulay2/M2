# `monom.{cpp,hpp}` and the `monom*` variants

`monom.hpp` declares the abstract `Monom` class in `namespace BIBasis`. The
three sibling headers — [`monomLex.hpp`](README.md), [`monomDL.hpp`](README.md),
[`monomDRL.hpp`](README.md) — are concrete specialisations for the three
monomial orderings the algorithm supports.

Part of the [`bibasis/`](README.md) subdirectory.

[← bibasis overview](README.md) · [← engine overview](../README.md)

## Abstract `Monom`

```cpp
namespace BIBasis {

class Monom {
public:
    typedef short int Integer;
    enum Order { Lex, DegLex, DegRevLex };
    // ...
};

}
```

`Integer` is `short int` — exponents are small because every monomial in
the Boolean ring is squarefree (each exponent is 0 or 1) and the variable
count fits in a short. Choosing `short` keeps monomials cache-friendly.

The `Order` enum is informational; the *concrete* monomial classes
implement one ordering each.

## Three orderings

| Header | Class | Ordering |
|---|---|---|
| `monomLex.hpp` | `MonomLex` | Pure lexicographic |
| `monomDL.hpp` | `MonomDL` | Degree, then lex |
| `monomDRL.hpp` | `MonomDRL` | Degree, then reverse-lex |

Each provides the same interface (multiplication, divisibility, comparison)
but uses an ordering-specialised internal storage. The bibasis algorithm
templates on `MonomType`, so the choice is made at compile time.

## Allocation

Monomials allocate from [`allocator.{cpp,hpp}`](README.md) — a pool
allocator specialised for the small fixed-size objects bibasis creates.
The header includes `allocator.hpp` directly.

## Why three classes

Bibasis's prolongation loop generates a lot of monomials, and the cost of
comparison dominates run-time. Three concrete classes let the compiler inline
the comparison routine for each ordering. A single virtual class would lose
the inlining and slow the inner loop significantly.

## Related

- [`README.md`](README.md) — bibasis overview.
- [`file-bibasis.md`](file-bibasis.md) — driver.
- [`file-janettree.md`](file-janettree.md), [`file-polynom.md`](file-polynom.md)
  — types templated on `MonomType`.
- [`allocator.{cpp,hpp}`](README.md) — memory pool.
