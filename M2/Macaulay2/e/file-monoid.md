# `monoid.{cpp,hpp}` — the `Monoid` class

`Monoid` is the engine's representation of the multiplicative side of a
polynomial ring: a finite set of named variables together with a monomial
ordering and (multi-)grading.

This file is part of the [Monoids & monomials](monoids-and-monomials.md)
area. See [`imonorder.cpp`](imonorder.cpp) for the encoded internal form, and
[`monorder.cpp`](monorder.cpp) for the user-facing ordering description.

[← per-area: monoids-and-monomials](monoids-and-monomials.md) · [← engine overview](README.md)

## State

A `Monoid` carries:

- A pointer to a **degree monoid** (`mDegreeMonoid`) and a **degree ring**
  (`mDegreeRing`) — degrees of variables themselves live in another monoid
  recursively; the base case is the trivial monoid.
- The user-facing **`MonomialOrdering`** (`mo_`).
- The encoded **`MonomialOrder`** (`monorder_`) — the runtime-friendly form
  produced by [`imonorder.cpp`](imonorder.cpp).
- The **variable count** (`mVariableCount`), **names** (`mVariableNames`),
  per-variable **degree vectors** flattened into `mDegrees`, the **heft
  vector** (`mHeftVector`) used to detect bounded-degree subsets, and the
  per-variable **heft degrees** (`mHeftDegrees`).
- The monomial **byte layout**: `monomial_size()` returns the size in `int`s
  of an encoded monomial in this monoid.

The class inherits from `MutableEngineObject` — its identity is stable across
its lifetime even though the engine may attach derived data to it.

## Why two orderings

The split between `MonomialOrdering` and `MonomialOrder` is deliberate:

- `MonomialOrdering` is the *declarative* form — what the user wrote
  (`Lex`, `GRevLex`, weight blocks). It survives M2-side serialisation.
- `MonomialOrder` is the *operational* form — an encoded byte sequence the
  inner loop walks to compare two monomials. Computing it is a one-time cost
  per monoid.

[`imonorder.cpp`](imonorder.cpp) is the translator between the two.

## Monomial allocation idioms

Engine code allocates monomials on the **stack** for the duration of a single
operation:

```c
monomial m = ALLOCATE_MONOMIAL(MONOMIAL_BYTE_SIZE(M->monomial_size()));
```

The `alloca`-based macros (`ALLOCATE_MONOMIAL`, `ALLOCATE_EXPONENTS`) are
defined at the top of [`monoid.hpp`](monoid.hpp). Stack allocation is
critical for performance: a typical GB step builds thousands of transient
monomials per pair.

## Hot-path operations

- `multiply(a, b, out)` — multiply two monomials.
- `divide(a, b, out)` — quotient, used to test divisibility.
- `compare(a, b)` — encoded ordering comparison; tiny inline loop.
- `to_expvector(m, exp)` / `from_expvector(exp, m)` — convert encoded ↔ dense
  exponent vector.

All of these are templated on the encoded layout for speed.

## How it connects

```
PolynomialRing                ← uses
   └── Monoid                  ← this file
        ├── MonomialOrdering   ← user-facing
        ├── MonomialOrder      ← imonorder.cpp
        └── degree-ring view   ← recursive
```

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — area overview.
- [`imonorder.cpp`](imonorder.cpp), [`monorder.cpp`](monorder.cpp) — ordering layers.
- [`file-polyring.md`](file-polyring.md) — `PolynomialRing` holds a `Monoid*`.
- [`interface/monoid.{h,cpp}`](interface/README.md) — public C interface.
