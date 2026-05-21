# `monoid.{h,cpp}` (in `interface/`) — public C entry points for `Monoid`

`interface/monoid.h` declares the **public C functions** the interpreter
uses to construct and query the engine's [`Monoid`](../file-monoid.md) class.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#if defined(__cplusplus)
class Ring;
class Monoid;
class MonomialOrdering;
#else
typedef struct Ring             Ring;
typedef struct Monoid           Monoid;
typedef struct MonomialOrdering MonomialOrdering;
#endif
```

Dual-mode forward declarations as in every `interface/*.h`. The `// TODO:
make this unnecessary` comment marks this as a candidate for replacement
by a cleaner header arrangement once the engine.h migration is complete.

## Entry points

- **Construction** — `rawMonoid(...)` builds a `Monoid*` from a
  user-supplied monomial ordering, variable names, degree vectors, and a
  degree monoid (a pointer to another `Monoid*` whose elements are the
  degrees of this one).
- **Inspection** — `rawNumberOfVariables(M)`, `rawDegrees(M)`,
  `rawMonomialOrdering(M)`, `rawGetVariableName(M, i)`.

The constructor is by far the most complex `raw…` entry point in the
engine — monoid construction takes ~10 separate inputs (orderings,
weights, components, heft, names, …). The bulk of the validation lives in
`monoid.cpp`.

## Why the degree-monoid recursion

A `Monoid`'s degrees live in *another* monoid. Most of the time that other
monoid is `ZZ^n` (the standard multi-degree case), but it can be any
finitely-generated abelian group expressible as a monoid. The base case is
the trivial monoid.

This recursion is built up through chained calls to `rawMonoid(...)` from
M2 code.

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-monoid.md`](../file-monoid.md) — `Monoid` implementation.
- [`../monoids-and-monomials.md`](../monoids-and-monomials.md) — monoid /
  monomial area overview.
- `file-monomial-ordering-interface.md` (forthcoming) — sister header for
  monomial orderings.
- [`../file-imonorder.md`](../file-imonorder.md) — encoded ordering form.
