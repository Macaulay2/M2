# `dpoly.{cpp,hpp}` — univariate polynomials over QQ extensions and finite fields

`dpoly.cpp` implements **univariate polynomial arithmetic over
algebraic extensions of `QQ`** and over **finite fields**. It supports
the GCD and modular GCD operations the engine needs for factorisation,
extension construction, and similar tasks.

Part of the [Computations](computations.md) area.

[← per-area: computations](computations.md) · [← engine overview](README.md)

## Scope

The header is explicit about scope:

```cpp
// Code for univariate polynomials over algebraic extensions of QQ
// and over finite fields

// The basic operations:
//   "monic gcd mod p" over extension fields
//   modular gcd algorithm
// Later, we will extend this to multivariate polynomials and function fields
```

Today it covers:

- **Monic GCD mod p** — over a finite extension field.
- **Modular GCD** — for univariate polynomials over `QQ`, lift the
  GCD computation to `Z/p` for many primes and CRT back.

The "later, we will extend …" comment flags multivariate and function-
field extensions as planned future work.

## Why a separate file from `Factory`

Factory ([`interface/file-factory-interface.md`](interface/file-factory-interface.md))
provides general GCD and factorisation over many coefficient rings.
`dpoly.cpp` is the engine's **native** path that doesn't require
Factory — useful when Factory is unavailable or when the engine wants
a path it can debug end-to-end.

It is also used by some `aring-*` implementations
([`file-aring-m2-gf.md`](file-aring-m2-gf.md), `file-aring-tower.md`)
that need a univariate-polynomial layer for extension construction.

## Storage

The header includes [`ExponentVector.hpp`](file-ExponentVector.md) —
even though the file is "univariate," the data structure can be reused
for multivariate work later, and `ExponentVector<int, true>` with
`nvars = 1` is a perfectly serviceable univariate exponent
representation.

## Used by

- [`file-aring-m2-gf.md`](file-aring-m2-gf.md) — native GF
  construction uses `dpoly` for minimal-polynomial work.
- [`file-aring-tower.md`](file-aring-tower.md) — each level of a
  tower of extensions stores its minimal polynomial via this code.
- Some Factor-free factorisation paths.

## Related

- [`computations.md`](computations.md) — area overview.
- [`interface/file-factory-interface.md`](interface/file-factory-interface.md)
  — external alternative.
- [`file-aring-m2-gf.md`](file-aring-m2-gf.md), [`file-aring-tower.md`](file-aring-tower.md)
  — primary consumers.
- [`file-ExponentVector.md`](file-ExponentVector.md) — storage helper.
