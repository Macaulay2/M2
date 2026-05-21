# `monomial.{cpp,hpp}` — `EngineMonomial`

`monomial.cpp` defines **`EngineMonomial`** — the engine's opaque
single-monomial value type that crosses the engine ↔ interpreter
boundary. It is the M2-side `Monomial` (note capitalisation: an
M2-level `Monomial` is one of these), distinct from the encoded
monomial layouts that live inside `Monoid`
([`file-monoid.md`](file-monoid.md)).

Part of the [Monoids & monomials](monoids-and-monomials.md) area.

[← per-area: monoids-and-monomials](monoids-and-monomials.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "monomial.hpp"
#include "error.h"
#include "monoid.hpp"

EngineMonomial::EngineMonomial() {
    // This routine is private because it leaves the object in
    // an incorrect state... to be filled in by varpower routines.
}

// SIGH... the front end reverses monomials.  For commutative ones, this
// is not a problem.  For non-commutative ones, one needs to reverse the
// varpower pairs before calling this function.
```

`EngineMonomial` is a thin wrapper around a **varpower-encoded
monomial** (the sparse `(variable, exponent)` representation).
Its constructor is *private* and intentionally leaves the object in
an incomplete state until varpower routines populate it.

## The reversal comment

The header's "SIGH..." comment captures a long-standing convention
mismatch:

> The front end reverses monomials. For commutative ones, this is not
> a problem. For non-commutative ones, one needs to reverse the
> varpower pairs before calling this function.

The M2-interpreter side reverses the variable-power list before
sending a monomial to the engine. For commutative monomials the
reversal is moot (since the same multiset). For non-commutative
words the order matters; engine code that uses `EngineMonomial` in
the NC setting (rare) must un-reverse first.

## Why `EngineMonomial` exists

Three kinds of monomial representations coexist:

| Type | File | Use |
|---|---|---|
| Encoded monomial inside `Monoid` | [`file-monoid.md`](file-monoid.md) | Inner-loop arithmetic |
| Varpower / ntuple monomials | [`file-ExponentList.md`](file-ExponentList.md), [`file-ExponentVector.md`](file-ExponentVector.md) | Storage layer |
| `EngineMonomial` (this file) | this file | Engine-boundary value type |

`EngineMonomial` is the engine's "official" single-monomial value
exposed to the interpreter. M2's `Monomial` type is bound to it
through [`interface/file-monoid-interface.md`](interface/file-monoid-interface.md).

## Used by

- The interpreter's `Monomial` type (via opaque pointer).
- `interface/monoid.h`'s `rawMonomial...` entry points.
- M2-level monomial-ideal construction
  ([`interface/file-monomial-ideal-interface.md`](interface/file-monomial-ideal-interface.md)).

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — area overview.
- [`file-monoid.md`](file-monoid.md) — `Monoid` (encoded inside-engine
  representation).
- [`file-ExponentList.md`](file-ExponentList.md), [`file-ExponentVector.md`](file-ExponentVector.md)
  — storage encodings.
- [`interface/file-monoid-interface.md`](interface/file-monoid-interface.md),
  [`interface/file-monomial-ideal-interface.md`](interface/file-monomial-ideal-interface.md)
  — public APIs.
