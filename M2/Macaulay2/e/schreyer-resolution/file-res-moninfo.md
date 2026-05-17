# `res-moninfo.{cpp,hpp}` — `ResMonoid` (resolution-tuned monomial layout)

`res-moninfo.hpp` selects the **resolution-tuned monomial layout** the
engine uses inside the Schreyer-frame resolution. The header is a
two-line dispatcher between a dense and a sparse implementation.

Part of the [`schreyer-resolution/`](README.md) subdirectory.

[← schreyer-resolution overview](README.md) · [← engine overview](../README.md)

## Full content of the dispatcher

```cpp
#ifndef _res_moninfo_hpp_
#define _res_moninfo_hpp_

#include "schreyer-resolution/res-moninfo-dense.hpp"
#include "schreyer-resolution/res-moninfo-sparse.hpp"

using ResMonoid = ResMonoidDense;
// using ResMonoid = ResMonoidSparse;

#endif
```

The header is intentionally tiny. It:

1. Pulls in both implementations.
2. Aliases `ResMonoid` to one of them.

To switch implementations, edit this file (uncomment the alternate
`using` and comment out the chosen one).

## The two implementations

### `ResMonoidDense` (active)

Lives in
[`res-moninfo-dense.{cpp,hpp}`](README.md). Stores each monomial as a
dense exponent vector of length `nvars`. Best when monomials are dense
or `nvars` is small.

### `ResMonoidSparse`

Lives in [`res-moninfo-sparse.{cpp,hpp}`](README.md). Stores each
monomial as a sparse `(variable, exponent)` list. Best when monomials
have low support and `nvars` is large.

## Why a `using` alias

The resolution code is templated on the monomial type but uses the
`ResMonoid` typedef pervasively. Swapping implementations requires no
changes to call sites — only this header changes.

The commented-out alternative line documents that the sparse path is
still maintained; it just isn't the production default. Periodic
benchmarking decides which is faster on representative inputs.

## What `ResMonoid` provides (informally)

Both implementations expose the same interface:

- `monomial_size()` — encoded byte size.
- `compare(a, b)` — encoded order comparison.
- `multiply(a, b, out)` — encoded multiplication.
- `to_expvector(...)` / `from_expvector(...)` — conversion.

The sparse and dense versions just store the in-between bytes differently.

## Related

- [`README.md`](README.md) — schreyer-resolution overview.
- [`res-moninfo-dense.{cpp,hpp}`](README.md), [`res-moninfo-sparse.{cpp,hpp}`](README.md)
  — the two implementations.
- [`file-res-poly-ring.md`](file-res-poly-ring.md) — primary consumer.
- [`file-res-schreyer-frame.md`](file-res-schreyer-frame.md) —
  another heavy consumer.
- [`../file-monoid.md`](../file-monoid.md) — general-purpose engine
  `Monoid` (this is its resolution specialised cousin).
