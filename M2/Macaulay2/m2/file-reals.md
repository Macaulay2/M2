# `reals.m2` — `RR`, `CC`, `RRi`, `CCi`, `ImmutableType`

`reals.m2` defines the **`ImmutableType`** abstract base plus the
M2-side **`RR`**, **`CC`** (and their interval variants `RRi`,
`CCi`) types. These are the types for real and complex numbers at
configurable precision.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## `ImmutableType`

```m2
needs "enginering.m2"

ImmutableType = new Type of HashTable
ImmutableType.synonym = "immutable type"
globalAssignment ImmutableType
```

`ImmutableType` is the abstract base for types whose underlying
representation is **immutable** (cannot be mutated in place). Real
and complex numbers fit: once you have a specific `RR_53` value,
its bits don't change.

## The numeric family

| Type | Backed by | Precision |
|---|---|---|
| `RR` | `ARingRR` ([`../e/file-aring-RR.md`](../e/file-aring-RR.md)) | 53-bit `double` (default) |
| `RR_n` | `ARingRRR` ([`../e/file-aring-RRR.md`](../e/file-aring-RRR.md)) | `n` mantissa bits via MPFR |
| `CC` | `ARingCC` ([`../e/file-aring-CC.md`](../e/file-aring-CC.md)) | Complex (pair of `double`) |
| `CC_n` | `ARingCCC` ([`../e/file-aring-CCC.md`](../e/file-aring-CCC.md)) | Complex (pair of MPFR) |
| `RRi`, `RRi_n` | `ARingRRi` ([`../e/file-aring-RRi.md`](../e/file-aring-RRi.md)) | Real intervals |
| `CCi`, `CCi_n` | `ARingCCi` ([`../e/file-aring-CCi.md`](../e/file-aring-CCi.md)) | Complex intervals |

The `_n` syntax constructs a precision-parameterised variant:

```m2
RR_100 = numeric(100, e^(pi*ii))
```

Each precision is its own ring — `RR_53` and `RR_100` are not
interchangeable.

## What this file does

- Declares `ImmutableType` as the parent of every numeric type.
- Declares the `RR`, `CC`, `RRi`, `CCi` types and their `_n` syntax.
- Adds M2-side methods that route to the corresponding engine
  arings.
- Implements `numeric x`, `precision x`, and friends.

## Used by

- Every M2 expression containing a real / complex / interval value.
- Numerical packages (`NumericalAlgebraicGeometry`, `Bertini`, …).
- LAPACK / FFLAS dispatch — the precision determines which back end
  applies.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-integers.md`](file-integers.md), [`file-rationals.md`](file-rationals.md)
  — sibling exact-arithmetic types.
- [`../e/file-aring-RR.md`](../e/file-aring-RR.md),
  [`../e/file-aring-RRR.md`](../e/file-aring-RRR.md),
  [`../e/file-aring-CC.md`](../e/file-aring-CC.md),
  [`../e/file-aring-CCC.md`](../e/file-aring-CCC.md),
  [`../e/file-aring-RRi.md`](../e/file-aring-RRi.md),
  [`../e/file-aring-CCi.md`](../e/file-aring-CCi.md) — engine
  implementations.
