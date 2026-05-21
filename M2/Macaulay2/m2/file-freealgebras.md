# `freealgebras.m2` — `FreeAlgebra` and `FreeAlgebraQuotient` stubs

`freealgebras.m2` is a **stub** that declares the M2-side types for
non-commutative free algebras and their quotients. The detailed
implementation lives in the user-loadable
[`AssociativeAlgebras`](../packages/) package.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Full content

```m2
needs "enginering.m2"
needs "quotring.m2"

-- these objects are fleshed out in AssociativeAlgebras.m2
FreeAlgebra         = new Type of EngineRing
FreeAlgebraQuotient = new Type of QuotientRing
```

Like [`file-localring.md`](file-localring.md), this is the
type-declaration-only Core entry point. The full operations on
`FreeAlgebra` / `FreeAlgebraQuotient` — multiplication, ideal
construction, GB computation — get added when the user loads
`AssociativeAlgebras`.

## Why a Core stub

The types need to exist in Core so that:

- M2 method dispatch can recognise a `FreeAlgebra` argument.
- Polynomial value types
  ([`../e/file-Polynomial.md`](../e/file-Polynomial.md)) referencing
  them are valid.
- Core code paths handling polynomial multiplication can branch on
  `class R === FreeAlgebra`.

If the types only existed in the package, those branches would
either fail to compile or require deferred loading.

## Engine peers

| M2 type | Engine class |
|---|---|
| `FreeAlgebra` | [`../e/file-M2FreeAlgebra.md`](../e/file-M2FreeAlgebra.md) (M2-facing wrapper) |
| `FreeAlgebraQuotient` | [`../e/file-M2FreeAlgebraQuotient.md`](../e/file-M2FreeAlgebraQuotient.md) |

Underneath, both engine wrappers use the inner classes from
[`../e/NCAlgebras/`](../e/NCAlgebras/README.md).

## Used by

- The `AssociativeAlgebras` package — defines the operations.
- M2 method dispatch — `FreeAlgebra` is checked in `class R` tests.
- Algebraic-topology packages occasionally working with free
  algebras.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-M2FreeAlgebra.md`](../e/file-M2FreeAlgebra.md),
  [`../e/file-M2FreeAlgebraQuotient.md`](../e/file-M2FreeAlgebraQuotient.md)
  — engine wrappers.
- [`../e/NCAlgebras/README.md`](../e/NCAlgebras/README.md) — engine
  NC infrastructure.
- [`../packages/`](../packages/README.md) — `AssociativeAlgebras`
  full implementation.
- [`file-localring.md`](file-localring.md) — sister Core stub.
