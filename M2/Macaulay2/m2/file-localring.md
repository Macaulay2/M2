# `localring.m2` — `LocalRing` stub

`localring.m2` is a **lightweight stub** that declares the
`LocalRing` type so M2 knows about it at startup. The actual
implementation lives in the user-loadable
[`LocalRings`](../packages/LocalRings.m2) package.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Full content

```m2
needs "enginering.m2"

-- these objects are fleshed out in LocalRings.m2
LocalRing = new Type of EngineRing
```

That's essentially the entire file. The pattern: declare the type
here so it can be referenced from Core, and load the full
implementation later via `needsPackage "LocalRings"`.

## Why this split

`LocalRing` operations are heavyweight enough to deserve their own
package, but the **type itself** needs to be in Core so that other
Core code paths (matrix arithmetic, GB dispatch) can recognise a
`LocalRing` value without unconditionally loading the
`LocalRings` package.

The pattern is used for several types:

- `LocalRing` — this file.
- `FreeAlgebra`, `FreeAlgebraQuotient` —
  [`file-freealgebras.md`](file-freealgebras.md).
- `GaloisField` — [`file-galois.md`](file-galois.md).

In each case Core declares the type; the package adds the methods.

## Engine backing

The engine's `LocalRing` class
([`../e/file-localring.md`](../e/file-localring.md)) is fully
defined; the M2-side is just a type wrapper. Operations on
`LocalRing` values flow through the engine like any other ring.

## Used by

- Algebraic-geometry packages working at points (tangent cones,
  jets, ...).
- Local-cohomology computations.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-localring.md`](../e/file-localring.md) — engine class.
- [`../packages/LocalRings.m2`](../packages/README.md) — full
  implementation.
- [`file-enginering.md`](file-enginering.md) — parent type.
