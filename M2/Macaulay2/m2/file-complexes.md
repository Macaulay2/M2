# `complexes.m2` — `ChainComplex` / `Complex` dispatch

`complexes.m2` is the **M2-side dispatcher** between the `Complexes`
and `OldChainComplexes` packages. Macaulay2 has two parallel chain-
complex implementations; this file picks one and routes the
top-level functions (`res`, `Hom`, `dual`, etc.) accordingly.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## The two implementations

```m2
needs "shared.m2"     -- for isMorphism
needs "gateway.m2"    -- for ScriptedFunctor
needs "matrix1.m2"    -- for Ideal
needs "modules.m2"    -- for Module

-- whether to use Complexes or OldChainComplexes
HomologicalAlgebraPackage = "Complexes"
```

The single setting `HomologicalAlgebraPackage` decides which package
the user gets:

- **`"Complexes"`** (modern, default) — the
  [`Complexes`](../packages/Complexes.m2) package. Cleaner API,
  better type discipline, actively developed.
- **`"OldChainComplexes"`** — the legacy `ChainComplex` type that
  predates the cleaner reimplementation. Many older M2 codes still
  use it.

The switch lets the user keep old code working: set
`HomologicalAlgebraPackage = "OldChainComplexes"` before loading
old code that depends on the old API.

## The default is `Complexes`

As of the current release, `complexes.m2` defaults to the modern
implementation. The `Complexes` package was promoted to default
after years of parallel testing in
[`../packages/Complexes/`](../packages/README.md).

## What this file does mechanically

Beyond setting the package, the file:

- Defines `baseRing'` — used by the legacy `OldChainComplexes` to
  walk the ring tower.
- Routes the user-facing `res`, `Hom`, `Ext`, `Tor` to either
  package's implementation.
- Re-exports symbols the user expects regardless of which package is
  in use.

## Related TODOs

The header notes two GitHub issues:

- [#647](https://github.com/Macaulay2/M2/issues/647)
- [#2159](https://github.com/Macaulay2/M2/issues/2159)

Both relate to lingering inconsistencies between the two
implementations. Closing them is the path to fully retiring
`OldChainComplexes`.

## Used by

- Every M2 user computing resolutions or `Ext`/`Tor`.
- Algebraic-geometry packages that manipulate complexes.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../packages/Complexes.m2`](../packages/README.md) — modern
  package.
- `OldChainComplexes` — legacy package (also in `packages/`).
- [`../e/file-comp-res.md`](../e/file-comp-res.md) — engine-side
  resolution Computation.
