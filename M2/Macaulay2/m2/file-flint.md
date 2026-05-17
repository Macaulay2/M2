# `flint.m2` — `ZZFlintRing`, `QQFlintRing` stubs

`flint.m2` declares the **`ZZFlintRing`** and **`QQFlintRing`** type
stubs and provides `makeZZFlint` / `makeQQFlint` constructors. These
are the M2-level wrappers for the engine's FLINT-backed
[`ARingZZ`](../e/file-aring-zz-flint.md) and
[`ARingQQFlint`](../e/file-aring-qq-flint.md).

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "engine.m2"
needs "enginering.m2"
needs "mutablemat.m2"
needs "quotring.m2"     -- for initializeEngineLinearAlgebra

ZZFlintRing = new Type of EngineRing
QQFlintRing = new Type of EngineRing

makeZZFlint = () -> (
    ...
)
```

Two M2-level types:

- **`ZZFlintRing`** — wraps the engine's
  [`ARingZZ`](../e/file-aring-zz-flint.md).
- **`QQFlintRing`** — wraps
  [`ARingQQFlint`](../e/file-aring-qq-flint.md).

Construction goes through `makeZZFlint()` / `makeQQFlint()` which
calls into the engine factories.

## Why a separate file from `integers.m2` / `rationals.m2`

`integers.m2` and `rationals.m2` deal with the **default** `ZZ` and
`QQ` (which today happen to be FLINT-backed by typedef). `flint.m2`
exposes the FLINT-backed variants as **explicit, named** rings that
the user can choose deliberately.

This matters when:

- A user wants to force FLINT semantics regardless of the default.
- A package wants to test against the FLINT path specifically.

## Used by

- Testing and benchmarking code that compares FLINT vs. GMP paths.
- Sophisticated users who explicitly construct the FLINT variants.
- The engine's linear-algebra initialisation
  (`initializeEngineLinearAlgebra`) sets up dispatch tables that
  reference these types.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-aring-zz-flint.md`](../e/file-aring-zz-flint.md),
  [`../e/file-aring-qq-flint.md`](../e/file-aring-qq-flint.md)
  — engine peers.
- [`file-integers.md`](file-integers.md), [`file-rationals.md`](file-rationals.md)
  — sibling numeric-type files (using the default backends).
- FLINT submodule under [`../../submodules/README.md`](../../submodules/README.md).
