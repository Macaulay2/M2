# `integers.m2` — `Number` and `ZZ` operations

`integers.m2` defines the **`Number`** abstract base and the
M2-side operations on **`ZZ`** (integers). It establishes the
`Number → Ring` connection that lets `ring x = ZZ` work for integer
values.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "rings.m2"

-----------------------------------------------------------------------------
-- Number
-----------------------------------------------------------------------------

isHomogeneous Number := x -> true
ring Number := class
```

Two foundational lines:

1. **`isHomogeneous Number := x -> true`** — numbers are trivially
   homogeneous (degree 0). This default lets `isHomogeneous` work
   on any expression that bottoms out in numbers without needing
   per-subclass overrides.
2. **`ring Number := class`** — the same class-as-ring trick from
   [`file-enginering.md`](file-enginering.md): `ring 5` returns the
   class of `5`, which is `ZZ`.

## The `Number` hierarchy

```
Thing
 └── Type
      └── Number (declared here as a type tag)
           ├── ZZ
           ├── QQ
           └── (RR, CC, ZZ/p, GF — also descend from Number)
```

`Number` itself is just a marker; concrete numeric classes (`ZZ`,
`QQ`, `RR`, `CC`) declare their own arithmetic.

## What integer operations live here

The file builds on the engine-provided base:

- Engine produces `ZZ` arithmetic (`+`, `*`, `==`, …) via
  [`../e/file-aring-zz-flint.md`](../e/file-aring-zz-flint.md).
- This file declares the M2-side method bindings that route those
  operations through the engine.
- Plus M2-only conveniences: `even`, `odd`, `gcd`, `binomial`,
  `factorial`, etc.

## Used by

- Every M2 expression containing an integer.
- [`file-rationals.md`](file-rationals.md) — `QQ` builds on `ZZ`.
- [`../e/file-aring-zz-flint.md`](../e/file-aring-zz-flint.md) —
  engine peer.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-rationals.md`](file-rationals.md) — Q analogue.
- [`file-reals.md`](file-reals.md) — RR/CC analogue.
- [`../e/file-aring-zz-flint.md`](../e/file-aring-zz-flint.md) —
  engine integer arithmetic.
