# `ringmap.m2` — `RingMap` type and operations

`ringmap.m2` defines the M2-side **`RingMap`** type — homomorphisms
`R → S` — and the rich set of operations that flow through them.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-- TODO: needs "newring.m2" for flattenRing
needs "galois.m2"
needs "matrix1.m2"
needs "modules.m2"
needs "modules2.m2"
needs "mutablemat.m2"
```

Six dependencies: a `RingMap` touches almost everything in M2.

## User-facing API

- **`map(S, R, images)`** — build a ring map from the images of `R`'s
  generators in `S`. The standard constructor.
- **`f x`** — apply a ring map `f` to an element / matrix / module.
- **`source f` / `target f`** — the rings.
- **`isHomogeneous f`** — does the map preserve gradings?
- **`coker f` / `kernel f` / `image f`** — ideal-theoretic
  operations on a ring map (treat as a module homomorphism).
- **`f * g`** — composition via image computation.
- **`inverse f`** — when defined.

## Apply-to-anything pattern

```m2
f RingElement       -- apply to a ring element
f Matrix            -- apply entry-wise to a matrix
f Module            -- pushforward / pullback (depending on direction)
f Ideal             -- image of the ideal
f QuotientRing      -- substitute into the quotient
```

The single `f x` calling convention is implemented as a multi-method:
`RingMap RingElement := ...`, `RingMap Matrix := ...`, etc. The
operation chosen depends on `class x`.

## Engine backing

A `RingMap` value internally carries a `RawRingMap*` engine
pointer ([`../e/file-ringmap.md`](../e/file-ringmap.md)) plus
M2-side metadata (the image list as a `Matrix`, options like
`DegreeMap`).

## Used by

- Every M2 substitution / change-of-ring operation.
- `flattenRing`, `presentation`, `lift`, `promote` — all defined as
  `RingMap` operations behind the scenes.
- Algebraic-geometry packages: morphisms of schemes, sheaf
  pullbacks.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-ringmap.md`](../e/file-ringmap.md) — engine `RingMap`.
- [`../e/interface/file-ringmap-interface.md`](../e/interface/file-ringmap-interface.md)
  — public C entry points.
- [`file-polyrings.md`](file-polyrings.md), [`file-quotring.md`](file-quotring.md)
  — source / target types.
