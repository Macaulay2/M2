# Ring elements and ring maps

The **`RingElement`** type is what the interpreter holds onto when M2 code
manipulates an element of a ring; the **`RingMap`** type is the engine
representation of a homomorphism between rings.

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e) · [per-area docs](README.md#top-level-files-per-area-docs)

## Ring elements

| File pair | Purpose |
|---|---|
| `relem.{cpp,hpp}` | `RingElement` — pair of `(Ring*, ring-specific-value)`. **Deep dive:** [`file-relem.md`](file-relem.md) |

A `RingElement` is essentially a tagged union: a pointer to the ring it
belongs to plus an opaque value the ring knows how to interpret. All
arithmetic dispatches through the ring's virtual methods (or through the
templated [`aring`](coefficient-rings.md) machinery for newer code).

## Ring maps (homomorphisms)

| File pair | Purpose |
|---|---|
| `ringmap.{cpp,hpp}` | `RingMap` — specifies a homomorphism by giving images of generators. **Deep dive:** [`file-ringmap.md`](file-ringmap.md) |

A `RingMap : R → S` is built from:

- a source ring `R`
- a target ring `S`
- a list of images, one per generator of `R`

Applying a ring map to a polynomial means substituting images for generators
and evaluating in the target ring. Composition is handled by walking through
generators.

## M2-facing wrappers for non-commutative algebras

| File pair | Purpose |
|---|---|
| `M2FreeAlgebra.{cpp,hpp}` | M2-side `RingElement`-compatible wrapper for [`NCAlgebras/FreeAlgebra`](NCAlgebras/README.md) |
| `M2FreeAlgebraQuotient.{cpp,hpp}` | Wrapper for [`NCAlgebras/FreeAlgebraQuotient`](NCAlgebras/README.md) |

These exist because the non-commutative classes in `NCAlgebras/` keep their
internal representation private; the wrappers here translate to/from
`RingElement` so the rest of the engine can treat NC rings like any other.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — concrete rings that elements
  live in.
- [`polynomial-rings.md`](polynomial-rings.md) — polynomial-ring elements
  flow through `RingElement`.
- [`interface/ringelement.{h,cpp}`](interface/README.md) and
  [`interface/ringmap.{h,cpp}`](interface/README.md) — public API.
