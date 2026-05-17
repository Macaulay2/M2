# `enginering.m2` — `EngineRing` and `RingElement`

`enginering.m2` defines **`EngineRing`** — the abstract M2 base for
rings that delegate to the engine — and the user-facing
**`RingElement`** type that holds values in those rings.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "expressions.m2"
needs "remember.m2"
needs "rings.m2"

RingElement.synonym = "ring element"
value RingElement := identity
raw RingElement := f -> f#0
RingElement == RawRingElement := (x, y) -> raw x === y
RawRingElement == RingElement := (x, y) -> x === raw y
ring RingElement := r -> class r
factor RingElement := r -> error "factor: unimplemented for this ring"
precision RingElement := precision @@ ring
```

Three closely-related types:

- **`EngineRing`** — abstract subclass of
  [`Ring`](file-rings.md). All engine-backed rings inherit from it.
- **`RingElement`** — the values these rings produce. Backed by a
  single `RawRingElement` engine value stored under index `0`.
- **`RawRingElement`** — the engine boundary type (defined in `d/`).

## The class-as-ring trick

The line `ring RingElement := r -> class r` shows M2's clever trick:
**a ring element's class IS its ring.** When you write `class
(x + y)` you get the polynomial ring; when you write `class 5` you
get `ZZ`. This makes `ring x` a one-line operation.

## Default-error methods

```m2
factor RingElement := r -> error "factor: unimplemented for this ring"
```

The default `factor` raises an explicit "unimplemented" error.
Subclasses that *do* support factoring (e.g., over fields) override
this. The pattern makes the error clear and pinpoints where to
implement the missing method.

## Where `EngineRing` fits

Hierarchy:

```
Ring
 └── EngineRing (this file)
      ├── PolynomialRing (file-polyrings.md)
      ├── QuotientRing (quotring.m2)
      ├── LocalRing (localring.m2)
      ├── GaloisField (galois.m2)
      ├── FreeAlgebra, FreeAlgebraQuotient (freealgebras.m2)
      └── ...
```

`EngineRing` is the *abstract* base for "engine-backed." Concrete
rings (left column) inherit from it and add their specific options.

## Used by

- Every engine-backed ring class in the M2 layer.
- Every `RingElement` arithmetic op.
- M2's `value`, `raw`, `ring`, `class`, `parent` reflection
  primitives.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-rings.md`](file-rings.md) — abstract `Ring` parent.
- [`file-polyrings.md`](file-polyrings.md) — primary subclass.
- [`../e/file-relem.md`](../e/file-relem.md) — engine `RingElement`.
- [`../e/file-ringelem.md`](../e/file-ringelem.md) — engine
  `ring_elem` value type.
