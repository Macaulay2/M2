# `ringmap.{h,cpp}` (in `interface/`) — public C entry points for `RingMap`

`interface/ringmap.h` declares the **public C functions** the interpreter
uses to construct, query, and apply
[`RingMap`](../file-ringmap.md) values — homomorphisms `R → S`.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#if defined(__cplusplus)
class FreeModule;
class Matrix;
class MutableMatrix;
class Ring;
class RingElement;
class RingMap;
#else
typedef struct FreeModule    FreeModule;
typedef struct Matrix        Matrix;
typedef struct MutableMatrix MutableMatrix;
typedef struct Ring          Ring;
typedef struct RingElement   RingElement;
typedef struct RingMap       RingMap;
#endif
```

The header pulls in `MutableMatrix` in addition to the usual suspects,
because applying a `RingMap` to a `MutableMatrix` is a supported
operation.

## Entry points

- **Construction** — `rawRingMap(image_list)`: a `RingMap*` from a matrix
  whose entries are the images of the source generators.
- **Application** — `rawRingMapEval(f, x)`, `rawRingMapEval(f, M)`,
  `rawRingMapEval(f, mut_M)`. Each evaluates the ring map on a single
  element, an immutable matrix, or a mutable matrix respectively.
- **Inspection** — `rawSource(f)`, `rawTarget(f)`, `rawMatrix(f)` (recover
  the matrix of images), `rawIsHomogeneous`.

## Composition

There is no `rawCompose(f, g)` entry point. Composition is built up at
the M2 level by calling `rawMatrix(f)` to recover `f`'s images, applying
`g` element-by-element via `rawRingMapEval`, and constructing a fresh
ring map from the result. This is intentional — see
[`../file-ringmap.md`](../file-ringmap.md).

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-ringmap.md`](../file-ringmap.md) — class implementation.
- [`../ring-elements-and-maps.md`](../ring-elements-and-maps.md) — area
  overview.
- [`file-ringelement-interface.md`](file-ringelement-interface.md) — sibling.
- [`../../m2/ringmap.m2`](../../m2/README.md) — M2-side wrappers.
