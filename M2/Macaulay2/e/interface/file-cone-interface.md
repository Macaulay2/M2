# `cone.{h,cpp}` (in `interface/`) — public C entry points for cone operations

`interface/cone.h` declares the **public C functions** the interpreter
uses for **rational polyhedral cone** operations — Hilbert basis,
extreme rays, lineality space, dual cones. The implementation routes to
external libraries (cddlib, normaliz) via separate dispatchers.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#if defined(__cplusplus)
class Matrix;
#else
typedef struct Matrix Matrix;
#endif

#if defined(__cplusplus)
extern "C" {
#endif
// ... raw cone functions ...
#if defined(__cplusplus)
}
#endif
```

Compared to its sibling headers, this one is **small**: cones are
represented by their facet / ray matrices, so the only forward-declared
type is `Matrix`. Everything else is `M2_arrayint` or other primitive
types.

## Entry points

The functions cover the operations needed by the `Polyhedra` and toric
geometry packages:

- `rawConeFromGenerators(rays)` — build a cone from ray generators.
- `rawConeFromInequalities(ineqs)` — build a cone from facet inequalities.
- `rawDualCone(C)` — dual of a cone.
- `rawHilbertBasis(C)` — Hilbert basis (minimal generators of the
  semigroup of lattice points in the cone).
- `rawExtremeRays(C)`, `rawLineality(C)` — extreme rays and lineality
  space.

The user-visible package that drives these is
[`Polyhedra`](../../packages/Polyhedra.m2); see also `Normaliz` and
toric-geometry packages for further consumers.

## External library dependencies

- **cddlib** — provides the convex hull / dual cone operations.
- **normaliz** — provides the Hilbert basis algorithm.

Both are detected by `cmake/FindCDDLIB.cmake` and `cmake/FindNormaliz.cmake`
([`../../../cmake/README.md`](../../../cmake/README.md)). When absent,
the corresponding entry points return `null` and the M2 layer reports
"feature not available."

## Related

- [`README.md`](README.md) — interface overview.
- [`Polyhedra`](../../packages/Polyhedra.m2),
  [`Normaliz`](../../packages/Normaliz.m2) — M2-level consumers.
- cddlib / normaliz under [`../../../libraries/`](../../../libraries/README.md).
