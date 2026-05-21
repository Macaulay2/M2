# `res-schreyer-frame.{cpp,hpp}` — the Schreyer frame

`SchreyerFrame` is the **frame** data structure for the F4-style resolution
— the engine's compact representation of the entire resolution as it is
being built. It records, for every homological level and degree, the
generators, their Schreyer order data, and the matrix entries that will
become the next differential.

Part of the [`schreyer-resolution/`](README.md) subdirectory.

[← schreyer-resolution overview](README.md) · [← engine overview](../README.md)

## What a frame is

For a free resolution

```
… → F_2 → F_1 → F_0 → M → 0,
```

the frame stores, per level `i`, an ordered list of generators of `F_i`
along with:

- The **leading monomial** of the syzygy each generator represents.
- The **Schreyer order data** that will be used when the next level's
  generators are computed (see [`file-res-schreyer-order`](README.md)).
- The **degree** of the generator.
- Status flags (live, retired, …).

Together these are enough to drive the F4 reductions at the next level
without referring back to the original input — the frame is self-contained.

## Threading

The header includes `m2tbb.hpp` and pulls in
[`BettiDisplay`](../file-betti.md). TBB primitives enable per-degree and
per-level parallelisation; the `parallelizeByDegree` flag in
[`file-res-f4-computation.md`](file-res-f4-computation.md) chooses the
grain size.

## Outstanding work

The header's `to do list` (preserved verbatim at the top of the file)
shows the active refactor agenda:

- "display of `poly` elements in the resolution"
- `get_matrix`: should return frame elements when they exist
- `CoefficientArray`: build incrementally
- monomial lookup routine
- decide whether monomials should be varpowers or simple variable lists

These items live in `TODO-branch-res-2018` in this subdirectory.

## Related

- [`README.md`](README.md) — schreyer-resolution overview.
- [`file-res-f4-computation.md`](file-res-f4-computation.md) — primary
  consumer of the frame.
- [`file-res-poly-ring.md`](file-res-poly-ring.md) — ring view used by the
  frame's polynomials.
- [`../file-betti.md`](../file-betti.md) — frame produces a `BettiDisplay`
  on demand.
- [`res-dep-graph.cpp`](README.md) — dependency graph for parallel
  scheduling between frame nodes.
