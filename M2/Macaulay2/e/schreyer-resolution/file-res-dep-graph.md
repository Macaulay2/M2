# `res-dep-graph.{cpp,hpp}` — TBB dependency graph for parallel resolutions

`res-dep-graph.cpp` builds a **dependency graph** over the per-degree
work units of a Schreyer-frame resolution, using Intel TBB's flow-graph
primitives. The graph lets multiple homological degrees be computed in
parallel while still respecting the data dependencies that make some
order necessary.

Part of the [`schreyer-resolution/`](README.md) subdirectory.

[← schreyer-resolution overview](README.md) · [← engine overview](../README.md)

## TBB integration

```cpp
#include "m2tbb.hpp"

#ifdef WITH_TBB

#include <iostream>
#include <vector>
#include <memory>
#include <mutex>
#include <stack>

using TBBNode    = tbb::flow::continue_node<tbb::flow::continue_msg>;
using TBBNodePtr = std::shared_ptr<TBBNode>;
```

The whole file is `WITH_TBB`-gated. If TBB is unavailable at build time,
the parallel path compiles to no-ops and the resolution runs serially.

`tbb::flow::continue_node<tbb::flow::continue_msg>` is a TBB flow-graph
node that triggers its successors after every predecessor has fired. It
is the natural primitive for "compute (level, degree) once all (level',
degree') it depends on have completed."

## The grid index

```cpp
inline int getIndex(int lev, int sldeg, int nlevels, int nslanted_degrees) {
    (void) nslanted_degrees;
    return lev + (sldeg * nlevels);
}
```

The dependency graph is laid out as a 2-D grid keyed by `(level,
slanted-degree)` and linearised by `getIndex`. "Slanted degree" is the
combinatorial coordinate `level + degree` that the Schreyer-frame
machinery naturally produces. The `nslanted_degrees` parameter is
currently unused — kept for future graph-layout work.

## Dependencies between cells

A cell `(level, slanted-degree)` depends on:

- The cell **one level up** at the same slanted-degree.
- The cell **one slanted-degree down** at the same level.

(These are the standard dependencies of a Schreyer-style F4 resolution.)
The constructor wires the corresponding TBB edges.

## Parallelism vs. degree-vs.-level

The grid layout lets the user choose between **degree-major** and
**level-major** parallelism via the `parallelizeByDegree` flag in
[`F4ResComputation`](file-res-f4-computation.md). Both partitionings
respect the graph; they differ in which cells become parallel and which
become serial.

## Related

- [`README.md`](README.md) — schreyer-resolution overview.
- [`file-res-f4-computation.md`](file-res-f4-computation.md) — primary
  consumer (`parallelizeByDegree` flag).
- [`file-res-schreyer-frame.md`](file-res-schreyer-frame.md) — produces
  the input grid.
- `m2tbb.hpp` at the engine top level — TBB wrapper.
- TBB submodule under [`../../../submodules/README.md`](../../../submodules/README.md).
