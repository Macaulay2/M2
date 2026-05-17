# `res-tasking-example.cpp` — standalone TBB task-graph sandbox

`res-tasking-example.cpp` is a **standalone sandbox** for
experimenting with Intel TBB's `flow::continue_node` task graphs.
Not part of the engine build — meant to be compiled and run
manually to validate threading patterns before integrating them
into the real resolution engine.

Part of [`schreyer-resolution/`](README.md).

[← schreyer-resolution/ overview](README.md) · [← engine overview](../README.md)

## Header

```cpp
// run this with
// clang++ -I`brew --prefix tbb@2021`/include -L`brew --prefix tbb@2021`/lib --std=c++17 -ltbb res-tasking-example.cpp -g -o example

#include <tbb/tbb.h>
#include <iostream>
#include <vector>
#include <memory>
#include <unistd.h>
#include <mutex>
#include <thread>
#include <chrono>
#include <random>

using Node = tbb::flow::continue_node<tbb::flow::continue_msg>;
using NodePtr = std::shared_ptr<Node>;

tbb::flow::graph G;
std::vector<std::vector<NodePtr>> nodes; // nodes[lev][sldeg] is that particular node.

struct OurNode {
```

The build command at the top is **the entire compile recipe**
— no Makefile, no CMakeLists. Run literally:

```sh
clang++ -I`brew --prefix tbb@2021`/include \
        -L`brew --prefix tbb@2021`/lib \
        --std=c++17 -ltbb \
        res-tasking-example.cpp -g -o example
./example
```

The `brew --prefix tbb@2021` form is **macOS-specific** — points
at the Homebrew-installed TBB. Linux users would substitute their
own paths.

## What it explores

The file simulates the **task structure of a free-resolution
computation**:

- `nodes[lev][sldeg]` — a node per (level, slanted-degree).
- Each node depends on lower-level nodes per the
  resolution-frame ordering.
- TBB's `flow::graph` schedules them according to dependencies.
- Each "node" pretends to be a computation by sleeping a random
  time, then signalling its dependents.

The point: confirm that TBB can express the resolution's task DAG
correctly and finish in roughly the optimal parallel time.

## Why a sandbox

The real resolution engine
([`file-res-dep-graph.md`](file-res-dep-graph.md),
[`file-res-schreyer-frame.md`](file-res-schreyer-frame.md)) uses
TBB to parallelise across (level, degree) cells. Getting the task
graph structure right was non-trivial — wrong dependencies →
data races or sequential bottlenecks.

The sandbox lets the developer:

- **Iterate fast** — recompile in under a second.
- **Validate timing** — measure speedup vs. cores.
- **Test edge cases** — empty levels, single-level computations.

Without affecting the main build.

## Why kept in-tree

A few reasons:

- **Documentation** — shows the intended task-graph structure.
- **Regression diagnosis** — if the real engine misbehaves, the
  sandbox can isolate "is TBB itself working?"
- **Reference** — new contributors can use it to understand the
  task-graph design.

## Used by

- Engine developers experimenting with TBB task graphs.
- Anyone debugging parallelism issues in the resolution engine.

## Related

- [`README.md`](README.md) — schreyer-resolution/ overview.
- [`file-res-dep-graph.md`](file-res-dep-graph.md) — the
  production task-graph implementation.
- [`file-res-schreyer-frame.md`](file-res-schreyer-frame.md) —
  consumes the task graph.
- [`../file-m2tbb.md`](../file-m2tbb.md) — TBB wrapper header.
- Intel TBB — external library.
