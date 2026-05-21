# `m2tbb.hpp` — engine TBB wrapper

`m2tbb.hpp` is the engine's **wrapper around Intel TBB** (Threading
Building Blocks) — the parallel-programming primitives the engine uses
for F4 inner-loop parallelism and Schreyer-resolution scheduling. The
wrapper exists to make TBB an *optional* dependency: when TBB is absent,
all the parallel paths compile to no-ops and the engine runs serially.

Part of the [Utilities](utilities.md) area (and a cross-cutting
dependency of [`schreyer-resolution/`](schreyer-resolution/README.md)
and the F4 engines).

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## Full source

The header is exceptionally short:

```cpp
#ifndef M2TBB_HPP
#define M2TBB_HPP

// The plan: All uses of TBB go through the following interface.

#include <M2/config.h>   // make sure WITH_TBB is set before including mtbb.hpp

#ifndef WITH_TBB
#define MATHICGB_NO_TBB 1
#endif

#include "mathicgb/mtbb.hpp"

#endif
```

Two moving parts:

1. **`WITH_TBB`** — defined (or not) by configure-time CMake detection in
   [`cmake/FindTBB.cmake`](../../cmake/README.md). Propagated through
   `M2/config.h`.
2. **`mathicgb/mtbb.hpp`** — the
   [mathicgb](https://github.com/Macaulay2/mathicgb)-side abstraction
   layer over TBB. mathicgb defines a small set of parallel primitives
   (parallel `for`, atomic counters, mutexes, flow-graph nodes) that
   degrade gracefully when TBB is unavailable.

`MATHICGB_NO_TBB` is set when `WITH_TBB` is not — that's mathicgb's
mechanism for compiling the abstractions away.

## "All uses of TBB go through the following interface"

The single-line plan in the header is the policy: engine code **must
include `m2tbb.hpp`**, not `<tbb/…>` directly. This way, adding TBB to a
build that previously didn't have it requires no code changes in the
engine — just `WITH_TBB=ON` at configure time.

## Primary consumers

- [`schreyer-resolution/`](schreyer-resolution/README.md) —
  [`SchreyerFrame`](schreyer-resolution/file-res-schreyer-frame.md) and
  [`res-dep-graph`](schreyer-resolution/file-res-dep-graph.md) use TBB
  flow-graph for cross-degree / cross-level parallelism.
- [`NCAlgebras/NCF4`](NCAlgebras/file-NCF4.md) — pulls in `m2tbb.hpp`
  for parallel row reduction.

Other engine paths may be parallelised in the future without changing
this header.

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`schreyer-resolution/file-res-dep-graph.md`](schreyer-resolution/file-res-dep-graph.md)
  — exemplar consumer.
- [`NCAlgebras/file-NCF4.md`](NCAlgebras/file-NCF4.md) — another consumer.
- TBB & mathicgb under [`../../submodules/README.md`](../../submodules/README.md).
- [`../../cmake/README.md`](../../cmake/README.md) — `FindTBB.cmake`.
