# `res-a0.{cpp,hpp}`, `res-a1.{cpp,hpp}`, `res-a2.{cpp,hpp}` — older resolution engines

The `res-a0`, `res-a1`, `res-a2` families implement the **older
free-resolution engines** that predate the Schreyer-frame F4
resolution in [`schreyer-resolution/`](schreyer-resolution/README.md).
They remain in the tree because they handle cases the modern engine
doesn't yet (general orderings, non-field coefficients).

Part of the [engine](README.md) — resolutions.

[← engine overview](README.md) · [resolutions](resolutions.md)

## The three generations

| Generation | Key class | When used |
|---|---|---|
| `res-a0` | `res2_comp` | Original resolution engine (1996) |
| `res-a1` | `res_comp` | Reworked layout, additional features (1996) |
| `res-a2` | `gb2_comp` | Multi-stage version with Hilbert hints (1997-2006) |

User-facing `resolution(I)` is dispatched by the M2 layer (Core
M2 file `../m2/res.m2`) to one of these
based on heuristics:

- Strategy 0 / 1 → `res-a0` family.
- Strategy 2 → `res-a1` family.
- Strategy 3 → `res-a2` family.
- Strategy 4 → the Schreyer-frame F4 engine
  ([`schreyer-resolution/`](schreyer-resolution/README.md)).

## `res-a0` — original

```cpp
// Copyright 1996.  Michael E. Stillman
#include "comp-res.hpp"

struct res2_pair;
class res2_comp;
class res2_poly;
```

The struct/class names with `2` suffixes are an artefact of
naming — there was a `res_comp` (in `res-a1`) that came first
chronologically, so this got `res2_` even though it's earlier in
the build sequence.

Layout: traditional Schreyer-style with explicit pair processing
and a custom polynomial type (`res2_poly`).

## `res-a1` — reworked

```cpp
#include "res-a1-poly.hpp"

class res_pair;
class res_degree;
```

Same basic algorithm but **degree-by-degree** processing
(`res_degree` per-degree containers). Better suited for
Hilbert-function-driven termination.

## `res-a2` — multi-stage

```cpp
#include "spair.hpp"

#define STATE_DONE 0
#define STATE_NEW_DEGREE 1
#define STATE_HILB 2
#define STATE_GB 3
```

Adds **multiple stages** per degree: Hilbert prediction, partial
GB, then full reduction. The state machine
(`STATE_DONE`/`NEW_DEGREE`/`HILB`/`GB`) is exposed so
[`comp.cpp`](file-comp.md)'s incremental machinery can pause
between stages.

## Why keep three generations

Each has different strengths:

- **`res-a0`** — simplest, easiest to verify, no Hilbert hints.
- **`res-a1`** — handles weighted gradings cleanly.
- **`res-a2`** — fastest where Hilbert info is available.

Users picking a strategy effectively pick which generation runs.

## Used by

- M2's `resolution I`.
- M2's `betti` (via the resolution).
- The interpreter through [`interface/file-groebner-interface.md`](interface/file-groebner-interface.md).

## Related

- [`README.md`](README.md) — engine overview.
- [`resolutions.md`](resolutions.md) — area.
- [`schreyer-resolution/README.md`](schreyer-resolution/README.md)
  — the modern engine (where most work goes).
- [`file-comp-res.md`](file-comp-res.md) — common base.
- M2-side `resolution` dispatcher lives in the Core M2 load
  sequence — see [`../m2/README.md`](../m2/README.md).
