# `GBF4Interface.{cpp,hpp}` — engine-boundary entry to the new F4

`GBF4Interface.{cpp,hpp}` is the **engine-boundary entry point**
for the new F4 GB engine (in `newf4::`). The factory function
`createGBF4Interface` builds a `GBComputation` wrapping a
`newf4::GBF4Computation`.

Part of [`gb-f4/`](README.md).

[← gb-f4/ overview](README.md) · [← engine overview](../README.md)

## Header

```cpp
#pragma once

#include "BasicPolyList.hpp"
#include "GBF4Computation.hpp"
#include "PolynomialList.hpp"
#include "../e/comp-gb.hpp"
#include "../matrix-stream.hpp"

class Matrix;

auto createGBF4Interface(const Matrix *inputMatrix,
                         const std::vector<int>& variableWeights, // what is this, do we need it?
                         int strategy,
                         int numThreads
                         ) -> GBComputation*;

namespace newf4 {

class GBF4Computation;
enum class Strategy { Normal };
```

The factory function signature:

- **`inputMatrix`** — generators as an M2 `Matrix`.
- **`variableWeights`** — weights for each variable (used in
  degree calculations).
- **`strategy`** — algorithm variant (currently only `Normal`).
- **`numThreads`** — parallelism level.

Returns a `GBComputation*` — the legacy abstract base
([`../file-comp.md`](../file-comp.md))'s subclass that wraps
`newf4::GBF4Computation`.

## The "what is this, do we need it?" comment

```cpp
const std::vector<int>& variableWeights, // what is this, do we need it?
```

The developer comment is honest: `variableWeights` is in the
signature, but it's not 100% clear whether the new F4 actually
uses it. Likely a holdover from the old F4 engine's signature
that needs cleanup but works as-is.

## Why a separate "interface" file

The pattern across `gb-f4/`:

- **`GBF4Computation.cpp`** — pure `newf4::GBF4Computation`,
  templated, no legacy types.
- **`GBF4Interface.cpp`** — adapter from legacy `Matrix`,
  `GBComputation` to the new template-based engine.

Separating keeps the templated core clean and isolates the
"glue to legacy types" mess in one place.

## Translation path

```
M2 user:  gb I
   ↓ (interpreter, m2/gb.m2)
   rawGB(matrix, ...)
   ↓ (interface/groebner.cpp)
   IM2_RawGB(...)
   ↓ (dispatches by strategy)
   createGBF4Interface(matrix, weights, strategy, threads)
   ↓ (this file)
   newf4::GBF4Computation::create(...)
   ↓ (the actual F4 algorithm runs)
```

## The Strategy enum

```cpp
enum class Strategy { Normal };
```

Just one variant for now (`Normal`). The enum is set up to grow:
strategies like `Signature`, `F5`, `Pure` could be added without
changing the signature.

## Used by

- The interpreter via `interface/groebner.cpp`
  ([`../interface/file-groebner-interface.md`](../interface/file-groebner-interface.md)).
- Test code that exercises the new F4 path.
- The `MSolve` package indirectly.

## Related

- [`README.md`](README.md) — gb-f4/ overview.
- [`file-GBF4Computation.md`](file-GBF4Computation.md) — the
  computation class this interface wraps.
- [`file-PolynomialList.md`](file-PolynomialList.md) — input
  type.
- [`../interface/file-groebner-interface.md`](../interface/file-groebner-interface.md)
  — caller from the interpreter.
- [`../file-comp.md`](../file-comp.md) — `GBComputation` base.
