# `groebner.{h,cpp}` (in `interface/`) — public C entry points for GB / resolutions

`interface/groebner.h` declares the **public C functions** that the
interpreter uses to start, drive, and inspect Gröbner basis and resolution
computations. It is the C-shaped façade over the
[`GBComputation`](../file-comp-gb.md) and
[`ResolutionComputation`](../file-comp-res.md) class families.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#include "engine-includes.hpp"
#include "interface/computation.h"

#if defined(__cplusplus)
class Computation;
class FreeModule;
class Matrix;
class Ring;
class RingElement;
class MonomialOrdering;
class MutableMatrix;
class RingMap;
#else
typedef struct Computation Computation;
typedef struct FreeModule  FreeModule;
typedef struct Matrix      Matrix;
// ...
#endif

#if defined(__cplusplus)
extern "C" {
#endif

// ...declarations...

#if defined(__cplusplus)
}
#endif
```

Like its sibling [`aring.h`](file-aring-interface.md), the header is dual-mode
— C++ classes for the engine, opaque C structs for the interpreter — and
wraps everything in `extern "C"`.

The author's comment notes that this header is based on what `groebner.cpp`
actually exports, *not* on the older flat-layout entry points in `engine.h`.
Some declarations in `engine.h` may not match; this file is the source of
truth.

## What it exposes

The header declares C entry points that bracket every GB / resolution
operation:

- **Construct** a `GBComputation*` or `ResolutionComputation*` from a
  matrix, ring, options.
- **Drive** the computation (set stop conditions, start, resume).
- **Read** results (basis, change matrix, syzygies, leading-term matrix,
  Betti table).
- **Inspect** state (`status()`, `complete_thru_degree()`).

All operations take and return opaque `Computation*` plus the supporting
types declared at the top of the header.

## How it connects to the dispatcher

Inside `groebner.cpp`, each entry point dispatches to:

- The `GBComputation::choose_gb(...)` factory ([`file-comp-gb.md`](../file-comp-gb.md))
  — picks the right GB algorithm.
- The `ResolutionComputation::choose_res(...)` factory
  ([`file-comp-res.md`](../file-comp-res.md)) — picks the right resolution
  algorithm.

The interpreter never sees the dispatching logic; it just gets back an
opaque pointer.

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-comp-gb.md`](../file-comp-gb.md), [`../file-comp-res.md`](../file-comp-res.md)
  — the class families this header exposes.
- [`computation.h`](README.md) — generic stop-conditions / status enums
  shared with other Computations.
- [`../../d/engine.dd`](../../d/README.md) — interpreter side.
