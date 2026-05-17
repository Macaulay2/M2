# `engine.h` — the aggregating engine public header (legacy)

`engine.h` is the **legacy aggregating header** that historically
declared every engine entry point in one place. New code is encouraged
to use the narrower headers in [`interface/`](interface/README.md)
instead, but `engine.h` remains the canonical "include this and get
everything" shortcut.

Part of the engine's [Public interface](README.md#top-level-files-per-area-docs).

[← engine overview](README.md)

## Top of the header

```c
#ifndef _engine_h_
#define _engine_h_

/**
   \mainpage Hi, this is my main documentation page.
 */

#include "engine-includes.hpp"

#if defined(__cplusplus)
class FreeModule;
class Matrix;
class MutableMatrix;
class RingMap;
class Computation;
class MutableComplex;
#else
/* Define the externally visible types here */
typedef struct FreeModule    FreeModule;
typedef struct Matrix        Matrix;
typedef struct MutableMatrix MutableMatrix;
typedef struct RingMap       RingMap;
typedef struct Computation   Computation;
typedef struct MutableComplex MutableComplex;
#endif

#include "interface/aring.h"
#include "interface/computation.h"
```

The two pieces:

1. **Forward declarations** — the same dual-mode pattern every
   `interface/*.h` header uses, repeated here for the convenience
   types `engine.h` declared first historically.
2. **Includes from `interface/`** — the header pulls in each of the
   per-area `interface/*.h` headers, so consumers get the full public
   surface in one `#include`.

The `/** \mainpage */` block is for Doxygen — see
[`docs/`](../docs/README.md) for how the engine's developer docs are
built.

## Why a flat `engine.h` still exists

Historical reasons:

- **Backwards compatibility** — old code outside the engine includes
  `engine.h` and shouldn't need to change.
- **One-include convenience** — for small programs that don't care to
  pick the right narrower header.
- **Doxygen entry point** — the file serves as the documented
  "front page" of the engine's public API.

The recommended pattern for **new** code is to include only the
narrower `interface/*.h` headers that match what the code actually
needs (see [`interface/README.md`](interface/README.md)). This:

- Reduces compile-time include bloat.
- Makes dependencies explicit.
- Aligns with the engine's long-running migration away from `engine.h`.

## What's in (and what's not)

`engine.h` re-exports essentially the entire `interface/` subdirectory,
plus a few legacy `x-*.cpp` entry points that haven't been migrated.
The exact list of inclusions changes over time as files migrate; the
authoritative view is the file itself.

## Related

- [`README.md`](README.md) — engine overview ("Public interface"
  section).
- [`interface/README.md`](interface/README.md) — the recommended
  per-area headers to use instead.
- [`docs/README.md`](../docs/README.md) — where Doxygen output lives.
- `engine-includes.hpp` — sibling header pulling in the engine-wide
  baseline (`config.h`, `M2_*` types, etc.).
