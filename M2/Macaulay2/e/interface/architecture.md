# Engine public-interface architecture

This document is the **architectural reference** for
`M2/Macaulay2/e/interface/` — the public C API the rest of the
engine exposes to the interpreter. The smallest of the engine
subdirs but the one with the strictest contract.

[← interface/ overview](README.md) · [← engine architecture](../architecture.md)

## The boundary contract

`interface/` is the **only place** in the engine that the
interpreter directly includes. Three rules govern this boundary:

1. **No `#include "engine.h"`** within `interface/`. The aggregating
   header `engine.h` historically pulled everything in; new entries
   live in narrower `interface/<area>.h` files.
2. **No C++ classes in public function signatures.** Values cross
   as opaque pointers (`Matrix*`, `MutableMatrix*`, ...), GMP types
   (`mpz_t`), or M2 universal types (`M2_arrayint`, `M2_string`).
3. **Plain C linkage on public functions.** Each header wraps its
   declarations in `extern "C" { ... }` (when compiling as C++) so
   names aren't mangled. The interpreter's `.d`-translated C code
   calls them directly.

These rules let the interpreter call into the engine without
needing C++ compatibility — `scc1`-emitted C code can `#include
"interface/foo.h"` without bringing in any C++.

## What lives here

```
┌──────────────────────────────────────────────────────┐
│   Type-system aliases                                 │
│   m2-types.{h,cpp}, m2-mem.{h,cpp}                    │
│   gmp-util.h, computation.h                           │
├──────────────────────────────────────────────────────┤
│   Per-mathematical-area entry points                  │
│   aring, ring, ringelement, ringmap, matrix,          │
│   mutable-matrix, freemodule, monoid,                 │
│   monomial-ordering, monomial-ideal                   │
├──────────────────────────────────────────────────────┤
│   Per-algorithm entry points                          │
│   groebner, cra, factory, flint, cone, polyroots,     │
│   NAG, random                                          │
└──────────────────────────────────────────────────────┘
```

Each `.h` file is small (typically 20-100 declarations); the
matching `.cpp` is where the actual dispatch lives.

## Pattern: `IM2_<Type>_<verb>`

Every public function follows the convention:

```c
EngineReturnType IM2_<Type>_<verb>(<Type>* obj, args...);
```

For example:

```c
const Matrix* IM2_Matrix_multiply(const Matrix* a, const Matrix* b);
size_t       IM2_Matrix_n_rows(const Matrix* m);
M2_arrayint  IM2_Matrix_get_shape(const Matrix* m);
```

The `IM2_` prefix is the **historical "interface to M2"** namespace
identifier. New code should follow the same convention so the
interpreter side knows what to expect.

## Dispatch pattern

A typical `interface/<area>.cpp` looks like:

```cpp
#include "interface/<area>.h"
#include "engine-includes.hpp"
#include "<class>.hpp"   // the actual C++ class

const Matrix* IM2_Matrix_multiply(const Matrix* a, const Matrix* b)
{
  try {
    return Matrix::multiply(a, b);
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}
```

Three things to notice:

- **Exception → flag conversion**: C++ exceptions get caught and
  converted to the interpreter's error-flag mechanism via the
  `ERROR(...)` macro from [`file-error.md`](../file-error.md).
- **Null on failure**: the convention is to return `nullptr` on
  error. Combined with the error flag, the interpreter sees both
  pieces.
- **One-liner forwarding**: most functions are thin shims that
  forward to a static method or factory.

## Three-layer separation

```
M2 user code
   │
   ▼ d/<area>.dd (interpreter binding)
   │
   ▼ d/engine.dd (Ccode escape)
   │
   ▼ ────────────────── public C boundary ───────────
   │
   ▼ interface/<area>.cpp (dispatch + exception conversion)
   │
   ▼ ────────────────── private C++ surface ──────────
   │
   ▼ <class>.cpp (internal C++ implementation)
```

The middle layer (this directory) is the **only place** allowed
to:

- Catch C++ exceptions and translate to interpreter-visible
  errors.
- Convert between `M2_*` types (the interpreter's wire format) and
  engine internal types.
- Manage opaque-pointer marshalling.

## File-by-file roles

### Type aliases

| File | Purpose |
|---|---|
| [`file-m2-types-interface.md`](file-m2-types-interface.md) | `M2_string`, `M2_arrayint`, etc. — opaque types crossing the boundary |
| [`file-m2-mem-interface.md`](file-m2-mem-interface.md) | Allocation hooks the interpreter calls back into |
| [`file-gmp-util-interface.md`](file-gmp-util-interface.md) | GMP/MPFR shared helpers |
| [`file-computation-interface.md`](file-computation-interface.md) | `Computation` status / stop-condition enums |

### Mathematical-area entry points

| File | Engine area |
|---|---|
| [`file-aring-interface.md`](file-aring-interface.md) | `aring` framework (coefficient rings) |
| [`file-ring-interface.md`](file-ring-interface.md) | Legacy `Ring` framework |
| [`file-ringelement-interface.md`](file-ringelement-interface.md) | `RingElement` operations |
| [`file-ringmap-interface.md`](file-ringmap-interface.md) | `RingMap` (homomorphisms) |
| [`file-matrix-interface.md`](file-matrix-interface.md) | `Matrix` (immutable) |
| [`file-mutable-matrix-interface.md`](file-mutable-matrix-interface.md) | `MutableMatrix` |
| [`file-freemodule-interface.md`](file-freemodule-interface.md) | `FreeModule` |
| [`file-monoid-interface.md`](file-monoid-interface.md) | `Monoid` |
| [`file-monomial-ordering-interface.md`](file-monomial-ordering-interface.md) | `MonomialOrdering` |
| [`file-monomial-ideal-interface.md`](file-monomial-ideal-interface.md) | `MonomialIdeal` |

### Per-algorithm entry points

| File | Purpose |
|---|---|
| [`file-groebner-interface.md`](file-groebner-interface.md) | GB and resolution entry points |
| [`file-cra-interface.md`](file-cra-interface.md) | Chinese remainder + rational reconstruction |
| [`file-factory-interface.md`](file-factory-interface.md) | Polynomial GCD / factoring (Factory library) |
| [`file-flint-interface.md`](file-flint-interface.md) | FLINT primality / factoring |
| [`file-cone-interface.md`](file-cone-interface.md) | Cone operations |
| [`file-polyroots.md`](file-polyroots.md) | Univariate root finding (MPSolve) |
| [`file-NAG-interface.md`](file-NAG-interface.md) | Numerical algebraic geometry |
| [`file-random-interface.md`](file-random-interface.md) | RNG |

## Why this directory exists

Three motivations:

1. **Boundary discipline** — keeping public C API in one place
   means it's easy to verify the "no internal C++ leaks" rule.
2. **Forward declarations** — every header in `interface/` can be
   included from `.d`-generated C without bringing in transitive
   C++ dependencies.
3. **Versionability** — if M2 ever wants to expose a stable engine
   ABI to other languages (Python, Julia, ...), this directory is
   the natural starting point.

## How to add a new public function

1. **Declare** in the appropriate `interface/<area>.h`.
2. **Define** the dispatch in `interface/<area>.cpp` (thin shim
   forwarding to internal class).
3. **Wrap** C++ exceptions to error flags.
4. **Expose** via `d/<area>.dd` with a `Ccode(...)` escape.

The legacy aggregating header [`../file-engine-h.md`](../file-engine-h.md)
will need an entry too if older `.d` code includes it.

## Used by

- The interpreter, exclusively. End users never see this directory.
- Engine developers extending the boundary.

## Related

- [`README.md`](README.md) — interface/ navigation hub.
- [`../architecture.md`](../architecture.md) — engine architecture
  (this directory is its boundary).
- [`../file-engine-h.md`](../file-engine-h.md) — legacy
  aggregating header.
- [`../../d/file-engine-dd.md`](../../d/file-engine-dd.md) — the
  `.dd` side that calls into here.
