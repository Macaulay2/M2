# `fplll-interface.hpp`, `fplll-interface.cpp` — fplll lattice-reduction bindings

`fplll-interface.{hpp,cpp}` is the **engine-side wrapper for
[fplll](https://github.com/fplll/fplll)** — a C++ library for fast
lattice basis reduction (LLL, BKZ, fpLLL). Exposes `fp_LLL` as a
single entry point used by M2's `LLL` user function.

Part of the [engine](README.md) — utilities.

[← engine overview](README.md) · [utilities](utilities.md)

## What's exposed

```cpp
class MutableMatrix;

bool fp_LLL(MutableMatrix *M, MutableMatrix *U, int strategy);
```

One function. Takes:

- **`M`** — the matrix to reduce (rows are the basis vectors).
  Modified in place.
- **`U`** — optional unimodular transformation matrix. If
  non-null, the multiplier that converts the original basis to
  the reduced basis is written here.
- **`strategy`** — 0 = LLL, 1 = BKZ, etc.

Returns `true` on success.

## Implementation

```cpp
#include "fplll-interface.hpp"
#include "mutablemat.hpp"

#ifdef HAVE_FPLLL
#include <stddef.h>
#include <fplll.h>
#endif

bool fp_LLL(MutableMatrix *M, MutableMatrix *U, int strategy)
{
  (void) M;
  (void) U;
  ...
}
```

The whole body is wrapped in `#ifdef HAVE_FPLLL` — if fplll isn't
installed at build time, `fp_LLL` returns false. The `(void) M;
(void) U;` casts silence unused-variable warnings in the
non-fplll build.

This is a recurring pattern in M2: optional dependencies have a
"dummy" fallback so callers don't need to `#ifdef` themselves.

## What LLL does

Given a lattice basis (rows of `M`), LLL produces an *almost
orthogonal* basis where:

- Each basis vector is short (close to the shortest possible).
- The lattice spanned is identical.
- The transformation matrix `U` records how the original maps to
  the new.

Used heavily for:

- **Integer relation finding** — given approximate floats, find
  integer combinations that vanish.
- **Polynomial factorisation** over ZZ (van Hoeij).
- **Cryptanalysis** (mostly outside M2's purview).

## When `fp_LLL` runs

User invocation:

```m2
LLL M
LLL(M, Strategy => NTL)    -- alternative implementation
```

The M2 layer dispatches between:

- **fplll** (this file) — typically the fastest.
- **NTL** (via `LLLglue.cpp` if available) — older, sometimes more
  numerically stable.
- **Pure M2** — fallback that always works but is slow.

## Used by

- M2's `LLL M` operation.
- The interpreter via [`interface/`](interface/README.md).
- Some numerical algorithms that need a reduced basis.

## Related

- [`README.md`](README.md) — engine overview.
- [`file-mat.md`](file-mat.md) — operates on `MutableMatrix`.
- fplll — external linked library.
- [`utilities.md`](utilities.md) — area.
