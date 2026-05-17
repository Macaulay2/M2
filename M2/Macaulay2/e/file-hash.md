# `hash.hpp` — `EngineObject` and `MutableEngineObject`

`hash.hpp` declares **`EngineObject`** and **`MutableEngineObject`** —
the two base classes from which essentially every long-lived engine
class derives. They provide GC integration, lazy hash computation, and
the virtual-destructor anchor every engine subclass shares.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## `EngineObject`

```cpp
#include "newdelete.hpp"
#include <cassert>
#include <M2/gc-include.h>

class EngineObject : public our_new_delete {
private:
    mutable unsigned int mHashValue;

public:
    EngineObject() : mHashValue(0) {}

    virtual ~EngineObject() { /* nothing to do here */ }

    unsigned int hash() const {
        if (mHashValue == 0) {
            mHashValue = computeHashValue();
            if (mHashValue == 0) mHashValue = 1;
        }
        return mHashValue;
    }
    // ...
};
```

Three responsibilities:

1. **GC integration** — `our_new_delete` routes allocation through
   bdwgc. Every `EngineObject` is GC-managed.
2. **Lazy hash** — `mHashValue` starts at 0 ("not yet computed"). The
   first call to `hash()` invokes the subclass's `computeHashValue()`
   and caches the result. The 0-to-1 bump ensures the cache marker
   stays distinguishable from a valid hash.
3. **Virtual destructor anchor** — the empty virtual `~EngineObject`
   makes `delete some_engine_object_ptr` safe even when the static
   type is the base class.

## `MutableEngineObject` (declared below)

The header also declares `MutableEngineObject` — an `EngineObject`
specialisation that **invalidates the hash cache on each mutation**.
Mutable engine objects (`MutableMatrix`, GB intermediate state, etc.)
inherit from this; immutable ones inherit from `EngineObject` directly.

The invalidation discipline keeps the lazy-hash invariant honest: if
the object changes, its hash must be recomputed, so the cache is reset.

## `computeHashValue`

The base classes leave `computeHashValue` for each subclass to
implement. Implementations vary:

- **`PolynomialRing`** — hash of (coefficient-ring hash, monoid hash,
  ring flags).
- **`Matrix`** — hash of (source hash, target hash, column entries).
- **`RingElement`** — hash of (ring hash, value-specific hash).

The hash values are used by the interpreter's `HashTable` to identify
engine objects across M2-side caches.

## Why a custom hash mechanism

`std::hash<T>` won't do — the engine's objects are too large to hash
in full each time, and the hash needs to be stable across a single
session (used as a cache key). The lazy-cached design pays the cost
once and amortises it across every lookup thereafter.

## Used by

Every engine class derives from one of these two, directly or
transitively. A non-exhaustive list:

- `Ring`, `Monoid`, `Matrix`, `FreeModule`, `RingElement`, `RingMap`,
  `MonomialIdeal`, `Computation`, `GBComputation`, `ResolutionComputation`,
  `MutableMatrix`, `MutableComplex`.

## Related

- [`utilities.md`](utilities.md) — area overview.
- `newdelete.hpp` (in subdirectories) — supplies `our_new_delete`.
- bdwgc submodule under [`../../submodules/README.md`](../../submodules/README.md).
