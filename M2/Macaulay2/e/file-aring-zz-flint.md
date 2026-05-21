# `aring-zz-flint.{cpp,hpp}` — `M2::ARingZZ` (FLINT-backed)

`aring-zz-flint.cpp` implements the engine's **FLINT-backed `Z`** —
arbitrary-precision integers using FLINT's `fmpz_t` value type as the
storage. It is one of two ZZ implementations alongside the legacy
GMP-backed one in [`aring-zz-gmp.cpp`](coefficient-rings.md).

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Why FLINT for ZZ

FLINT's `fmpz_t` is significantly faster than GMP's `mpz_t` for small
integers (the common case) because:

- **Small-value inlining** — values fitting in a single 64-bit word are
  stored inline; the heap-allocated `mpz_t` representation is used only
  for larger values.
- **Specialised primitives** — `fmpz_add`, `fmpz_mul`, `fmpz_pow_ui`,
  `fmpz_set_si`, `fmpz_set_mpz` all have tight implementations targeting
  the small-value path first.

Larger integers fall back to GMP underneath FLINT, so the worst case is
identical.

## File preamble (worth knowing)

```cpp
#include "interface/gmp-util.h"  // for mpz_reallocate_limbs

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "exceptions.hpp"
#include "ZZ.hpp"

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/flint.h>
#include <flint/fmpz.h>
#pragma GCC diagnostic pop
```

Three details to note:

1. The `<M2/gc-include.h>` include **must precede** any FLINT include.
   FLINT defines memory allocation hooks via `flint_malloc` and friends;
   the M2 header rewires them to go through bdwgc.
2. The `#pragma`s suppress warnings about implicit narrowing inside
   FLINT's own headers — not engine bugs.
3. The bridge to GMP via `mpz_reallocate_limbs` (from `gmp-util.h`) lets
   the engine pass `fmpz_t` values to code that wants `mpz_t` without
   copying limbs.

## Class structure

```cpp
namespace M2 {

class ARingZZ : public SimpleARing<ARingZZ> {
public:
    static const RingID ringID = ring_ZZ;
    typedef fmpz elem;
    // ... arithmetic, conversion, RNG ...
};

}
```

`SimpleARing<ARingZZ>` is the CRTP base; the derived class supplies the
ring-specific arithmetic methods. The `elem` typedef is `fmpz` (the
single-int integer head, distinct from `fmpz_t` which is an array of
one `fmpz`).

## Heavy use sites

`ARingZZ` is the default ZZ used throughout newer engine code:

- [`dmat-zz-flint.hpp`](file-dmat.md) — dense ZZ matrices.
- [`reducedgb-ZZ.{cpp,hpp}`](file-reducedgb.md) — reduced GB over ZZ.
- FLINT-backed GB / resolution paths.

The legacy [`ZZ.{cpp,hpp}`](coefficient-rings.md) is still around for
code paths that haven't migrated, but new code targets `ARingZZ`.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring.md`](file-aring.md) — `aring` framework.
- [`file-coeffrings.md`](file-coeffrings.md) — registry pattern.
- `aring-zz-gmp.{cpp,hpp}` — GMP counterpart (still present).
- FLINT submodule under [`submodules/`](../../submodules/README.md).
