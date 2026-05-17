# `aring-qq-flint.{cpp,hpp}` — `M2::ARingQQFlint` (QQ via FLINT)

`aring-qq-flint.cpp` implements the engine's **FLINT-backed rationals**
using FLINT's `fmpq_t` value type. It is the modern default `QQ`
implementation; the older [`aring-qq-gmp.cpp`](coefficient-rings.md)
remains available.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Why FLINT for QQ

FLINT's `fmpq_t` builds on `fmpz_t` ([`file-aring-zz-flint.md`](file-aring-zz-flint.md))
and inherits its small-value inlining. For most everyday rationals
(numerator and denominator small enough to fit in a single word), all
arithmetic is inline; only blowup forces heap allocation. This is a
significant speedup vs. GMP's `mpq_t`, where every value is heap-allocated.

## Header preamble

```cpp
#include "interface/gmp-util.h"  // for mpz_reallocate_limbs

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/flint.h>
#include <flint/fmpq.h>
#include <flint/fmpz.h>
#pragma GCC diagnostic pop

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "exceptions.hpp"
#include "ring.hpp"  // for promote — to be moved out later
```

Same pattern as [`file-aring-zz-flint.md`](file-aring-zz-flint.md):
`<M2/gc-include.h>` first so FLINT's malloc hooks route through bdwgc;
diagnostic pragmas around FLINT's own headers; `<flint/fmpq.h>` for
rational arithmetic.

The comment "promote needs ring.hpp. After moving promote out, remove
it here!" flags a known refactor: cross-ring promotion currently lives
in `aring-qq-flint.cpp` but should migrate to `aring-translate.hpp`.

## Class structure

```cpp
namespace M2 {

class ARingQQFlint : public SimpleARing<ARingQQFlint> {
public:
    static const RingID ringID = ring_QQFlint;
    typedef fmpq elem;
    // arithmetic, conversion, normalisation, RNG
};

}
```

`elem` is `fmpq` (the rational data struct, not the array form `fmpq_t`).
Arithmetic uses FLINT's `fmpq_add`, `fmpq_mul`, etc., which automatically
normalise after every operation (so `(1/2) + (1/2) == 1` is stored as
`1` not `(2/2)`).

## Heavy use sites

- [`dmat-qq-flint.hpp`](file-dmat.md) — dense QQ matrices.
- F4 with rational coefficients (via CRT lifting; see
  [`interface/file-cra-interface.md`](interface/file-cra-interface.md)).
- Most M2 code that writes `QQ[x_1, …, x_n]` uses this back end through
  the [`interface/aring.h`](interface/README.md) entry point.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring.md`](file-aring.md), [`file-coeffrings.md`](file-coeffrings.md)
  — framework / registry.
- [`file-aring-zz-flint.md`](file-aring-zz-flint.md) — `ARingZZ` base.
- `aring-qq-gmp.{cpp,hpp}` — GMP counterpart.
- FLINT submodule under [`submodules/`](../../submodules/README.md).
