# `aring-zzp-flint.{cpp,hpp}` — `M2::ARingZZpFlint` (FLINT `nmod`)

`aring-zzp-flint.cpp` implements **`Z/p`** using FLINT's `nmod_t` value
type. Among the engine's several Z/p back ends, this one is the standard
choice for small-to-medium primes when FFLAS-FFPACK is not available or
applicable.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## How FLINT represents Z/p

```cpp
#include <flint/flint.h>  // for nmod_t
#ifdef HAVE_FLINT_NMOD_H
  #include <flint/nmod.h>  // for nmod_neg, nmod_add, nmod_div, nmod_mul
#endif
```

FLINT stores `Z/p` modular arithmetic via `nmod_t`, a struct holding:

- The modulus `n` itself.
- A reciprocal `ninv` for Barrett or Newton-style fast modular reduction.
- The bit-size used for the reduction algorithm.

Each arithmetic operation (`nmod_add`, `nmod_mul`, `nmod_neg`, `nmod_div`)
uses the precomputed `ninv` to avoid a slow integer division per multiply.
For primes up to roughly `2^63`, this approach is faster than the
log/exp-table approach used by [`CoefficientRingZZp`](file-coeffrings.md).

## Class shape

```cpp
namespace M2 {

class ARingZZpFlint : public SimpleARing<ARingZZpFlint> {
public:
    static const RingID ringID = ring_ZZpFlint;
    typedef mp_limb_t elem;   // nmod's underlying unsigned word
    // ...
};

}
```

The `elem` type is `mp_limb_t` (a 64-bit unsigned word). Values are
*reduced representatives* in `[0, p)`. Arithmetic delegates to FLINT's
`nmod_*` functions.

## `HAVE_FLINT_NMOD_H` guard

FLINT moved `nmod_*` from `flint.h` into its own header in a recent
release. The `#ifdef HAVE_FLINT_NMOD_H` guard accommodates both
arrangements; the build's `configure` step decides which path applies.

## Heavy use sites

- [`dmat-zzp-flint.hpp`](file-dmat.md) — dense Z/p matrices.
- F4 GB engines when the user requests FLINT explicitly via
  `Strategy => …`.
- Various resolution and GB code paths.

When FFLAS-FFPACK is available, [`aring-zzp-ffpack.cpp`](coefficient-rings.md)
is typically faster for very small primes because it can use BLAS-style
matrix-arithmetic dispatch.

## Random elements

```cpp
#include "interface/random.h"
```

The header pulls in [`interface/random.h`](interface/README.md) for
generating random elements; the implementation uses FLINT's `flint_rand_t`
seeded from the engine's RNG.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring.md`](file-aring.md), [`file-coeffrings.md`](file-coeffrings.md)
  — framework / registry.
- `aring-zzp-ffpack.{cpp,hpp}` — FFLAS-FFPACK alternative.
- `aring-zzp.{cpp,hpp}` — generic table-based path (`CoefficientRingZZp`).
- FLINT submodule under [`submodules/`](../../submodules/README.md).
