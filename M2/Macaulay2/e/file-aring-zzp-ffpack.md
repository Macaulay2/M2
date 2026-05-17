# `aring-zzp-ffpack.{cpp,hpp}` — `M2::ARingZZpFFPACK` (Z/p via FFLAS-FFPACK)

`aring-zzp-ffpack.cpp` implements **`Z/p`** using the FFLAS-FFPACK
library's `Modular<…>` field type. It is the fastest Z/p back end the
engine ships for very small primes, especially in matrix-heavy code,
because FFLAS-FFPACK ties into BLAS-style dispatch for dense linear
algebra.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"

#include <type_traits>     // define bool_constant to fix issue #2347
#include <utility>
#include <ratio>           // fix compilation errors on some macs

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#define bool_constant givaro_bool_constant
#include <fflas-ffpack/ffpack/ffpack.h>
#undef bool_constant
#pragma GCC diagnostic pop
```

Three things to note:

1. **`#define bool_constant givaro_bool_constant`** — there is a name
   collision between `std::bool_constant` (C++17) and Givaro's own
   `bool_constant`. The redefinition (locally scoped via `#define` /
   `#undef`) lets both libraries coexist.
2. **`<ratio>`** — a workaround for a macOS-specific compilation error
   in FFLAS-FFPACK; the `<ratio>` header pulls in the missing typedefs.
3. **Diagnostic pragmas** — silence FFLAS-FFPACK's internal compiler
   warnings.

The bug number `#2347` referenced in the comment is the original M2
issue tracking this Givaro-vs-`std` collision.

## Why FFLAS-FFPACK

FFLAS-FFPACK is the gold-standard library for finite-field linear
algebra over `Z/p`. It exposes:

- **Modular arithmetic** with reduced representatives in `[0, p)`.
- **BLAS-routed matrix multiplication** — `Z/p` matrices are
  reinterpreted as `double` matrices, multiplied via BLAS, then
  reduced. For primes small enough that the result fits in 53-bit
  mantissa precision, this is dramatically faster than naïve
  element-wise multiplication.

The aring path doesn't directly use the matrix routines — those live
in [`dmat-zzp-ffpack.hpp`](file-dmat.md) — but the same field instance
serves both.

## Class structure

```cpp
namespace M2 {

class ARingZZpFFPACK : public SimpleARing<ARingZZpFFPACK> {
public:
    static const RingID ringID = ring_ZZpFFPACK;
    typedef FFPACK::Modular<double>::Element elem;
    // arithmetic, conversion, RNG
};

}
```

`elem` is FFLAS-FFPACK's `Modular<double>::Element`, typically a
`double` representing a reduced integer in `[0, p)`. The choice of
`Modular<double>` (rather than `Modular<int>` or `Modular<int64_t>`)
is what enables the BLAS dispatch.

## Prime size limits

The BLAS-route trick works only for primes where `p^2 * n_rows` fits in
the `double` mantissa. In practice, primes up to about `2^25` are safe
for matrices of typical size. Larger primes still work — the
implementation falls back to `int64_t`-based arithmetic — but lose the
BLAS speed.

The dispatcher in [`file-aring.md`](file-aring.md) picks
`ARingZZpFFPACK` for small primes when FFLAS-FFPACK is available.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring.md`](file-aring.md), [`file-coeffrings.md`](file-coeffrings.md)
  — framework / registry.
- [`file-dmat.md`](file-dmat.md), [`dmat-zzp-ffpack.hpp`](matrices.md)
  — matrix back end built on this.
- [`file-aring-zzp-flint.md`](file-aring-zzp-flint.md) — FLINT alternative
  (no BLAS dispatch).
- fflas-ffpack & givaro under [`submodules/`](../../submodules/README.md).
