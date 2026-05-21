# `aring-gf-flint.{cpp,hpp}` — `M2::ARingGFFlint` (small GF via FLINT Zech)

`aring-gf-flint.cpp` implements the engine's **Galois field for small
extensions** using FLINT's `fq_zech_*` family — Zech logarithm tables for
elements of `GF(p^k)` where the size is small enough that exhaustive
table storage is practical. For large extensions the engine instead uses
[`aring-gf-flint-big.cpp`](coefficient-rings.md) (`fq_nmod_*`).

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## What FLINT's `fq_zech` gives us

A **Zech logarithm table** for `GF(q)` (where `q = p^k`):

- Precomputed `α^i` for `i = 0, 1, …, q-2` (where `α` is a primitive root).
- Precomputed `Zech(i) = log_α(1 + α^i)` for the same range.

With these tables:

- Multiplication `α^i · α^j = α^{(i+j) mod (q-1)}`.
- Addition `α^i + α^j = α^j · (1 + α^{i-j}) = α^{j + Zech(i-j)}` (for
  `i >= j`).

Every operation becomes O(1) integer arithmetic on log indices. For
`q` up to a few thousand, this is the fastest possible representation.

## Header preamble

```cpp
#include <M2/gc-include.h>

// includes gmp.h, which is required for FLINT functions that use GMP
#include <M2/math-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fq_nmod.h>    // for fq_nmod_clear, fq_nmod_init, ...
#include <flint/fq_zech.h>    // for fq_zech_clear, fq_zech_ctx_clear, ...
#include <flint/nmod_poly.h>  // for nmod_poly_set_coeff_ui, ...
#pragma GCC diagnostic pop

#include "interface/random.h"
```

The `fq_zech_*` functions in turn depend on `fq_nmod_*` and
`nmod_poly_*`, all from FLINT.

## Class shape

```cpp
namespace M2 {

class ARingGFFlint : public SimpleARing<ARingGFFlint> {
public:
    static const RingID ringID = ring_GFFlintZech;
    typedef ulong elem;           // a Zech log index
    // arithmetic and conversions
};

}
```

`elem` is `ulong` (FLINT's unsigned word type) — an index into the Zech
table. Storage is one machine word per field element.

## When to choose this over `aring-gf-flint-big`

The choice is **automatic** at construction time:

- `q = p^k` small enough that the Zech table fits in memory → this file.
- Otherwise → [`aring-gf-flint-big.cpp`](coefficient-rings.md), which
  uses `fq_nmod_*` (polynomial-quotient representation, slower per
  operation but no table-size limit).

The cutoff is configurable via FLINT's
`fq_zech_ctx_init_modulus`. In practice, anything past `q = 2^16` or so
moves to the "big" path.

## Heavy use sites

- [`dmat-gf-flint.hpp`](file-dmat.md) — dense GF matrices.
- Code paths in algebraic coding theory packages.
- Anywhere `GF(8)`, `GF(16)`, `GF(32)`, ... is used.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring.md`](file-aring.md), [`file-coeffrings.md`](file-coeffrings.md)
  — framework / registry.
- `aring-gf-flint-big.{cpp,hpp}` — large-extension sibling.
- `aring-m2-gf.{cpp,hpp}` — native M2 GF (slower but no FLINT dep).
- `GF.{cpp,hpp}` — legacy table-based path.
- FLINT submodule under [`submodules/`](../../submodules/README.md).
