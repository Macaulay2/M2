# `dmat-zz-flint.hpp` — `DMat<ARingZZ>` FLINT specialisation

`dmat-zz-flint.hpp` is the **FLINT-backed specialisation** of the
engine's [`DMat<R>`](file-dmat.md) template for the ZZ ring. The
specialisation stores the matrix as a FLINT `fmpz_mat_t` and routes
arithmetic to FLINT's matrix routines — significantly faster than the
generic template path for large integer matrices.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include <assert.h>
#include <utility>             // for swap
#include "aring-zz-flint.hpp"  // for ARingZZ

#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/fmpz_mat.h>    // for fmpz_mat_t, fmpz_mat_entry, fmpz_mat_clear, fmpz_m...
#pragma GCC diagnostic pop
```

Same pattern as every FLINT-backed file:

1. `<M2/gc-include.h>` first so FLINT's malloc hooks route through
   bdwgc.
2. Diagnostic pragmas around the FLINT header.
3. `<flint/fmpz_mat.h>` for the matrix arithmetic.

## What's specialised

The header provides a **full template specialisation**:

```cpp
template <>
class DMat<M2::ARingZZ> {
    fmpz_mat_t mMat;   // FLINT-managed storage
    // ...
};
```

Every method that the generic `DMat<R>` exposes is overridden here:

- **Storage** — backed by `fmpz_mat_t` (a FLINT `mp_limb_t`-array).
- **Element access** — via `fmpz_mat_entry(mMat, i, j)`.
- **Arithmetic** — `fmpz_mat_add`, `fmpz_mat_mul`, etc.
- **LU / rank / det** — `fmpz_mat_solve`, `fmpz_mat_rank`,
  `fmpz_mat_det`.
- **Inverse / nullspace** — `fmpz_mat_inv`, `fmpz_mat_nullspace`.

Every operation goes through FLINT's hand-tuned implementation, which
typically wins over the generic template by 10–100× on large
matrices.

## Constructors / destructors

The header takes care to:

- Initialise `fmpz_mat_t` with the correct dimensions via
  `fmpz_mat_init`.
- Free the matrix via `fmpz_mat_clear` in the destructor.
- Use `swap` semantics for moves (since copying a `fmpz_mat_t` is
  expensive).

The GC-managed storage of the underlying limbs is the same as for
[`file-aring-zz-flint.md`](file-aring-zz-flint.md) — `fmpz_t`s
allocated through the FLINT path, with limb memory routed via bdwgc.

## Used by

- [`file-mutablemat.md`](file-mutablemat.md) — when the user
  constructs a mutable matrix over `ZZ`, this is the back end.
- [`file-LLL.md`](file-LLL.md) — LLL over ZZ matrices runs through
  `DMat<ARingZZ>`.
- [`file-mat-linalg.md`](file-mat-linalg.md) — the templated linear
  algebra dispatches to this for ZZ matrices.

## Sibling specialisations

Every `dmat-*-flint.hpp` follows the same pattern with a different
FLINT type:

| File | Specialisation | FLINT type |
|---|---|---|
| `dmat-zz-flint.hpp` (this file) | `DMat<ARingZZ>` | `fmpz_mat_t` |
| `dmat-qq-flint.hpp` | `DMat<ARingQQFlint>` | `fmpq_mat_t` |
| `dmat-zzp-flint.hpp` | `DMat<ARingZZpFlint>` | `nmod_mat_t` |
| `dmat-gf-flint.hpp` | `DMat<ARingGFFlint>` | `fq_zech_mat_t` |
| `dmat-gf-flint-big.hpp` | `DMat<ARingGFFlintBig>` | `fq_nmod_mat_t` |

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-dmat.md`](file-dmat.md) — generic template.
- [`file-aring-zz-flint.md`](file-aring-zz-flint.md) — element type.
- FLINT submodule under [`../../submodules/README.md`](../../submodules/README.md).
