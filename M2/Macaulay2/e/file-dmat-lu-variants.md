# `dmat-lu-inplace.hpp`, `dmat-lu-qq.hpp`, `dmat-lu-zzp-ffpack.hpp`, `dmat-lu-zzp-flint.hpp` — `DMatLinAlg<R>` specialisations

These four files together provide **per-ring dense LU
specialisations** for `DMatLinAlg<R>` — rank, determinant, LU
decomposition, null space, rank profile.

Part of the [engine](README.md) — matrices.

[← engine overview](README.md) · [matrices](matrices.md)

## The four files

| File | Specialisation for | Backend |
|---|---|---|
| `dmat-lu-inplace.hpp` | `M2::ARingGFFlint`, `M2::ARingGFFlintBig` | FLINT `fq_*_mat_lu` (in-place) |
| `dmat-lu-qq.hpp` | `M2::ARingQQ` | FLINT `fmpq_mat_*` |
| `dmat-lu-zzp-ffpack.hpp` | `M2::ARingZZpFFPACK` | FFPACK |
| `dmat-lu-zzp-flint.hpp` | `M2::ARingZZpFlint` | FLINT `nmod_mat_*` |

Each provides a `template <> class DMatLinAlg<RingType>` body
overriding the generic implementation.

## `dmat-lu-inplace.hpp`

```cpp
#include "dmat.hpp"
#include "mat-elem-ops.hpp"
#include "mat-util.hpp"

#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/fq_nmod_mat.h>  // for fq_nmod_mat_lu, fq_zech_mat_lu
```

Wraps FLINT's **in-place LU** for Galois fields. "In-place" means
the LU factors overwrite the input matrix — saves memory but
destroys the original. Callers that need to preserve `M` copy
first.

## `dmat-lu-qq.hpp`

```cpp
template <>
class DMatLinAlg<M2::ARingQQ>
{
 public:
  typedef M2::ARingQQ RingType;
  typedef typename RingType::ElementType ElementType;
  typedef DMat<RingType> Mat;
  ...
};
```

LU over `QQ`. Internally:

1. Find common denominator across the matrix.
2. Scale to integers.
3. Use FLINT `fmpz_mat_*` over `ZZ`.
4. Convert results back to `QQ`.

This is *much* faster than naive rational arithmetic, which would
blow up denominators on every elimination step.

## `dmat-lu-zzp-ffpack.hpp`

```cpp
namespace ffpackInterface {
size_t rank(const DMatZZpFFPACK& A);
void determinant(const DMatZZpFFPACK& A, ZZpFFPACK::ElementType& result_det);
M2_arrayintOrNull rankProfile(const DMatZZpFFPACK& A, bool row_profile);
...
}
```

Uses [FFPACK](https://github.com/linbox-team/fflas-ffpack) —
ultra-optimised finite-field LAPACK-like routines. Very fast for
medium-sized fields (`p` ≤ ~2^23).

## `dmat-lu-zzp-flint.hpp`

```cpp
#include <flint/nmod_mat.h>  // for nmod_mat_lu, nmod_mat_rank, nmod_mat_det
```

FLINT's `nmod_mat_t` — competitive with FFPACK in many cases,
sometimes faster, sometimes slower. M2's heuristic picks based on
matrix size and prime.

## Including discipline

`dmat-lu-*.hpp` files have a comment near the top:

```cpp
// This file should only be included once, by what?
```

The "by what?" is rhetorical — these files extend
`DMatLinAlg<R>` for specific `R`, and including them twice would
cause duplicate-specialisation linker errors. They're meant to be
included by `dmat-linalg.hpp` (the dispatcher).

## Used by

- `DMatLinAlg<R>` for these specific `R`s.
- M2's `rank M`, `det M`, `LUdecomposition M` for these rings.
- The F4 / GB engines when reducing Macaulay matrices.

## Related

- [`README.md`](README.md) — engine overview.
- [`matrices.md`](matrices.md) — area.
- [`file-dmat.md`](file-dmat.md) — `DMat<R>` base.
- [`file-mat-linalg.md`](file-mat-linalg.md) — generic
  `DMatLinAlg<R>` template.
- FFPACK, FLINT — external libraries.
