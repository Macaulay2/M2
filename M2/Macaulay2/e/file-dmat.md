# `dmat.{cpp,hpp}` — `DMat<ACoeffRing>` — generic dense-matrix template

`DMat` is the engine's **dense-matrix template**, parameterised on a
coefficient ring. Specialised instantiations exist for FLINT-backed ZZ, QQ,
Z/p, GF, and CC; everything else uses the generic instantiation.

Part of the [Matrices](matrices.md) area; the dense back end of
[`MutableMatrix`](file-mutablemat.md).

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Template structure

```cpp
template <typename ACoeffRing>
class DMat;

// Specialisations are *included* (not just declared) from dmat.hpp:
#include "dmat-zz-flint.hpp"
#include "dmat-qq-flint.hpp"
#include "dmat-zzp-flint.hpp"
#include "dmat-gf-flint-big.hpp"
#include "dmat-gf-flint.hpp"

template <typename ACoeffRing>
class DMat {
   // Generic (fallback) implementation:
   //   - column-major dense storage of ring_elem entries,
   //   - operations via ACoeffRing::add, mult, ...
};
```

The pattern is "**include all specialisations from the umbrella header**".
This means consumers `#include "dmat.hpp"` and automatically pick the right
implementation for the ring type at compile time.

## Storage

Generic `DMat` stores entries in a column-major `std::vector<ring_elem>` of
size `n_rows * n_cols`. Specialisations override storage to use the
back-end-native type (e.g. `fmpz_mat_t` for ZZ via FLINT, `nmod_mat_t` for
Z/p via FLINT) — this is where the speed comes from.

## Operations

Templated operations live in companion headers:

| Header | What it provides |
|---|---|
| `mat-arith.hpp` | `add`, `subtract`, `mult`, `negate` |
| `mat-elem-ops.hpp` | Row / column ops, swap, scale |
| `mat-linalg.hpp` | `rank`, `LU`, `solve`, `determinant`, `inverse`, `nullspace` |
| `mat-util.hpp` | Iteration helpers |
| `mat-jordan.hpp` | Jordan canonical form |

Each takes `DMat<R>&` and uses `R`'s arithmetic.

## LU specialisations

The LU-decomposition path is performance-critical and has its own per-ring
files:

| File | Ring |
|---|---|
| `dmat-lu.hpp`, `dmat-LU.hpp`, `dmat-LU-template.hpp` | Generic template |
| `dmat-lu-inplace.hpp` | In-place reusable buffer |
| `dmat-lu-qq.hpp` | QQ |
| `dmat-lu-zzp-flint.hpp` | Z/p via FLINT |
| `dmat-lu-zzp-ffpack.hpp` | Z/p via FFLAS-FFPACK (fastest path) |

## How `MutableMatrix` picks a `DMat`

When [`MutableMatrix`](file-mutablemat.md) is constructed with a specific
coefficient ring, the factory `MutableMatrix::create(R, …, is_dense=true)`
chooses the appropriate `DMat<R>` instantiation and wraps it in the
abstract `MutableMatrix` interface. Operations dispatch through the
abstract API and then inline the templated code.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-mutablemat.md`](file-mutablemat.md) — abstract front-end.
- [`coefficient-rings.md`](coefficient-rings.md) — entry rings.
- [`smat.hpp`](matrices.md) — sparse counterpart.
- fflas-ffpack / flint submodules under
  [`submodules/`](../../submodules/README.md).
