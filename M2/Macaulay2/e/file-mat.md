# `mat.hpp`, `mat.cpp` — `MutableMatrix` base class

`mat.hpp` and `mat.cpp` define **`MutableMatrix`** — the abstract
base class for M2's mutable matrices. The dense
([`DMat`](file-dmat.md)) and sparse
([`SMat`](file-smat.md) if added) implementations inherit from it.

Part of the [engine](README.md) — matrices.

[← engine overview](README.md) · [matrices](matrices.md)

## What's declared

```cpp
class MutableMatrix : public MutableEngineObject
{
 protected:
  // protected state
 public:
  virtual size_t n_rows() const = 0;
  virtual size_t n_cols() const = 0;
  virtual const Ring* get_ring() const = 0;

  virtual void resize(size_t nrows, size_t ncols) = 0;
  virtual void set_entry(size_t r, size_t c, ring_elem a) = 0;
  ...
};
```

Pure-virtual abstract base. Concrete subclasses are:

- **`MutableMat<MatT>`** — templated wrapper over `DMat<R>` or
  `SMat<R>`.
- The wrapped types `DMat<R>` themselves provide the actual
  representation.

## `mat.cpp` — dispatch and trampolines

```cpp
#include "util.hpp"
#include "dmat.hpp"
#include "smat.hpp"
#include "mat.hpp"
#include "mutablemat.hpp"

#include "aring-RRR.hpp"
#include "aring-RR.hpp"
#include "aring-CCC.hpp"
#include "aring-zz-gmp.hpp"
#include "aring-zz-flint.hpp"
#include "aring-zzp.hpp"
#include "aring-zzp-ffpack.hpp"
#include "aring-zzp-flint.hpp"
#include "aring-m2-gf.hpp"
#include "aring-glue.hpp"
#include "aring-tower.hpp"
#include "aring-qq.hpp"
```

The huge include block tells the story: `mat.cpp` is the **type
dispatch hub**. Every ring kind that supports mutable matrices is
included here, and the trampolines in `mat.cpp` switch on
`get_ring()->ringID()` to pick the right concrete type.

This is where the legacy `Ring`-based API meets the modern
templated `aring`-based dense matrices.

## Why two layers (`MutableMatrix` and `MutableMat<MatT>`)

`MutableMatrix` is the **virtual interface** — what callers see
through the C ABI. `MutableMat<MatT>` is the **template wrapper**
that provides the virtuals by forwarding to the templated `MatT`
(`DMat<R>` or `SMat<R>`).

```
MutableMatrix       (virtual base, what interface uses)
   ▲
   │
MutableMat<MatT>    (template wrapper, MatT = DMat<R> or SMat<R>)
   │ contains
   ▼
MatT                (DMat<ARingZZp>, SMat<ARingQQGMP>, ...)
```

This indirection lets the inner-loop heavy matrix code stay
templated (fast) while the API stays virtual (uniform).

## Operations exposed

The basic API every `MutableMatrix` supports:

- **Shape** — `n_rows`, `n_cols`, `get_ring`.
- **Access** — `get_entry`, `set_entry`.
- **Row/column ops** — `swap_rows`, `scale_row`, `add_row`,
  `delete_rows`, `interchange_columns`.
- **Linear algebra** — `rank`, `determinant`, `LU_decomposition`,
  `solve_LU`, `null_space`, `inverse`.
- **High-level** — `transpose`, `mult`, `add`.

Linear-algebra methods often dispatch to FFPACK / FLINT for
finite-field work; to LAPACK for floating-point; to fraction-free
algorithms for `ZZ`.

## Used by

- M2 user code: every `MutableMatrix` operation.
- [`interface/mutable-matrix.cpp`](interface/file-mutable-matrix-interface.md)
  — C API.
- The F4 GB engine indirectly (uses `DMat` directly, but the
  outputs become `MutableMatrix`).

## Related

- [`README.md`](README.md) — engine overview.
- [`matrices.md`](matrices.md) — matrix area overview.
- [`file-dmat.md`](file-dmat.md) — dense implementation.
- [`file-smat.md`](file-smat.md) if added — sparse implementation.
- [`file-mutablemat-imp.md`](file-mutablemat-imp.md) if added —
  the `MutableMat<MatT>` wrapper.
