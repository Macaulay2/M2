# `mat-util.hpp` — generic matrix helpers (`displayMat`, etc.)

`mat-util.hpp` collects **templated helpers** for both `DMat<R>` and
`SMat<R>` matrices that don't fit naturally under
[`file-mat-arith.md`](file-mat-arith.md),
[`file-mat-elem-ops.md`](file-mat-elem-ops.md), or
[`file-mat-linalg.md`](file-mat-linalg.md). The first such helper —
`displayMat` — is the matrix-printing utility used throughout
debugging code.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## `displayMat`

```cpp
#include <assert.h>
#include "buffer.hpp"
#include "text-io.hpp"

template <typename Mat>
void displayMat(buffer &o, const Mat &A) {
    // Assumption: Mat is either DMat<RingType> or SMat<RingType>, in that it
    // defines the following:
    //   Mat::ElementType
    //   A.ring()
    //   A.numRows(), A.numColumns()
    //   ...
}
```

The template works on **any matrix type** that exposes the documented
duck-typed interface (`ElementType`, `ring()`, `numRows()`,
`numColumns()`, entry-iteration). Both `DMat<R>` and `SMat<R>` qualify;
so does any user-defined matrix following the convention.

The function writes a human-readable rendering to a
[`buffer`](file-buffer.md), using
[`text-io`](file-text-io.md)'s wrapping helpers to avoid runaway lines.

## Other utilities

The file collects other templated helpers as needed:

- **Iteration helpers** — visit each entry, skipping zeros.
- **Conversion helpers** — between matrix flavours.
- **Validation predicates** — `isSquareMatrix`, `isUpperTriangular`,
  etc.

The "miscellaneous mat helpers" theme lets new utilities accumulate
here without needing to spawn dedicated files.

## Why the assumption-based templating

The engine's `DMat<R>` and `SMat<R>` have different concrete types and
methods, but they share a small set of common operations. Rather than
making both inherit from a common abstract base (which would force
virtual dispatch), `mat-util.hpp` uses C++ duck typing: the templates
require only the listed methods.

This pattern matches the rest of the matrix-template layer
([`file-mat-arith.md`](file-mat-arith.md), etc.) and lets each
specialisation be inlined per call site.

## Used by

- Debugging code throughout the engine — `displayMat` is the standard
  way to dump a matrix to stdout or to an error buffer.
- The `unit-tests/` C++ suite for matrix sanity checks.
- M2 user output paths in `interface/matrix.cpp` and
  `interface/mutable-matrix.cpp`.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-mat-arith.md`](file-mat-arith.md), [`file-mat-elem-ops.md`](file-mat-elem-ops.md),
  [`file-mat-linalg.md`](file-mat-linalg.md) — sibling matrix helpers.
- [`file-dmat.md`](file-dmat.md), `smat.hpp` — matrix templates.
- [`file-buffer.md`](file-buffer.md), [`file-text-io.md`](file-text-io.md)
  — output mechanism.
