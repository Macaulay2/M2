# `mutablemat-defs.hpp` and `mutablemat-imp.hpp` — `MutableMat<Mat>` internals

`mutablemat-defs.hpp` and its companion `mutablemat-imp.hpp` are the
**template declaration** and **template implementation** halves of
`MutableMat<Mat>` — the templated class behind the abstract
[`MutableMatrix`](file-mutablemat.md) interface.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Why two files

The engine splits the templated mutable-matrix code in three:

| File | Role |
|---|---|
| [`mutablemat.hpp`](file-mutablemat.md) | Umbrella header — `#include`s the other two plus SLP bits |
| `mutablemat-defs.hpp` (this file) | Template declarations; coefficient-ring forward declarations |
| `mutablemat-imp.hpp` | Template implementations (heavy `#include`s into NAG / SLP machinery) |

The split avoids dragging the SLP and NAG includes into every TU that
just needs `MutableMatrix` declarations. Concretely, a translation unit
working with `MutableMatrix*` includes the umbrella; one that needs to
actually instantiate the template (i.e. specialise on a ring) pulls in
the `-defs.hpp` directly.

## `mutablemat-defs.hpp` content

```cpp
#include <iostream>
#include "mat.hpp"

namespace M2 {
class ARingZZp;
class ARingRR;
class ARingCC;
class ARingRRR;
class ARingCCC;
}

template <typename RT> class DMat;
template <typename RT> class SMat;

// declarations of MutableMat<DMat<R>> and MutableMat<SMat<R>>:
template <typename Mat>
class MutableMat : public MutableMatrix {
    Mat mat;
public:
    // ... typed wrappers around mat's methods ...
};
```

`MutableMat<Mat>` is the **bridge from the templated `DMat`/`SMat`** to
the **virtual `MutableMatrix` base**: it owns a `Mat` instance and
implements `MutableMatrix`'s virtual methods by forwarding to `mat`'s
templated methods.

This is the engine's standard pattern for "templated implementation
behind a virtual interface" — analogous to
[`file-aring-glue.md`](file-aring-glue.md)'s `ConcreteRing<R>`.

## `mutablemat-imp.hpp` content

Heavy template definitions, including the SLP-evaluator factory:

```cpp
template <typename Mat>
M2SLEvaluator *MutableMat<Mat>::createSLEvaluator(M2SLProgram   *P,
                                                  M2_arrayint    constsPos,
                                                  M2_arrayint    varsPos) const {
    if (n_rows() != 1 || n_cols() != constsPos->len) {
        ERROR("1-row matrix expected; or numbers of constants don't match");
        return nullptr;
    } else
        return new M2SLEvaluator(
            new SLEvaluatorConcrete<typename Mat::CoeffRing>(
                &(P->value()), constsPos, varsPos, this));
}
```

The implementation pulls in [`SLP-imp.hpp`](file-SLP.md) and
[`NAG.h`](interface/file-NAG-interface.md) — exactly the chain of
includes the `-defs.hpp` was trying to avoid. By keeping the
implementation in its own file, the dependency graph stays clean.

## Used by

- [`file-mutablemat.md`](file-mutablemat.md) — the umbrella header.
- Per-ring `MutableMat<DMat<R>>` specialisations the dispatcher
  constructs.
- [`file-NAG.md`](file-NAG.md), [`file-SLP.md`](file-SLP.md) — pull in
  the imp header for SLP evaluator construction.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-mutablemat.md`](file-mutablemat.md) — umbrella header.
- [`file-dmat.md`](file-dmat.md), `smat.hpp` — wrapped matrix
  templates.
- [`file-SLP.md`](file-SLP.md), [`file-NAG.md`](file-NAG.md) — SLP
  consumers.
