# `SLP-imp.hpp` — `SLEvaluatorConcrete<RT>` (templated SLP evaluator)

`SLP-imp.hpp` defines **`SLEvaluatorConcrete<RT>`** — the templated
implementation that evaluates a [`SLProgram`](file-SLP-defs.md) at
specific input values. The template is parameterised on the
coefficient ring `RT` so the inner evaluation loop inlines per ring.

Part of the [Other computations](computations.md) area.

[← per-area: computations](computations.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include <cstdlib>
#include <algorithm>
#include <dlfcn.h>
#include "timing.hpp"

// SLEvaluator
template <typename RT>
SLEvaluatorConcrete<RT>::SLEvaluatorConcrete(
    SLProgram                       *SLP,
    M2_arrayint                      cPos,
    M2_arrayint                      vPos,
    const MutableMat<SMat<RT> >     *consts)
    : mRing(consts->getMat().ring())
{
    // ...
}
```

The constructor takes:

- The `SLProgram` to evaluate.
- **`cPos`** — positions of the program's constants.
- **`vPos`** — positions of the program's variable inputs.
- **`consts`** — a row-matrix of pre-evaluated constants.

`mRing` is initialised from the matrix's ring — `SLEvaluatorConcrete`
will evaluate the SLP entirely in this ring's arithmetic.

## `dlfcn.h` for dynamic loading

The include of `<dlfcn.h>` (`dlopen`/`dlsym`) hints at an
**experimental JIT path**: the evaluator can in some configurations
compile an SLP to a shared library at runtime, load it with `dlopen`,
and call into the compiled code for evaluation. This delivers
substantially faster path-tracking at the cost of compilation time
and platform portability.

The JIT path isn't always enabled; the default evaluation walks the
SLP node-by-node.

## What `evaluate` does

For an SLP with `n` inputs and `m` outputs, calling
`evaluator.evaluate(x_1, …, x_n)`:

1. Initialises a working array with the constants and inputs.
2. Walks the SLP nodes in topological order, applying each node's
   operation (`add`, `mult`, …) to the working array.
3. Reads off the output positions.

All arithmetic dispatches through `RT`'s aring methods, which the
template specialises at compile time.

## `timing.hpp`

The header pulls in [`timing.hpp`](file-timing.md) for benchmark
timestamps — the evaluator can report total evaluation time across
many calls, useful for tuning NAG continuation step sizes.

## Used by

- [`file-NAG.md`](file-NAG.md) — primary user. Every continuation
  step calls `evaluate(...)` on the system's SLP.
- [`unit-tests/PointArray.cpp`](unit-tests/README.md) — exercises SLP
  evaluation under test conditions.

## Author note

Anton Leykin's code in this file is in the public domain.

## Related

- [`computations.md`](computations.md) — area overview.
- [`file-SLP.md`](file-SLP.md), [`file-SLP-defs.md`](file-SLP-defs.md)
  — sibling pieces of the SLP system.
- [`file-NAG.md`](file-NAG.md) — primary consumer.
- [`file-mutablemat.md`](file-mutablemat.md) — evaluator inputs are
  `MutableMat<SMat<RT>>`.
- [`file-VectorArithmetic.md`](file-VectorArithmetic.md) — ring
  arithmetic dispatcher.
