# `NAG.{cpp,hpp}` — Numerical Algebraic Geometry

`NAG.cpp` is the engine entry point for **Numerical Algebraic Geometry** —
homotopy continuation, witness sets, monodromy, and the other techniques used
to numerically describe the zero set of a polynomial system. Originally
contributed by Anton Leykin; large portions of the code are in the public
domain (see the copyright notice).

Part of the [Other computations](computations.md) area.

[← per-area: computations](computations.md) · [← engine overview](README.md)

## What it does

NAG complements symbolic GB / resolution computation by working over `RR` or
`CC`. Given a system `F : C^n → C^m`, the engine can:

- Track the **solutions of `F = 0`** as the system deforms continuously from
  a known start system (`predictor-corrector` continuation).
- Compute a **witness set**: a representative collection of points on each
  component of the variety, tagged by dimension.
- Run **monodromy** loops to discover all permutations of a witness set under
  small homotopies — used to detect irreducible components.
- Evaluate polynomial systems efficiently at many points via
  **straight-line programs** ([`file-SLP.md`](file-SLP.md)).

## Numerical workspace

The header pulls in a wide range of C++ utilities:

```cpp
#include <algorithm>
#include <map>
#include <math.h>
#include <vector>
// ...
```

This unusual freedom (most engine files avoid STL containers in favour of
`gc_vector`) is because NAG operates on numerical types whose representation
is fixed by FLINT/MPFR/Arb, not on garbage-collected M2 values.

## Connection to SLPs

The evaluator for a polynomial system in NAG is a **straight-line program**:
a DAG of arithmetic operations on intermediate values. Given a system, NAG
compiles it once to an SLP and then evaluates the SLP at every continuation
step. SLP machinery lives in [`SLP.{cpp,hpp}`](file-SLP.md),
`SLP-defs.hpp`, and `SLP-imp.hpp`.

## TODO

The directory has a `TODO-numerics` file with outstanding work in this area.
The major themes: cleaner separation between symbolic and numerical code,
better path-tracker heuristics, and arbitrary-precision continuation.

## Related

- [`computations.md`](computations.md) — area overview.
- [`file-SLP.md`](file-SLP.md) — straight-line-program evaluator.
- [`coefficient-rings.md`](coefficient-rings.md) — `RR` and `CC` rings NAG
  operates over.
- [`unit-tests/PointArray.cpp`](unit-tests/README.md) — tests for the NAG
  point-array data structure.
- [`interface/NAG.h`](interface/README.md) — public C interface.
