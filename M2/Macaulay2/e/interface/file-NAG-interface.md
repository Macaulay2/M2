# `NAG.h` (in `interface/`) — public C entry points for Numerical Algebraic Geometry

`interface/NAG.h` declares the **public C functions** the interpreter
uses for Numerical Algebraic Geometry — homotopy continuation, witness
sets, monodromy, and the rest of the NAG toolkit implemented in
[`../file-NAG.md`](../file-NAG.md).

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#if defined(__cplusplus)
class M2SLEvaluator;
class M2Homotopy;
class M2SLProgram;
class StraightLineProgram;
class PathTracker;
class M2PointArray;
#else
typedef struct M2SLEvaluator        M2SLEvaluator;
typedef struct M2Homotopy           M2Homotopy;
typedef struct M2SLProgram          M2SLProgram;
typedef struct StraightLineProgram  StraightLineProgram;
typedef struct PathTracker          PathTracker;
typedef struct M2PointArray         M2PointArray;
#endif
```

Six opaque types are exposed:

- **`M2SLProgram`** — the engine-side wrapper around a `StraightLineProgram`
  ([`../file-SLP.md`](../file-SLP.md)).
- **`M2SLEvaluator`** — evaluates a `M2SLProgram` at a point.
- **`M2Homotopy`** — packages a continuation system together with start
  and target equations.
- **`PathTracker`** — owns the continuation algorithm state and step-size
  control.
- **`M2PointArray`** — a collection of numerical points (witness sets, sample
  outputs).
- **`StraightLineProgram`** — the underlying SLP class.

## Entry points

The functions cover the full NAG workflow:

- **SLP construction** — `rawSLP(...)`, `rawSLPCompose(...)`.
- **Evaluator construction** — `rawSLEvaluator(slp, …)`.
- **Evaluation** — `rawSLEvaluatorEvaluate(eval, x)`.
- **Homotopy construction** — `rawHomotopy(start, target, …)`.
- **Path tracking** — `rawPathTracker(...)`, `rawPathTrackerSolve(...)`.
- **Point array operations** — `rawPointArray(...)`, `rawPointArrayLookup(...)`.

User-facing M2 packages: `NumericalAlgebraicGeometry`,
`MonodromySolver`, and friends.

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-NAG.md`](../file-NAG.md) — engine implementation.
- [`../file-SLP.md`](../file-SLP.md) — SLP evaluator.
- [`../computations.md`](../computations.md) — area overview.
- [`../unit-tests/PointArray.cpp`](../unit-tests/README.md) — tests.
- `NumericalAlgebraicGeometry` and `MonodromySolver` M2 packages.
