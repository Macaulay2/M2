# `factory.{h,cpp}` (in `interface/`) — public C entry points for polynomial GCD / factorisation

`interface/factory.h` declares the **public C functions** the interpreter
uses for polynomial **GCD computation** and **factorisation over fields**.
The implementation routes to the [Factory](https://www.singular.uni-kl.de/dox/html/factory.html)
library plus [MPSolve](https://mpsolve.dm.unipi.it/) for univariate
complex root finding.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#if defined(__cplusplus)
class Matrix;
class RingElement;
#else
typedef struct Matrix      Matrix;
typedef struct RingElement RingElement;
#endif

#if defined(__cplusplus)
extern "C" {
#endif
// ... raw GCD / factor functions ...
#if defined(__cplusplus)
}
#endif
```

The header pulls in only `Matrix` and `RingElement` — polynomial GCD /
factorisation operates on these types.

## Entry points

- **GCD** — `rawGCD(f, g)`, `rawExtendedGCD(f, g)` (returns `(d, a, b)`
  with `d = af + bg`).
- **Factorisation** — `rawFactor(f)`, `rawIrreducibleFactors(f)`.
- **Root finding** — `rawRoots(f, prec, …)` — numerical roots via
  MPSolve, with arbitrary precision.

Inputs and outputs are `RingElement*` values; the engine handles type
agreement (e.g. factoring requires the coefficient ring to be a field
Factory understands: `QQ`, `Z/p`, `GF`, `RR`, `CC`).

## External libraries

- **Factory** — Singular's polynomial factorisation library. Vendored;
  built via the `factory/` directory under
  [`libraries/`](../../../libraries/README.md). Detected by CMake's
  `FindFactory.cmake`.
- **MPSolve** — arbitrary-precision complex root finder. Vendored,
  detected by `FindMPSolve.cmake`.

If either library is absent, the corresponding entry points return
`null` and M2 reports the missing feature.

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-aring.md`](../file-aring.md) — coefficient rings on which
  factorisation operates.
- [`../file-polyring.md`](../file-polyring.md) — polynomial-ring context.
- [`file-flint-interface.md`](file-flint-interface.md) — sibling for ZZ
  number-theory functions.
- Factory / MPSolve under [`../../../libraries/`](../../../libraries/README.md).
