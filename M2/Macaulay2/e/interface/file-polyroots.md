# `polyroots.cpp` — univariate polynomial root finder via MPSolve

`polyroots.cpp` is the **engine boundary function `rawRoots`** —
finds numerical roots of a univariate polynomial via MPSolve.
This is the only file in `interface/` with no matching header
(it's pure implementation; the public declaration lives in
`engine.h`).

Part of the [`interface/`](README.md) C boundary layer.

[← interface/ overview](README.md) · [← engine overview](../README.md)

## Header

```cpp
#include "interface/factory.h"
#include "interface/ring.h"

// mps uses the c++ keyword register, which is no longer allowed in the C++17
// standard, so we disable it by defining an empty macro.
#define register
#include <mps/mps.h>
#undef register
#include <stdlib.h>

#include "aring-CCC.hpp"
#include "aring.hpp"
#include "error.h"
#include "monoid.hpp"
#include "polyring.hpp"
#include "relem.hpp"
#include "ring.hpp"
#include "ringelem.hpp"

#define abs(x) (((x) < 0) ? -(x) : (x))
#define max(a, b) (((a) > (b)) ? (a) : (b))
```

The `#define register` trick at the top is a portability hack:
**MPSolve uses the C `register` keyword** in its headers, but
`register` was removed from C++17 (deprecated since C++11). The
`#define register` makes the keyword evaluate to nothing during
MPSolve's header parse; `#undef register` immediately after
restores normal C++ semantics.

## What `rawRoots` does

```cpp
engine_RawRingElementArrayOrNull rawRoots(const RingElement *p,
                                          long prec,
                                          int unique)
```

Takes:

- **`p`** — a univariate polynomial (M2 verifies univariate
  before calling).
- **`prec`** — desired precision in bits (e.g., 200 for
  high-precision).
- **`unique`** — if non-zero, return only one of each
  multiple root (the `(void) unique;` line shows this is
  currently ignored — future feature).

Returns: an array of `RingElement *` over `CCC` (complex with
specified precision), one per root.

## Why MPSolve

Univariate root-finding has several mature libraries:

- **MPSolve** — Bini's robust algorithm, arbitrary precision,
  certified accuracy.
- **GSL `polynomial_solve`** — only double precision.
- **NumPy `roots`** — double precision, M2 doesn't want to
  embed Python.

MPSolve wins on precision + reliability. It can find roots of a
polynomial of degree thousands to thousands of bits.

## Output format

The function builds a `CCC` ring at the requested precision and
allocates a `RingElement` per root:

```cpp
typedef M2::ConcreteRing<M2::ARingCCC> RingCCC;
RingCCC *CC = RingCCC::create(prec);
ring_elem c = CC->from_cc(re, im);
```

(approximate). Each root becomes a `RingElement` over `CCC`.

## Failure modes

Returns null on:

- Non-univariate input.
- Non-numerical coefficient ring.
- MPSolve internal failure (rare; logs to stderr).

## Used by

- M2's `roots` user function.
- The `NumericalAlgebraicGeometry` package's univariate paths.

## Related

- [`README.md`](README.md) — interface/ overview.
- [`../file-aring-CCC.md`](../file-aring-CCC.md) — output ring.
- [`file-factory-interface.md`](file-factory-interface.md) —
  sister polynomial-operation interface.
- MPSolve — external linked library.
