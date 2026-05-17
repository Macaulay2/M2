# `BasicPolyList.hpp`, `BasicPolyList.cpp` — ring-agnostic polynomial list

`BasicPolyList` is a **plain-data polynomial list** — a
`std::vector<BasicPoly>` — used as the **transport format** between
M2-side polynomial representations, the F4 GB engine, and serialised
file formats. It deliberately *doesn't* require a ring.

Part of the [engine](README.md) — utilities + matrices.

[← engine overview](README.md) · [matrices](matrices.md) · [polynomial rings](polynomial-rings.md)

## Header

```cpp
// BasicPolyList is a vector of polynomials (with components)
// which we can easily translate to and from other polynomial and matrix types.
// This class really doesn't require any ring.
// Current restriction: the coefficients must be an integral type. TODO: allow infinite precision integers too.
//   TODO: how should we handle coefficients which are: GF(p^n), QQ, fraction fields? or even polynomials?
#pragma once

#include "exceptions.hpp"

#include <stdexcept>
#include <string>
#include <vector>
#include <iostream>

#include "BasicPoly.hpp"
#include "PolynomialStream.hpp"

using BasicPolyList = std::vector<BasicPoly>;

long bytesUsed(const BasicPolyList& F);

class BasicPolyListStreamCollector
```

## Why ring-agnostic

The motivation is **decoupling**:

- The F4 GB engine works with its own internal types
  ([`gb-f4/`](gb-f4/README.md)).
- M2's interpreter works with `Matrix` (column-oriented free-module
  elements, ring-aware).
- File formats (msolve, raw text) use yet another representation.

Going pairwise (`F4 ↔ Matrix`, `F4 ↔ file`, `Matrix ↔ file`)
would require 6 conversion paths. With `BasicPolyList` as the
hub, you only need 3 (each format ↔ `BasicPolyList`).

The TODO comments enumerate the limits:

- Integral coefficients only (for now).
- GF, QQ, fraction fields, recursive polynomial coefficients —
  not yet.

## `BasicPolyListStreamCollector`

```cpp
class BasicPolyListStreamCollector
{
  ...
  void idealBegin(size_t polyCount) { ... }
  void appendPolynomialBegin(size_t termCount) { ... }
};
```

A **streaming collector** that builds a `BasicPolyList` from a
sequence of stream events:

```
idealBegin(2)
  appendPolynomialBegin(3)
    [3 terms emitted]
  appendPolynomialDone()
  appendPolynomialBegin(2)
    [2 terms emitted]
  appendPolynomialDone()
idealDone()
```

The streaming pattern lets large lists be built without
pre-allocating everything.

## `toMatrix`

```cpp
const Matrix* toMatrix(const FreeModule *target, const BasicPolyList& Fs)
{
  MatrixStream S(target);
  toStream(Fs, S);
  return S.value();
}
```

Converts a `BasicPolyList` into an M2 `Matrix` by streaming into
a `MatrixStream` (the matrix-building counterpart of
`BasicPolyListStreamCollector`).

## Used by

- The F4 GB engine ([`gb-f4/`](gb-f4/README.md)) — its primary
  input/output type.
- File-format readers
  ([`file-matrix-stream.md`](file-matrix-stream.md) if added).
- M2 ↔ msolve interop.
- The `BasicPolyListParser` ([`file-BasicPolyListParser.md`](file-BasicPolyListParser.md)
  if added) parses text files into this type.

## Related

- [`README.md`](README.md) — engine overview.
- [`file-BasicPoly.md`](file-BasicPoly.md) if added — single
  polynomial type.
- [`gb-f4/file-PolynomialList.md`](gb-f4/file-PolynomialList.md)
  — F4 engine's ring-aware polynomial list.
- [`polynomial-rings.md`](polynomial-rings.md), [`matrices.md`](matrices.md).
