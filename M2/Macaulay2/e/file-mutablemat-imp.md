# `mutablemat-imp.hpp` — `MutableMat<Mat>` template implementations

`mutablemat-imp.hpp` contains **template implementations** of
`MutableMat<Mat>` methods that are too long to live inline in
`mutablemat.hpp`. Primarily the **straight-line program (SLP)
evaluator** glue for numerical algebraic geometry.

Part of the [engine](README.md) — matrices + numerical AG.

[← engine overview](README.md) · [matrices](matrices.md)

## Header

```cpp
// Copyright 2015 Anton Leykin and Mike Stillman

// Anton Leykin's code in this file is in the public domain.

#ifndef _mutable_mat_imp_hpp_
#define _mutable_mat_imp_hpp_

template <typename Mat>
M2SLEvaluator* MutableMat<Mat>::createSLEvaluator(M2SLProgram* P,
                                                M2_arrayint constsPos,
                                                M2_arrayint varsPos) const
{
  if (n_rows() != 1 || n_cols() != constsPos->len) {
    ERROR("1-row matrix expected; or numbers of constants don't match");
    return nullptr;
```

The `createSLEvaluator` factory bridges from a `MutableMatrix`
(holding constant values for a numerical homotopy) to an
`SLEvaluator` — the workhorse object that evaluates a
straight-line program for a homotopy step.

Anton Leykin (NAG package author) contributed the SLP / Bertini-
style numerical paths.

## Why a separate `-imp.hpp`

C++ requires template definitions to be visible at the point of
instantiation. `MutableMat<DMat<ARingRR>>::createSLEvaluator` is
needed wherever someone creates an SLP — but the body is long
and pulls in NAG headers.

Splitting:

- `mutablemat.hpp` declares the template class.
- `mutablemat-imp.hpp` defines long template methods.

This keeps `mutablemat.hpp` lean (everyone includes it) while
`mutablemat-imp.hpp` is only pulled in where SLP factory methods
are needed.

## What else lives here

Beyond `createSLEvaluator`:

- **SLP-flavored methods** — `createSLProgram`, `evaluate`.
- **NumericalAG-specific helpers** that the M2 NAG package needs.
- Any other `MutableMat<Mat>` method long enough to want a
  separate `-imp` location.

## Used by

- `MutableMat<DMat<ARingRR>>`, `MutableMat<DMat<ARingCCC>>`,
  ... whenever NAG / SLP is enabled.
- M2's `NumericalAlgebraicGeometry` user package.
- [`file-NAG.md`](file-NAG.md) if added — Numerical AG engine.

## Related

- [`README.md`](README.md) — engine overview.
- [`file-mat.md`](file-mat.md) — `MutableMatrix` base.
- [`file-NAG.md`](file-NAG.md) if added — NAG engine.
- [`matrices.md`](matrices.md) — area.
