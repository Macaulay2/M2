# `launcher.hpp`, `launcher.cpp` — BIBasis dispatcher entry point

`launcher.{hpp,cpp}` is the **dispatcher** that the rest of the
engine calls into to run a BIBasis (Boolean Involutive Basis)
computation. Picks the right monomial-order specialisation
(`MonomDL`, `MonomDRL`, `MonomLex`) based on user input.

Part of [`bibasis/`](README.md) — Mikhail V. Zinin's BIBasis
engine.

[← bibasis/ overview](README.md) · [← engine overview](../README.md)

## Header

```cpp
/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#include "involutive.hpp"
#include "matrix.hpp"
```

Author and license unchanged from the upstream BIBasis sources
(Zinin's free-as-in-GPL distribution). The launcher is what M2's
engine boundary code calls to enter BIBasis.

## `launcher.cpp` — order dispatch

```cpp
#include "launcher.hpp"
#include "involutive.hpp"
#include "monom.hpp"
#include "monomDL.hpp"
#include "monomDRL.hpp"
#include "monomLex.hpp"
```

Three monomial-order specialisations are included; the launcher
dispatches between them based on the user's chosen ordering:

| User chose | `launcher` instantiates |
|---|---|
| `Lex` | `Involutive<MonomLex>` |
| `DegLex` (Degree Lex) | `Involutive<MonomDL>` |
| `DegRevLex` (Degree Reverse Lex) | `Involutive<MonomDRL>` |

The actual GB algorithm is templated on the monomial type so the
inner loops can inline order-comparison without virtual dispatch.

## Entry point

The single function exposed to the rest of the engine
(approximate signature):

```cpp
const Matrix* bibasisRun(const Matrix* input,
                         int order,
                         int maxDegree);
```

- `input` — generators of the boolean ideal.
- `order` — which monomial order to use.
- `maxDegree` — degree cutoff (-1 for unlimited).

Returns the matrix of computed involutive basis generators.

## Why a dedicated launcher

Two reasons:

1. **Template instantiation** — the templated
   `Involutive<MonomXXX>` class would otherwise need to be
   instantiated wherever it's called. Centralising in `launcher.cpp`
   means just one instantiation per order.
2. **Engine boundary** — keeps the rest of the engine free of
   BIBasis-specific template noise; the rest just sees `bibasisRun`.

## Used by

- The engine via [`file-bibasis.md`](file-bibasis.md) — the
  `BIBasis` driver class wraps `launcher` for the legacy
  `Computation` API.
- The interpreter, indirectly through
  [`interface/`](../interface/README.md).

## Related

- [`README.md`](README.md) — bibasis/ overview.
- [`file-bibasis.md`](file-bibasis.md) — engine-facing driver.
- [`file-involutive.md`](file-involutive.md) — the templated
  algorithm being instantiated.
- [`file-monom.md`](file-monom.md), [`file-monom-orders.md`](file-monom-orders.md)
  — monomial types.
