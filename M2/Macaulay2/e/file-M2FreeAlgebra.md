# `M2FreeAlgebra.{cpp,hpp}` — M2-facing `Ring` wrapper over `FreeAlgebra`

`M2FreeAlgebra.cpp` is the **`Ring`-shaped wrapper** that lets a
non-commutative [`FreeAlgebra`](NCAlgebras/file-FreeAlgebra.md) appear
to the rest of the engine as just another `Ring`. The non-commutative
implementation lives in [`NCAlgebras/`](NCAlgebras/README.md); this
file is the M2-side façade.

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include <M2/math-include.h>
#include "engine-includes.hpp"

#include <memory>
#include <string>
#include <vector>

#include "NCAlgebras/FreeAlgebra.hpp"
#include "NCAlgebras/FreeMonoid.hpp"
#include "Polynomial.hpp"
#include "ring.hpp"
#include "ringelem.hpp"
```

The wrapper depends on:

- **`NCAlgebras/FreeAlgebra.hpp`** — the wrapped non-commutative ring.
- **`NCAlgebras/FreeMonoid.hpp`** — the underlying word monoid.
- **`Polynomial.hpp`** ([`file-Polynomial.md`](file-Polynomial.md)) — the
  shared modern polynomial value type.
- **`ring.hpp`** — the legacy `Ring` API this class inherits from.

## Why the split

The non-commutative class `FreeAlgebra` does **not** inherit from
`Ring`. Two design goals are in tension:

1. The non-commutative code wants to be independent — clean templates,
   no virtual-dispatch overhead, no concern about the engine's older
   ring API.
2. The rest of the engine (matrices, modules, resolutions,
   `Computation`s) wants a `Ring*`.

`M2FreeAlgebra` resolves the tension: it owns a `FreeAlgebra` instance
and forwards every `Ring` virtual call to the wrapped class.

## Class shape

```cpp
class M2FreeAlgebra : public Ring {
    std::unique_ptr<FreeAlgebra> mFreeAlgebra;
    // ... Ring virtuals forwarding to mFreeAlgebra ...
};
```

The pattern mirrors [`file-aring-glue.md`](file-aring-glue.md)'s
`ConcreteRing<R>` — wrap a templated implementation in a `Ring`
subclass to satisfy the rest of the engine.

## What you get over `FreeAlgebra` alone

- The class can be passed wherever a `Ring*` is expected:
  [`Matrix`](file-matrix.md) source / target, [`MutableMatrix`](file-mutablemat.md)
  entries, [`RingElement`](file-relem.md) container.
- The non-commutative ring can participate in
  [`RingMap`](file-ringmap.md) construction.
- It can be the coefficient ring of a "polynomial in two layers"
  construction.

## Companion class

[`file-Polynomial.md`](file-Polynomial.md)'s `Poly` type is the value
type that `M2FreeAlgebra` stores inside `ring_elem`s. When you have a
`RingElement *e` in an `M2FreeAlgebra`, `e->get_value()` returns a
`Poly` (boxed).

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`NCAlgebras/file-FreeAlgebra.md`](NCAlgebras/file-FreeAlgebra.md) — wrapped
  implementation.
- [`NCAlgebras/file-FreeAlgebraQuotient.md`](NCAlgebras/file-FreeAlgebraQuotient.md)
  — quotient counterpart.
- `M2FreeAlgebraQuotient.{cpp,hpp}` — the matching `Ring` wrapper for
  the quotient case.
- [`file-aring-glue.md`](file-aring-glue.md) — same wrap-templated-in-a-`Ring`
  pattern for `aring`s.
