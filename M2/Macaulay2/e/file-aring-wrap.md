# `aring-wrap.{cpp,hpp}` — `RElementWrap<RingType>` (typed ring-element wrapper)

`aring-wrap.hpp` declares **`RElementWrap<RingType>`** — a templated
wrapper that holds a single `aring` value (typed by `RingType`) and
exposes it through the abstract `RElement` interface. It is the
type-safe complement of [`file-aring-glue.md`](file-aring-glue.md)
(which wraps a whole *ring*); this file wraps a single *element*.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "aring.hpp"

namespace M2 {

template <class RingType>
class RElementWrap : public RElement {
    friend bool ARing::converter(const ARing       *sourceR,
                                 const ARing       *targetR,
                                 const RElement    &a,
                                 RElement          &b);
    // ... stores a value of type `typename RingType::elem` ...
};

}
```

A `RElementWrap<RingType>` carries one value of the templated
`RingType::elem` type. The friend declaration grants the converter
visibility into the wrapped value so cross-ring coercion via
[`file-aring-translate.md`](file-aring-translate.md) can extract /
inject the typed value.

## Why an element wrapper

The `aring` framework dispatches arithmetic by ring type at compile
time. But values that cross the engine boundary (or get stored in a
`Matrix` / `Polynomial`) need a *runtime* abstract base — they can
be of any aring type, and the caller doesn't always know which.

`RElement` is that abstract base; `RElementWrap<RingType>` is the
concrete derivation. A value flowing across the boundary lives as
`RElement*` (or by-value `RElement`); inside the engine it gets
dispatched to the right `RElementWrap<R>` via runtime type
information.

## Pattern parallel to `ConcreteRing<R>`

`aring-wrap.hpp` and [`file-aring-glue.md`](file-aring-glue.md)
together implement a **wrap-templated-implementation-in-an-abstract-base**
pattern at two scales:

| Scale | Templated implementation | Abstract wrapper | Bridges from-to |
|---|---|---|---|
| Ring | `RingType` | `ConcreteRing<RingType>` | aring ↔ legacy `Ring` |
| Element | `RingType::elem` | `RElementWrap<RingType>` | aring value ↔ generic `RElement` |

The two files are read together when learning the aring framework's
type-erasure conventions.

## Used by

- Cross-ring coercion via [`file-aring-translate.md`](file-aring-translate.md).
- `RElement`-typed return values from cross-ring operations.
- Some test code in [`unit-tests/`](unit-tests/README.md) that
  needs to manipulate aring values without specialising on the ring
  type at the call site.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring.md`](file-aring.md) — `aring` framework + `RElement`
  declaration.
- [`file-aring-glue.md`](file-aring-glue.md) — sibling
  `ConcreteRing<R>` (ring-level wrapper).
- [`file-aring-translate.md`](file-aring-translate.md) — cross-ring
  coercion that uses `RElementWrap`.
