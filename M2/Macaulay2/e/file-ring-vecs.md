# `ring-vecs.cpp` — `Ring`'s vector-of-coefficients operations

`ring-vecs.cpp` defines the **`Ring` class's vector operations** —
how a ring's elements assemble into and operate within `vec`s (the
engine's sparse column-of-coefficients value type used to build
[`Matrix`](file-matrix.md) columns and [`FreeModule`](file-freemod.md)
elements).

Part of the [Ring elements & maps](ring-elements-and-maps.md) area.

[← per-area: ring-elements-and-maps](ring-elements-and-maps.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include "ring.hpp"
#include "text-io.hpp"
#include <vector>
#include "matrix.hpp"
#include "geovec.hpp"
#include "ringmap.hpp"
#include "poly.hpp"

//  Notes: ring_elem's are treated as immutable objects: they are not changed,
//  and the fact that one cannot change is used throughout.

vec Ring::new_vec() const { return new vecterm; }
void Ring::remove_vec_node(vec n) const { ... }
```

The header comment captures a fundamental invariant: **`ring_elem`
values are immutable** throughout the engine. Operations always
produce new values; nothing is mutated in place. The `vec` machinery
relies on this — it can share `ring_elem` references freely without
defensive copies.

## What's defined here

`Ring`'s virtual base class declares many `vec`-shaped operations but
defines few of them; the per-subclass implementations live in this
file. The operations:

- **Construction** — `new_vec()`, `make_vec(component, value)`,
  `make_vec_from_array(...)`.
- **Traversal** — `vec_n_terms`, `vec_lead_term`, `vec_lead_coeff`,
  `vec_lead_component`.
- **Arithmetic** — `vec_add`, `vec_subtract`, `vec_negate`,
  `vec_scalar_mult`, `vec_mult_by_term`.
- **Comparison** — `vec_compare`.
- **Conversion** — `vec_to_matrix` (single-column matrix),
  `vec_to_string` (textual output).

Each operation is virtual; subclasses (`PolyRing`, `PolyQuotient`,
`FractionField`, …) override or use the defaults.

## `vec` representation

A `vec` is a singly-linked list of `vecterm`s:

```cpp
struct vecterm {
    vecterm  *next;     // next term (descending component)
    int       comp;     // free-module component index
    ring_elem coeff;    // coefficient
};
```

Sorted in **descending order by component** so the leading term is at
the head. `ring-vecs.cpp` provides `new_vec()` (single-term ctor) and
`remove_vec_node` (one-step list manipulator) as the primitive
operations the rest of the engine builds on.

## Why `vecterm` is at the `Ring` level

The `vec` type is not specialised per ring — the same `vecterm`
linked-list shape carries elements over any ring. The ring's
contribution is the *coefficient operations*: how to add, multiply,
or compare `ring_elem`s. Centralising the vector primitives here lets
the same `vec` walk work over `ZZ`, `QQ`, `Z/p`, `R[x_1, …, x_n]`,
quotients, etc.

## Used by

- [`file-matrix.md`](file-matrix.md) — `Matrix` columns are `vec`s.
- [`file-freemod.md`](file-freemod.md) — `FreeModule` elements are
  `vec`s.
- [`file-comp-gb.md`](file-comp-gb.md) — GB output is converted from
  `gbvector` to `vec` for return to the interpreter.

## Related

- [`ring-elements-and-maps.md`](ring-elements-and-maps.md) — area overview.
- [`file-ringelem.md`](file-ringelem.md) — `ring_elem` value type.
- [`file-matrix.md`](file-matrix.md), [`file-freemod.md`](file-freemod.md)
  — primary consumers.
- `geovec.hpp` — the "geometric heap" used for fast `vec` accumulation.
