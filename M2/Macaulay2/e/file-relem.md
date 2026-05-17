# `relem.{cpp,hpp}` — the `RingElement` class

`RingElement` is the engine's representation of an element of a ring,
visible to the interpreter as the opaque type behind every polynomial /
integer / rational / field-element value the user manipulates.

Part of the [Ring elements & maps](ring-elements-and-maps.md) area.

[← per-area: ring-elements-and-maps](ring-elements-and-maps.md) · [← engine overview](README.md)

## Anatomy

```cpp
class RingElement : public EngineObject {
    const Ring *R;        // which ring it belongs to
    ring_elem  val;       // ring-specific opaque value
};
```

A `RingElement` is essentially a **tagged value**: the `Ring*` pointer is the
tag, and the `ring_elem` is the value the ring knows how to interpret. All
arithmetic dispatches by going through `R`.

## Construction

`RingElement` instances are usually built by methods on `Ring`, not by direct
construction:

```cpp
RingElement *e = RingElement::make_raw(R, R->from_int(42));
```

The `make_raw` factory accepts a pre-constructed `ring_elem` (e.g. from the
ring's `from_int`, `from_rational`, `var`, etc.) and wraps it with the
`Ring*`. Construction is cheap — no copying of the underlying value.

## Operations

Arithmetic is uniformly delegated:

```cpp
RingElement *operator+(const RingElement &b) const {
    assert(R == b.R);
    return RingElement::make_raw(R, R->add(val, b.val));
}
```

This pattern lets the same `RingElement` class represent integers, rationals,
polynomials, polynomial-quotient elements, finite-field elements, Weyl-algebra
operators, and so on — the ring does the work.

## Why opaque values

The `ring_elem` is `union { int i; long l; void *p; mpz_ptr z; ... }` (see
`ringelem.hpp`). Different rings use different members. Two reasons:

1. Performance — small values (e.g. Z/p elements) live inline in the union;
   only large values (`mpz_t`, `mpq_t`, polynomial pointers) need to be
   heap-allocated.
2. Encapsulation — the ring is the only thing that knows what `val` means.

## Cross-ring operations

There is no automatic coercion in the engine layer. If you have
`RingElement` instances in different rings, you must first apply a
[`RingMap`](file-ringmap.md) or build a common parent ring at the M2 level.

## Related

- [`file-ringmap.md`](file-ringmap.md) — homomorphisms between rings.
- [`ring-elements-and-maps.md`](ring-elements-and-maps.md) — area overview.
- [`interface/ringelement.{h,cpp}`](interface/README.md) — public C interface.
- [`m2/rings.m2`](../m2/README.md) — M2-side wrappers that produce `RingElement`s.
