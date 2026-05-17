# `polynom.hpp` — `Polynom<MonomType>`

`Polynom` is the **polynomial value type** in bibasis. Polynomials are
stored as a linked list of monomials (no coefficients, since the ground
field is `F_2`) sorted by the monomial order.

Part of the [`bibasis/`](README.md) subdirectory.

[← bibasis overview](README.md) · [← engine overview](../README.md)

## State

```cpp
namespace BIBasis {

template <typename MonomType>
class Polynom {
private:
    MonomType         *MonomListHead;     // linked list, highest term first
    static FastAllocator Allocator;       // pool allocator
    static MonomType   UniteMonom;        // constant "1" monomial
    // ...
};

}
```

Storage:

- **`MonomListHead`** — a singly-linked list of monomials. Each monomial
  carries a `Next` pointer; the head is the leading monomial.
- **`Allocator`** — a static `FastAllocator` ([`allocator.hpp`](README.md))
  shared by all `Polynom` instances of this `MonomType`. Polynomials are
  small and allocated in great numbers, so a custom pool dominates.
- **`UniteMonom`** — the multiplicative identity, kept around as a static
  member to avoid reconstructing it.

## Arithmetic

In a Boolean ring, polynomial arithmetic is **symmetric difference** of
monomial sets: `p + q` keeps monomials that appear in exactly one of `p`
or `q`. Multiplication walks both polynomials, multiplies monomial pairs
(taking advantage of the squarefree relation `x_i^2 = x_i` to keep
exponents in `{0, 1}`), and applies the same XOR rule.

This means there is no coefficient arithmetic at all in `Polynom` — every
term has implicit coefficient 1. The compiler turns the merge loop into a
very tight inner loop.

## Templated on `MonomType`

Like [`JanetTree`](file-janettree.md), `Polynom` is templated on the
concrete monomial class ([`file-monom.md`](file-monom.md)). Each ordering
gets its own instantiation.

## Why not reuse the engine's `Poly`

The engine's general polynomial type
([`Polynomial.{cpp,hpp}`](../polynomial-rings.md)) carries coefficients,
supports arbitrary rings, and uses the engine's monomial encoding. For the
Boolean / squarefree case, that machinery is overkill — `Polynom` strips
out everything except the symmetric-difference operation.

## Related

- [`README.md`](README.md) — bibasis overview.
- [`file-monom.md`](file-monom.md) — monomial type parameter.
- [`file-bibasis.md`](file-bibasis.md) — algorithm consumer.
- [`allocator.{cpp,hpp}`](README.md) — pool allocator.
