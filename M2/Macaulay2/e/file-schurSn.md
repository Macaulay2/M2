# `schurSn.{cpp,hpp}` — `SchurSnRing` (symmetric-group representation ring)

`schurSn.cpp` implements **`SchurSnRing`** — the engine's Schur ring
specialised for the **representation theory of the symmetric group
`S_n`**. Characters of irreducible `S_n` representations are indexed
by partitions of `n`; their product (in the sense of the
representation ring) is the **Kronecker product**.

Subclasses [`file-schur2.md`](file-schur2.md)'s `SchurRing2`. Part of
the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "schur2.hpp"

class SchurSnRing : public SchurRing2 {
public:
    SchurSnRing(const Ring *A, int n = -1);

    static SchurSnRing *create(const Ring *A, int n = -1);

    virtual const SchurSnRing *cast_to_SchurSnRing() const { return this; }
    virtual       SchurSnRing *cast_to_SchurSnRing()       { return this; }

    virtual ring_elem mult(const ring_elem f, const ring_elem g) const;
              ring_elem tensor_mult(const ring_elem f, const ring_elem g) const;
};
```

Two `cast_to_SchurSnRing` overrides let code dispatch on
"is this a Schur ring of `S_n` characters?" without `dynamic_cast`.

## `mult` vs `tensor_mult`

The class overrides multiplication and exposes a separate
`tensor_mult`:

- **`mult`** (inherited from `SchurRing2`) — Schur-function product
  (the standard ring product on the Schur ring).
- **`tensor_mult`** (this class) — **Kronecker tensor product** of
  characters, the actual operation that representation theorists
  call multiplication in the representation ring of `S_n`.

The two operations agree under some interpretations but differ in
general; carrying both lets users pick.

## `n = -1` default

The constructor takes an optional `n` (default `-1` = unspecified).
With `n = -1`, the ring is the **graded direct sum** `⊕_n R(S_n)` —
characters of all symmetric groups combined. With `n > 0`, only the
characters of `S_n` are represented.

## Used by

- Representation-theory packages that compute irreducible character
  tables.
- `Posets` and combinatorial packages that operate on the
  character ring.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-schur2.md`](file-schur2.md) — base class.
- [`file-schur.md`](file-schur.md) — older Schur ring.
- [`file-schur-poly-heap.md`](file-schur-poly-heap.md) —
  accumulator used by both `SchurRing2` and `SchurSnRing`.
