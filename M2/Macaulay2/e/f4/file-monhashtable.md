# `monhashtable.{cpp,hpp}` — F4's monomial hash table

`monhashtable.hpp` declares two **trait classes** used to instantiate a
generic hash table over packed monomials. The hash table itself is a
template; these traits configure it for two slightly different keying
schemes.

Part of the [`f4/`](README.md) subdirectory.

[← f4 overview](README.md) · [← engine overview](../README.md)

## Trait classes

```cpp
class MonomialsWithComponent {
public:
    typedef packed_monomial value;
    long hash_value(value m) const   { return m[0] + m[1]; }
    bool is_equal(value m, value n)  { return mMonoid.is_equal(m, n); }
    // ...
private:
    const MonomialInfo &mMonoid;
};

class MonomialsIgnoringComponent {
    // same shape, but is_equal compares the monomial only,
    // ignoring the free-module component slot
};
```

The two traits express the same data structure with different equality
semantics:

- **`MonomialsWithComponent`** — distinguishes `e_i · m` from `e_j · m`.
  Used when we are tracking monomials in a free module.
- **`MonomialsIgnoringComponent`** — treats `e_i · m` and `e_j · m` as the
  same key. Used when we just want to know if monomial `m` has appeared
  anywhere.

The hash function reads the first two ints of the packed monomial. The
first holds the encoded length / degree summary; the second holds the
first variable's exponent (or weight). Their sum is a cheap, well-spread
hash for most inputs.

## Where the hash table comes from

The trait classes are consumed by the generic `HashTable<Traits>` declared
in `mathic` (vendored as a submodule under [`submodules/`](../../../submodules/README.md)).
The actual hash-table implementation lives there; this file just supplies
the engine-specific traits.

## Cross-subdir reuse

The header also includes
[`schreyer-resolution/res-moninfo.hpp`](../schreyer-resolution/README.md)
and `res-monomial-types.hpp` — the same hash-table machinery is shared
across F4 and the resolution code. This is a deliberate factoring that
ensures both subsystems pay the same per-monomial cost.

## Related

- [`README.md`](README.md) — F4 overview.
- `MonomialInfo` in `moninfo.{cpp,hpp}` — packed-monomial layout.
- `mathic` submodule — the generic hash-table template.
- [`../schreyer-resolution/README.md`](../schreyer-resolution/README.md) —
  cross-consumer of the same traits.
