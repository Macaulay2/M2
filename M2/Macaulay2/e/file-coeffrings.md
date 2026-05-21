# `coeffrings.{cpp,hpp}` — coefficient-ring registry & `SimpleARing` examples

`coeffrings.cpp` is the engine's **coefficient-ring registry**: it
defines a handful of `CoefficientRing<X>` classes that wrap concrete
arithmetic back ends and register them with the [`aring`](file-aring.md)
framework. The class `CoefficientRingZZp` declared at the top of the
header is the smallest fully-worked example.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## `CoefficientRingZZp` — a worked example

The header declares:

```cpp
class CoefficientRingZZp : public M2::SimpleARing<CoefficientRingZZp> {
    int  p;
    int  p1;          // p - 1
    int  minus_one;
    int  zero;
    int *log_table;   // log_table[a] = discrete log of a (base primitive root)
    int *exp_table;   // exp_table[i] = (primitive root)^i mod p

    static inline int modulus_add(int a, int b, int p) {
        int t = a + b;
        // ...
    }
    // ...
};
```

`CoefficientRingZZp` represents `Z/p` for small primes (`p ≤ 32749`) by
**discrete-logarithm tables**:

- Multiplication becomes addition of logs: `a * b = exp_table[log_table[a] + log_table[b]]`.
- Squaring, inversion, powers all become arithmetic on the log indices.
- The tables fit in cache for `p` up to ~32000 — very fast.

The CRTP pattern `M2::SimpleARing<CoefficientRingZZp>` is the
template-based dispatch mechanism: the base class provides default
implementations parameterised on the derived class, so virtual calls
inline.

## Why `coeffrings.cpp` exists

The engine has many possible concrete coefficient back ends:

| Ring | Back end |
|---|---|
| Z/p (small) | `CoefficientRingZZp` (this file) |
| Z/p (FLINT) | `aring-zzp-flint` |
| Z/p (FFLAS) | `aring-zzp-ffpack` |
| ZZ (GMP) | `aring-zz-gmp` |
| ZZ (FLINT) | `aring-zz-flint` |
| QQ (GMP, FLINT) | `aring-qq-{gmp,flint}` |
| GF | `aring-{gf-flint,gf-flint-big,m2-gf}` |
| RR, CC | `aring-{RR,RRR,RRi,CC,CCC,CCi}` |

`coeffrings.cpp` is the place where these back ends register with the
[`aring`](file-aring.md) dispatcher. Each ring's tag becomes selectable
through the `aring` `RingID` enum, and `coeffrings.cpp` ensures the right
type ends up at the right tag.

## Pattern

To **add a new coefficient ring** following this style:

1. Implement the back end as a `class FooCoeffs : public M2::SimpleARing<FooCoeffs>`.
2. Add `add`, `mult`, `negate`, `invert` to the class.
3. Register it in `coeffrings.cpp` with a new `RingID` value.
4. Expose it through [`interface/aring.h`](interface/README.md).

The same pattern that produces `CoefficientRingZZp` works for any new
small-ring back end.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring.md`](file-aring.md) — registry / dispatcher.
- [`ZZp.{cpp,hpp}`](coefficient-rings.md) — older, legacy Z/p without
  the `SimpleARing` CRTP.
- [`aring-glue.hpp`](coefficient-rings.md) — bridge between
  `CoefficientRing*` types and the legacy `Ring*` API.
