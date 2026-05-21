# `ringmap.{cpp,hpp}` — the `RingMap` class

`RingMap` is the engine's representation of a homomorphism `R → S` between two
rings. The map is specified by giving images for each generator of `R` (as
elements of `S`).

Part of the [Ring elements & maps](ring-elements-and-maps.md) area.

[← per-area: ring-elements-and-maps](ring-elements-and-maps.md) · [← engine overview](README.md)

## State

```cpp
class RingMap : public EngineObject {
    const Ring *R;      // source
    const Ring *S;      // target
    M2_arrayint immediate_images;   // images of variables
    bool is_monomial_;              // optimisation flag
    // ...
};
```

The fields:

- **`R`** — source ring (where the map applies).
- **`S`** — target ring (where images live).
- **Images** — one `RingElement`-equivalent of `S` per generator of `R`.
- **`is_monomial_`** — set when every image is a single monomial of `S`;
  unlocks a much faster evaluation path.

## Evaluation

Applying a `RingMap` to a polynomial in `R` walks each term, substitutes the
image of each variable, and accumulates in `S`. The algorithm is:

```
for each term c · x_1^{e_1} … x_n^{e_n} of input:
    image_term = c   (or σ(c) if S has a different coefficient ring)
    for i in 1..n:
        image_term *= image[i] ^ e[i]
    result += image_term
return result
```

For maps where coefficients live in fields the engine knows aren't preserved
(e.g. Z/p → Z), it routes the coefficient through a `RingMap` on the
coefficient ring as well.

## Composition

There is no first-class `compose(f, g)` in the engine — composition is done
by evaluating `f` on each image of `g`, producing a new image list. This is
intentional: ring maps are usually short-lived and built ad hoc.

## Where ring maps come from

- `R → R/I` quotient projection (built by `qring.cpp`).
- Substitution maps from M2 code: `map(S, R, {y, z, x})`.
- Ring extension morphisms.
- Coercions when the user writes `1_R` in a context expecting `S`.

## Related

- [`file-relem.md`](file-relem.md) — the value type a ring map manipulates.
- [`ring-elements-and-maps.md`](ring-elements-and-maps.md) — area overview.
- [`interface/ringmap.{h,cpp}`](interface/README.md) — public C interface.
- [`m2/ringmap.m2`](../m2/README.md) — M2-side wrappers and operators.
