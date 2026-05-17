# `gbweight.{cpp,hpp}` — `GBWeight`

`GBWeight` computes the **heuristic weight** of polynomials and monomials
during Gröbner basis computation. It is the engine's central source of the
"sugar" and degree information that drives S-pair selection.

Part of the [Gröbner bases](groebner-bases.md) area.

[← per-area: groebner-bases](groebner-bases.md) · [← engine overview](README.md)

## State

```cpp
class GBWeight : public our_new_delete {
    M2_arrayint   wts_;
    bool          use_component_degrees_;
    const FreeModule *F_;
    int           nvars_;
    GBRing       *R_;
    // ...
};
```

The weight is a function `monomial → int` constructed from:

- A **weight vector** `wts_` (one weight per variable).
- An option `use_component_degrees_` that, when true, adds the degree of
  the free-module component to the weight.
- A pointer to the **free module** the polynomials live in (used to read
  per-component degrees).
- A pointer to the **GBRing** (used to read encoded monomials).

## Why "heuristic" weight

The weight is not the actual degree of the monomial — it can be tuned to
make certain GB inputs faster. Common uses:

- **Standard degree** — `wts_` is the all-ones vector.
- **Custom weighting** — supplied by the M2-level `gb` call to bias S-pair
  selection toward particular variables.
- **Sugar** — degree of the original input polynomial *as a homogeneous
  thing*, propagated through S-pair formation.

`GBWeight` doesn't choose between these — it computes whichever was
configured at construction time.

## API surface

- `gbvector_weight(gbvector *v, int &deg_seen)` — weight of an entire
  polynomial; also reports the maximum component degree encountered.
- `monomial_weight(monomial m, int comp)` — weight of a single
  monomial/component pair.

Both are called per S-pair to decide which pairs to process first.

## Performance

The weight functions are fast: a packed multiplication of the exponent
vector by `wts_` plus a constant per-component lookup. They are called
millions of times in a typical GB run, so the fast path matters.

## Related

- [`groebner-bases.md`](groebner-bases.md) — area overview.
- [`file-gb-default.md`](file-gb-default.md) — primary consumer.
- [`file-spair.md`](file-spair.md) — uses `GBWeight` to compute S-pair
  priorities.
- [`file-gbring.md`](file-gbring.md) — supplies the `gbvector` type.
