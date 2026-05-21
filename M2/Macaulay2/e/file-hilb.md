# `hilb.{cpp,hpp}` — Hilbert function, series, polynomial

`hilb.cpp` implements **Bigatti et al.**'s recursive divide-and-conquer
algorithm for computing the Hilbert series of a graded module over a
polynomial ring. From the Hilbert series the engine derives the Hilbert
function and Hilbert polynomial.

Part of the [Other computations](computations.md) area.

[← per-area: computations](computations.md) · [← engine overview](README.md)

## What it computes

For a graded module `M = R^a / I`, the Hilbert series is the formal power
series

```
H_M(t) = Σ_d dim_k(M_d) · t^d
```

It is a rational function in `t`. The numerator (when the denominator is
fixed at `∏(1 - t^{deg x_i})`) is what `hilb.cpp` produces. From it:

- the **Hilbert function** = coefficients of the rational expansion,
- the **Hilbert polynomial** = the eventual polynomial behaviour for large
  `d`, extracted symbolically from the numerator.

## Algorithm (Bigatti–Caboara–Robbiano)

Roughly:

```
hilbert_numerator(I):
    if I is a monomial ideal:
        return base case (sum of t^{deg(m)} over min generators with inclusion-exclusion)
    pick a "pivot" monomial m
    split I into I:m and I + (m)
    recurse, combine
```

The split exploits the short exact sequence

```
0 → R/(I:m) ─·m→ R/I → R/(I + (m)) → 0
```

which gives `H_{R/I} = t^{deg m} · H_{R/(I:m)} + H_{R/(I+(m))}`.

The pivot heuristic (which `m` to pick) is the key to performance and is
controlled by `partition_table` declared at the top of `hilb.hpp`.

## Inputs

The engine accepts both:

- A **monomial ideal** directly — fast path; the algorithm above runs without
  detour through Gröbner bases.
- A **general ideal**, in which case the engine first computes a Gröbner
  basis and then takes the initial monomial ideal.

## API surface

The user-facing entry points (in [`interface/groebner.{h,cpp}`](interface/README.md))
return a `RingElement*` in `ZZ[t]` (the numerator) or the Hilbert polynomial as
a `RingElement*` in `QQ[d]`.

## Performance notes

The algorithm is sensitive to:

- Number of variables — exponential worst case.
- Sparsity of the input monomial ideal — fewer leading terms reduces recursion.
- Pivot choice — `partition_table` chooses pivots that balance the recursion
  tree.

For very large inputs, the alternative is to read the Betti table off a
resolution ([`file-comp-res.md`](file-comp-res.md)) and sum graded ranks.

## Related

- [`computations.md`](computations.md) — area overview.
- [`monideal.{cpp,hpp}`](file-monideal.md) — monomial-ideal back end.
- [`file-comp-res.md`](file-comp-res.md) — resolution-based alternative.
- [`interface/groebner.{h,cpp}`](interface/README.md) — public API.
