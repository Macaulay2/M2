# `schur-poly-heap.{cpp,hpp}` — `schur_poly_heap` (Schur-polynomial accumulator)

`schur-poly-heap.hpp` declares **`schur_poly_heap`** — a geometric
heap specialised for accumulating Schur-polynomial sums during LR-style
multiplications in [`file-schur2.md`](file-schur2.md)'s `SchurRing2`.

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
class schur_poly_heap : public our_new_delete {
    ring_elem           heap[GEOHEAP_SIZE];
    const SchurRing2  *S;
    int                 top_of_heap;

    void add_to(ring_elem &a, ring_elem &b);

public:
    schur_poly_heap(const SchurRing2 *S0);
    ~schur_poly_heap() { /* nothing */ }

    void add(ring_elem p);

    ring_elem
    // ... extract result ...
};
```

The pattern matches the engine's other geometric heaps
([`file-gbring.md`](file-gbring.md)'s `gbvectorHeap`,
[`file-geovec.md`](file-geovec.md)'s `vec` heap) — same idea, tuned
to the Schur-polynomial value type.

## Why a heap for Schur enumeration

Multiplying two Schur polynomials via Littlewood-Richardson generates
many intermediate `c_{λμ}^ν · s_ν` terms — one per LR tableau. If
the sum is built term-by-term with naïve merging, the cost is
quadratic in the number of terms.

A geometric heap with `GEOHEAP_SIZE = 15` levels keeps the cost at
`O(n log k)` for `k` distinct output partitions, even as `n` grows
large.

## `GEOHEAP_SIZE`

The heap depth is the engine-wide constant from
[`file-style.md`](file-style.md). Each of the 15 levels holds a
geometrically growing pool of terms before promotion to the next level.

## Used by

- [`file-schur2.md`](file-schur2.md)'s `mult` — primary user.
- [`file-schurSn.md`](file-schurSn.md)'s `mult` and `tensor_mult`.
- Schubert calculus enumeration paths.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-schur2.md`](file-schur2.md), [`file-schurSn.md`](file-schurSn.md)
  — primary consumers.
- [`file-gbring.md`](file-gbring.md)'s `gbvectorHeap`,
  [`file-geovec.md`](file-geovec.md) — sibling geometric heaps.
- [`file-style.md`](file-style.md) — `GEOHEAP_SIZE` constant.
