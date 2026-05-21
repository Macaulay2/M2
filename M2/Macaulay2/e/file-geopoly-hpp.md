# `geopoly.hpp` — geometric-heap polynomial accumulator

Defines `class polyheap`, a **geometric-heap** data structure for
adding many polynomials together efficiently. The classic use case is
the reduction step in a Gröbner-basis algorithm: you start with one
polynomial and add many smaller multiples of basis elements to it,
producing a long sum that needs to be combined into a single canonical
polynomial.

## The geometric-heap idea

If you naively merge each new polynomial into a running sum, you do
`O(N²)` work because each merge touches every term in the
ever-growing sum. The geometric heap fixes this by keeping
`GEOHEAP_SIZE` (15) bucket slots, each holding a polynomial of a
bounded length:

| Slot | Max term count |
|---|---|
| 0 | 4 |
| 1 | 16 |
| 2 | 64 |
| 3 | 256 |
| … | … |
| 14 | 67 108 864 |

(Bucket capacities from `heap_size[]` in
[`engine.cpp`](file-engine-cpp.md) — a quadrupling sequence.)

When you `add(p)`: drop `p` into the smallest bucket that can hold it,
merge with what's there; if the bucket overflows, **cascade** it into
the next-larger bucket. Like incrementing a base-4 number with carry.

When you `remove_lead_term()`: scan all non-empty buckets, find the
buckets sharing the lead monomial, sum their coefficients, advance
each contributing bucket past that term. **Lazy merging** — you only
pay to compare lead monomials, not to combine all the polynomials.

`value()` flattens everything into a single linearized polynomial and
resets the heap.

## Interface (≈10 lines public)

```cpp
class polyheap
{
  const PolynomialRing *F;
  const Ring           *K;          // coefficient ring
  Nterm                *heap[GEOHEAP_SIZE];
  int                   top_of_heap;
public:
  polyheap(const PolynomialRing *F);
  ~polyheap();
  void   add(Nterm *p);
  Nterm *remove_lead_term();        // NULL when empty
  Nterm *value();                   // linearize + reset
};
```

Note: `polyheap` works in terms of **`Nterm *`** (the underlying
linked-list polynomial representation), not `ring_elem`. It's a
low-level tool; users at the `Ring` level call `F->add_to(...)`
repeatedly instead.

## Sister structures

| Heap | Element type | Defined in |
|---|---|---|
| `polyheap` | `Nterm *` (commutative polynomial) | this file |
| `vecheap` | `vecterm *` (vector over a polynomial ring) | [`geovec.hpp`](file-geovec.md) |
| `geobucket<VECTYPE>` | template over any element type | [`geobucket.hpp`](file-geobucket.md) |
| GB-ring heap | `gbvector *` | inline in [`gbring.{hpp,cpp}`](file-gbring.md) |
| Schur-ring heap | `ring_elem` (Schur polynomial) | [`schur-poly-heap.hpp`](file-schur-poly-heap.md) |

`geobucket.hpp` is the modern templated version and is preferred for
new code. `geopoly.hpp` survives because the classical commutative
polynomial paths still use it.

## Consumers

`poly.cpp` and `polyring.cpp` use `polyheap` to implement
expensive-polynomial-arithmetic helpers (e.g. `mult_by_term` over many
terms, `power` via repeated squaring).

## Where it fits

This file is part of the **primitive-data-structure layer** of the
engine — same tier as
[`buffer.hpp`](file-buffer.md), [`hash.hpp`](file-hash.md), and
[`mem.hpp`](file-mem.md). It's not tied to any specific ring type;
the ring interface it uses is just `add_to`, `n_terms`, `remove`,
plus the underlying `Nterm` layout.

## See also

- [`file-engine-cpp.md`](file-engine-cpp.md) — defines `heap_size[]`
- [`file-style.md`](file-style.md) — defines `GEOHEAP_SIZE = 15`
- [`file-geobucket.md`](file-geobucket.md) — modern templated heap
- [`file-geovec.md`](file-geovec.md) — vector heap
- [`groebner-bases.md`](groebner-bases.md) — where geometric heaps get heavy use
