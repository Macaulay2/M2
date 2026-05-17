# `geobucket.hpp`, `geopoly.hpp` — geometric bucket polynomial reduction

`geobucket.hpp` and `geopoly.hpp` implement **geometric buckets** —
the data structure for fast polynomial addition / subtraction used
throughout the Gröbner-basis engines and resolutions. The "geo" is
geometric (size-doubling buckets).

Part of the [engine](README.md) — Gröbner bases support.

[← engine overview](README.md) · [Gröbner bases](groebner-bases.md)

## The problem geobuckets solve

GB reduction does **billions** of small polynomial additions. Naive
"add a small polynomial to a big polynomial":

```
big += k * small
```

is O(|big|) per operation. After N reductions you spend O(N²)
total work just on the accumulator.

Geobuckets fix this. The accumulator is split into a tower of
sub-polynomials of geometric size:

```
heap[0]   capacity 4
heap[1]   capacity 16
heap[2]   capacity 64
heap[3]   capacity 256
heap[4]   capacity 1024
...
heap[8]   capacity 65536
```

(`GEOHEAP_SIZE` heap levels, `heap_size[i]` capacities defined in
[`file-engine-h.md`](file-engine-h.md)'s `engine.cpp`.)

To add `small` of size `s`:

1. Find the smallest level `i` with `heap[i].size + s ≤
   capacity[i]`.
2. Merge `small` into `heap[i]`.
3. If overflow, cascade up.

Each addition is amortised O(log N) instead of O(N) — a massive
win.

## The two templates

```cpp
template <class FREEMODULETYPE, class VECTYPE>
class geobucket
{
  FREEMODULETYPE *F;
  const Ring *K;
  VECTYPE heap[GEOHEAP_SIZE];
  int top_of_heap;
  ...
};
```

(`geobucket.hpp`) — generic over the freemodule type and the
vector representation.

```cpp
class polyheap
{
  const PolynomialRing *F;
  const Ring *K;
  Nterm *heap[GEOHEAP_SIZE];
  int top_of_heap;
  ...
};
```

(`geopoly.hpp`) — specialised to polynomials (as opposed to
free-module elements). Same algorithm, different element type.

## The header notes a kludge

```cpp
// This should probably be done by:
// (a) making a type FREEMODULETYPE, that FreeModule, and res_poly
//     both can inherit from: but this is a bit of a kludge...
// (b) making a vector type with a next and coeff field, that
//     is then inherited by vecterm, resterm.
```

The author thought about refactoring (a common base class) but
preferred the template approach. The template avoids virtual
dispatch in the inner loop — critical for performance.

## Where geobuckets show up

- Almost every Gröbner-basis engine
  ([`comp-gb.md`](file-comp-gb.md), [`gb-default.md`](file-gb-default.md)).
- Resolution reduction loops
  ([`res-a2.md`](file-res-a2.md)).
- Some matrix multiplication paths.

If you grep `geobucket` you'll find usage everywhere reduction
happens.

## Used by

- All Gröbner-basis computations.
- Resolution computations.
- Some matrix-vector multiplication paths.

## Related

- [`README.md`](README.md) — engine overview.
- [`groebner-bases.md`](groebner-bases.md) — primary client area.
- [`file-engine-h.md`](file-engine-h.md) — where
  `heap_size[GEOHEAP_SIZE]` is declared.
- [`f4/file-memblock.md`](f4/file-memblock.md) — sister
  memory-block primitive.
