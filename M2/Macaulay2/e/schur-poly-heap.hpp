// Copyright 1996-2011 Michael E. Stillman

#ifndef _schur_poly_heap_hpp_
#define _schur_poly_heap_hpp_

/**
 * @file schur-poly-heap.hpp
 * @brief `schur_poly_heap` --- geometric-bucket accumulator specialised for `SchurRing2` polynomials.
 *
 * Declares `schur_poly_heap`, a `GEOHEAP_SIZE` (15)-level
 * size-quadrupling heap of `ring_elem` slots used by `SchurRing2`
 * multiplication to collect the many `c_{lambda mu}^nu s_nu`
 * intermediate terms produced by Littlewood-Richardson
 * enumeration. `add(p)` drops `p` into the smallest level whose
 * `heap_size[i]` threshold it fits under (the same `4, 16, 64,
 * ..., 1073741824` table from `engine.cpp`, each level four
 * times the previous) and cascades on overflow; `value()`
 * flattens the tower into a single canonical sum and resets the
 * heap. Sizing the threshold to each slot's current term count
 * amortises the LR collection away from the quadratic merge a
 * naive term-by-term accumulator would walk.
 *
 * The shape mirrors the engine's other geometric heaps
 * (`gbring.hpp`'s `gbvectorHeap`, `geovec.hpp`, `geopoly.hpp`)
 * but the slot type is the `ring_elem` of the owning
 * `SchurRing2 *S` --- `add_to` calls `S->add(a, b)` and assigns
 * `S->zero()` to the drained slot. Only `schur2.cpp` consumes
 * this header: the LR-multiplication driver allocates an
 * `SMheap` of this type, calls `add` for each output term, and
 * reads `value()` once at the end.
 *
 * @see schur2.hpp
 * @see geopoly.hpp
 * @see geovec.hpp
 */

class schur_poly_heap : public our_new_delete
{
  ring_elem heap[GEOHEAP_SIZE];
  const SchurRing2 *S;
  int top_of_heap;

  void add_to(ring_elem &a, ring_elem &b);

 public:
  schur_poly_heap(const SchurRing2 *S0);
  ~schur_poly_heap() { /* nothing */}

  void add(ring_elem p);

  ring_elem
  value();  // Returns the linearized value, and resets the schur_poly_heap.

  ring_elem debug_list(int i)
  {
    return heap[i];
  }  // DO NOT USE, except for debugging purposes!
};

inline void schur_poly_heap::add_to(ring_elem &a, ring_elem &b)
{
  ring_elem c = S->add(a, b);
  a = c;
  b = S->zero();
}

inline schur_poly_heap::schur_poly_heap(const SchurRing2 *S0)
    : S(S0), top_of_heap(-1)
{
  int i;
  for (i = 0; i < GEOHEAP_SIZE; i++) heap[i] = S->zero();
}

inline void schur_poly_heap::add(ring_elem p)
{
  size_t len = S->size(p);
  int i = 0;
  while (len >= heap_size[i]) i++;
  add_to(heap[i], p);
  len = S->size(heap[i]);
  while (len >= heap_size[i])
    {
      i++;
      add_to(heap[i], heap[i - 1]);
      len = S->size(heap[i]);
    }
  if (i > top_of_heap) top_of_heap = i;
}

inline ring_elem schur_poly_heap::value()
{
  ring_elem result = S->zero();
  for (int i = 0; i <= top_of_heap; i++)
    {
      if (S->size(heap[i]) == 0) continue;
      add_to(result, heap[i]);
    }
  top_of_heap = -1;
  return result;
}

#endif
