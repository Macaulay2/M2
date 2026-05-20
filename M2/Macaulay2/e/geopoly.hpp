// Copyright 1996  Michael E. Stillman

/**
 * @file geopoly.hpp
 * @brief `polyheap` --- polynomial-specialised geometric heap for reduction accumulators.
 *
 * Declares `polyheap`, the size-quadrupling bucket structure for
 * adding many small polynomials together as the GB reduction loop
 * does. `GEOHEAP_SIZE` (15) slots each hold a sub-polynomial of
 * bounded length (`4, 16, 64, ..., 1073741824` --- the
 * `heap_size[]` array from `engine.cpp`, each level four times
 * the previous); `add(p)` drops `p` into the smallest slot that
 * can absorb it and cascades on overflow, which is the standard
 * base-4 carry pattern. `remove_lead_term` scans the non-empty
 * slots, sums coefficients of any sharing the leading monomial,
 * and advances those contributors past the term --- lazy merging
 * that pays only to compare leading monomials. `value()`
 * flattens the whole tower into a single canonical polynomial
 * and resets the heap. The amortised complexity is logarithmic
 * per insertion instead of linear, the difference between O(N^2)
 * and O(N log N) for a typical reduction.
 *
 * Polynomial-side counterpart of `geobucket.hpp` (which is
 * templated over the freemodule / vector type); the
 * free-module-element counterpart is `geovec.hpp`.
 *
 * @see geobucket.hpp
 * @see geovec.hpp
 * @see engine.cpp
 */

// This should probably be done by:
// (a) making a type Ring, that FreeModule, and res_poly
//     both can inherit from: but this is a bit of a kludge...
// (b) making a vector type with a next and coeff field, that
//     is then inherited by vecterm, resterm.
// Redefine:
// Ring
//    routines that should be implemented in this class:
//    add_to, compare, get_ring, remove
// Nterm *
//    fields of this structure type should include:
//    next, coeff

class polyheap
{
  const PolynomialRing *F;  // Our elements will be vectors in here
  const Ring *K;            // The coefficient ring
  Nterm *heap[GEOHEAP_SIZE];
  int top_of_heap;

 public:
  polyheap(const PolynomialRing *F);
  ~polyheap();

  void add(Nterm *p);
  Nterm *remove_lead_term();  // Returns NULL if none.

  Nterm *value();  // Returns the linearized value, and resets the polyheap.

  Nterm *debug_list(int i)
  {
    return heap[i];
  }  // DO NOT USE, except for debugging purposes!
};

inline polyheap::polyheap(const PolynomialRing *FF)
    : F(FF), K(FF->getCoefficientRing()), top_of_heap(-1)
{
  // set K
  int i;
  for (i = 0; i < GEOHEAP_SIZE; i++) heap[i] = NULL;
}

inline polyheap::~polyheap()
{
  // The user of this class must insure that all 'vecterm's
  // have been removed first.  Thus, we don't need to
  // do anything here.
}

inline void polyheap::add(Nterm *p)
{
  int len = F->n_terms(p);
  int i = 0;
  while (len >= heap_size[i]) i++;

  ring_elem tmp1 = heap[i];
  ring_elem tmp2 = p;
  F->add_to(tmp1, tmp2);
  heap[i] = tmp1;

  len = F->n_terms(heap[i]);
  p = NULL;
  while (len >= heap_size[i])
    {
      i++;

      tmp1 = heap[i];
      tmp2 = heap[i - 1];
      F->add_to(tmp1, tmp2);
      heap[i] = tmp1;

      len = F->n_terms(heap[i]);
      heap[i - 1] = NULL;
    }
  if (i > top_of_heap) top_of_heap = i;
}

inline Nterm *polyheap::remove_lead_term()
{
  int lead_so_far = -1;
  for (int i = 0; i <= top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      if (lead_so_far < 0)
        {
          lead_so_far = i;
          continue;
        }
      int cmp = EQ;  // F->compare(heap[lead_so_far], heap[i]);
      if (cmp == GT) continue;
      if (cmp == LT)
        {
          lead_so_far = i;
          continue;
        }
      // At this point we have equality
      K->add_to(heap[lead_so_far]->coeff, heap[i]->coeff);
      Nterm *tmp = heap[i];
      heap[i] = tmp->next;
      tmp->next = NULL;
      F->remove(reinterpret_cast<ring_elem &>(tmp));

      if (K->is_zero(heap[lead_so_far]->coeff))
        {
          // Remove, and start over
          tmp = heap[lead_so_far];
          heap[lead_so_far] = tmp->next;
          tmp->next = NULL;
          F->remove(reinterpret_cast<ring_elem &>(tmp));
          lead_so_far = -1;
          i = -1;
        }
    }
  if (lead_so_far < 0) return NULL;
  Nterm *result = heap[lead_so_far];
  heap[lead_so_far] = result->next;
  result->next = NULL;
  return result;
}

inline Nterm *polyheap::value()
{
  Nterm *result = NULL;
  for (int i = 0; i <= top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      ring_elem tmp1 = result;
      ring_elem tmp2 = heap[i];
      F->add_to(tmp1, tmp2);
      result = tmp1;
      heap[i] = NULL;
    }
  top_of_heap = -1;
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
