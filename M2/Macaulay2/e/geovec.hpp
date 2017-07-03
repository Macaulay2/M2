// Copyright 1996  Michael E. Stillman

// This should probably be done by:
// (a) making a type const FreeModule, that FreeModule, and res_poly
//     both can inherit from: but this is a bit of a kludge...
// (b) making a vector type with a next and coeff field, that
//     is then inherited by vecterm, resterm.
// Redefine:
// const FreeModule
//    routines that should be implemented in this class:
//    add_to, compare, get_ring, remove
// vecterm *
//    fields of this structure type should include:
//    next, coeff

// GEOHEAP_SIZE: defined in style.hpp
// heap_size: defined in object.cpp

class vecHeap
{
  const FreeModule *F;  // Our elements will be vectors in here
  const Ring *K;        // The coefficient ring
  vecterm *heap[GEOHEAP_SIZE];
  int top_of_heap;
  int mLead;  // set after a call to get_lead_term.
              // set negative after each call to add,
              // or remove_lead_term
 public:
  vecHeap(const FreeModule *F);
  ~vecHeap();

  void add(vecterm *p);
  const vecterm *get_lead_term();  // Returns NULL if none.
  vecterm *remove_lead_term();     // Returns NULL if none.

  const FreeModule *get_target() const { return F; }
  vecterm *value();  // Returns the linearized value, and resets the vecHeap.

  vecterm *debug_list(int i)
  {
    return heap[i];
  }  // DO NOT USE, except for debugging purposes!
  vecterm *current_value()
      const;  // Adds up all the elements and returns this value
              // Mainly used for debugging.
};

inline vecHeap::vecHeap(const FreeModule *FF)
    : F(FF), K(FF->get_ring()), top_of_heap(-1), mLead(-1)
{
  // set K
  int i;
  for (i = 0; i < GEOHEAP_SIZE; i++) heap[i] = NULL;
}

inline vecHeap::~vecHeap()
{
  // The user of this class must insure that all 'vecterm's
  // have been removed first.  Thus, we don't need to
  // do anything here.
}

inline void vecHeap::add(vecterm *p)
{
  mLead = -1;
  int len = K->n_nonzero_terms(p);
  int i = 0;
  while (len >= heap_size[i]) i++;
  K->add_vec_to(heap[i], p);
  len = K->n_nonzero_terms(heap[i]);
  p = NULL;
  while (len >= heap_size[i])
    {
      i++;
      K->add_vec_to(heap[i], heap[i - 1]);
      len = K->n_nonzero_terms(heap[i]);
      heap[i - 1] = NULL;
    }
  if (i > top_of_heap) top_of_heap = i;
}

static int compare(const vecterm *t, const vecterm *s)
{
  int cmp = t->comp - s->comp;
  if (cmp < 0) return LT;
  if (cmp > 0) return GT;
  return EQ;
}

inline const vecterm *vecHeap::get_lead_term()
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
      int cmp = compare(heap[lead_so_far], heap[i]);
      if (cmp == GT) continue;
      if (cmp == LT)
        {
          lead_so_far = i;
          continue;
        }
      // At this point we have equality
      K->add_to(heap[lead_so_far]->coeff, heap[i]->coeff);
      vecterm *tmp = heap[i];
      heap[i] = tmp->next;
      tmp->next = NULL;
      K->remove_vec(tmp);

      if (K->is_zero(heap[lead_so_far]->coeff))
        {
          // Remove, and start over
          tmp = heap[lead_so_far];
          heap[lead_so_far] = tmp->next;
          tmp->next = NULL;
          K->remove_vec(tmp);
          lead_so_far = -1;
          i = -1;
        }
    }
  mLead = lead_so_far;
  if (lead_so_far < 0) return NULL;
  vecterm *result = heap[lead_so_far];
  return result;
}
inline vecterm *vecHeap::remove_lead_term()
{
  if (mLead < 0) get_lead_term();
  if (mLead < 0) return NULL;
  vecterm *result = heap[mLead];
  heap[mLead] = result->next;
  result->next = NULL;
  mLead = -1;
  return result;
}

inline vecterm *vecHeap::value()
{
  vecterm *result = NULL;
  for (int i = 0; i <= top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      K->add_vec_to(result, heap[i]);
      heap[i] = NULL;
    }
  top_of_heap = -1;
  return result;
}
inline vecterm *vecHeap::current_value() const
{
  vecterm *result = NULL;
  for (int i = 0; i <= top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      vecterm *tmp = K->copy_vec(heap[i]);
      K->add_vec_to(result, tmp);
    }
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
