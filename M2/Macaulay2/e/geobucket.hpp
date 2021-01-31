// Copyright 1996  Michael E. Stillman

// This should probably be done by:
// (a) making a type FREEMODULETYPE, that FreeModule, and res_poly
//     both can inherit from: but this is a bit of a kludge...
// (b) making a vector type with a next and coeff field, that
//     is then inherited by vecterm, resterm.
// Redefine:
// FREEMODULETYPE
//    routines that should be implemented in this class:
//    add_to, compare, get_ring, remove
// VECTYPE
//    fields of this structure type should include:
//    next, coeff

// GEOHEAP_SIZE: defined in style.hpp
// heap_size: defined in object.cpp

template <class FREEMODULETYPE, class VECTYPE>
class geobucket
{
  FREEMODULETYPE *F;		// Our elements will be vectors in here
  const Ring *K;		// The coefficient ring
  VECTYPE heap[GEOHEAP_SIZE];
  int top_of_heap;
  int mLead;			// set after a call to get_lead_term.
				// set negative after each call to add, 
				// or remove_lead_term
public:
  geobucket(FREEMODULETYPE *F);
  ~geobucket();

  void add(VECTYPE p);
  const VECTYPE get_lead_term(); // Returns NULL if none.
  VECTYPE remove_lead_term();	// Returns NULL if none.

  FREEMODULETYPE *get_target() const { return F; }
  VECTYPE value();		// Returns the linearized value, and resets the geobucket.

  VECTYPE debug_list(int i) { return heap[i]; } // DO NOT USE, except for debugging purposes!
  VECTYPE current_value() const; // Adds up all the elements and returns this value
				 // Mainly used for debugging.
};

template <class FREEMODULETYPE, class VECTYPE>
inline geobucket<FREEMODULETYPE, VECTYPE>::geobucket(FREEMODULETYPE *FF)
: F(FF),
  K(FF->get_ring()->getCoefficientRing()),
  top_of_heap(-1),
  mLead(-1)
{
  // set K
  int i;
  for (i=0; i<GEOHEAP_SIZE; i++)
    heap[i] = NULL;
}

template <class FREEMODULETYPE, class VECTYPE>
inline geobucket<FREEMODULETYPE, VECTYPE>::~geobucket()
{
  // The user of this class must insure that all 'vecterm's
  // have been removed first.  Thus, we don't need to
  // do anything here.
}

template <class FREEMODULETYPE, class VECTYPE>
inline void geobucket<FREEMODULETYPE, VECTYPE>::add(VECTYPE p)
{
  mLead = -1;
  int len = F->n_terms(p);
  int i= 0;
  while (len >= heap_size[i]) i++;
  F->add_to(heap[i], p);
  len = F->n_terms(heap[i]);
  p = NULL;
  while (len >= heap_size[i])
    {
      i++;
      F->add_to(heap[i], heap[i-1]);
      len = F->n_terms(heap[i]);
      heap[i-1] = NULL;
    }
  if (i > top_of_heap)
    top_of_heap = i;
}

template <class FREEMODULETYPE, class VECTYPE>
inline const VECTYPE geobucket<FREEMODULETYPE, VECTYPE>::get_lead_term()
{
  int lead_so_far = -1;
  for (int i=0; i <= top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      if (lead_so_far < 0) 
	{
	  lead_so_far = i;
	  continue;
	}
      int cmp = F->compare(heap[lead_so_far], heap[i]);
      if (cmp == GT) continue;
      if (cmp == LT)
	{
	  lead_so_far = i;
	  continue;
	}
      // At this point we have equality
      K->add_to(heap[lead_so_far]->coeff, heap[i]->coeff);
      VECTYPE tmp = heap[i];
      heap[i] = tmp->next;
      tmp->next = NULL;
      F->remove(tmp);

      if (K->is_zero(heap[lead_so_far]->coeff))
	{
	  // Remove, and start over
	  tmp = heap[lead_so_far];
	  heap[lead_so_far] = tmp->next;
	  tmp->next = NULL;
	  F->remove(tmp);
	  lead_so_far = -1;
	  i = -1;
	}
    }
  mLead = lead_so_far;
  if (lead_so_far < 0) return NULL;
  VECTYPE result = heap[lead_so_far];
  return result;
}

template <class FREEMODULETYPE, class VECTYPE>
inline VECTYPE geobucket<FREEMODULETYPE, VECTYPE>::remove_lead_term()
{
  if (mLead < 0) get_lead_term();
  if (mLead < 0) return NULL;
  VECTYPE result = heap[mLead];
  heap[mLead] = result->next;
  result->next = NULL;
  mLead = -1;
  return result;
}

template <class FREEMODULETYPE, class VECTYPE>
inline VECTYPE geobucket<FREEMODULETYPE, VECTYPE>::value()
{
  VECTYPE result = NULL;
  for (int i=0; i<=top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      F->add_to(result, heap[i]);
      heap[i] = NULL;
    }
  top_of_heap = -1;
  return result;
}

template <class FREEMODULETYPE, class VECTYPE>
inline VECTYPE geobucket<FREEMODULETYPE, VECTYPE>::current_value() const
{
  VECTYPE result = NULL;
  for (int i=0; i<=top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      VECTYPE tmp = F->copy(heap[i]);
      F->add_to(result, tmp);
    }
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
