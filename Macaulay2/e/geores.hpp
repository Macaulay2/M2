// Copyright 1996  Michael E. Stillman

// This should probably be done by:
// (a) making a type res2_poly, that FreeModule, and res_poly
//     both can inherit from: but this is a bit of a kludge...
// (b) making a vector type with a next and coeff field, that
//     is then inherited by vecterm, resterm.
// Redefine:
// res2_poly
//    routines that should be implemented in this class:
//    add_to, compare, Ring_of, remove
// res2term *
//    fields of this structure type should include:
//    next, coeff

const int GEOHEAP_SIZE = 15;

class respolyHeap
{
  const res2_poly *F;		// Our elements will be vectors in here
  const Ring *K;		// The coefficient ring
  res2term * heap[GEOHEAP_SIZE];
  int top_of_heap;

public:
  respolyHeap(const res2_poly *F);
  ~respolyHeap();

  void add(res2term * p);
  res2term * remove_lead_term();	// Returns NULL if none.

  res2term * value();		// Returns the linearized value, and resets the respolyHeap.

  res2term * debug_list(int i) { return heap[i]; } // DO NOT USE, except for debugging purposes!
};

static int heap_size[GEOHEAP_SIZE] = {4, 16, 64, 256, 1024, 4096, 
				    16384, 65536, 262144, 1048576, 4194304,
				    16777216, 67108864, 268435456,
				    1073741824};

inline respolyHeap::respolyHeap(const res2_poly *FF)
: F(FF),
  K(FF->Ring_of()->Ncoeffs()),
  top_of_heap(-1)
{
  // set K
  int i;
  for (i=0; i<GEOHEAP_SIZE; i++)
    heap[i] = NULL;
}

inline respolyHeap::~respolyHeap()
{
  // The user of this class must insure that all 'vecterm's
  // have been removed first.  Thus, we don't need to
  // do anything here.
}

inline void respolyHeap::add(res2term * p)
{
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

inline res2term * respolyHeap::remove_lead_term()
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
      res2term * tmp = heap[i];
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
  if (lead_so_far < 0) return NULL;
  res2term * result = heap[lead_so_far];
  heap[lead_so_far] = result->next;
  result->next = NULL;
  return result;
}

inline res2term * respolyHeap::value()
{
  res2term * result = NULL;
  for (int i=0; i<=top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      F->add_to(result, heap[i]);
      heap[i] = NULL;
    }
  top_of_heap = -1;
  return result;
}
