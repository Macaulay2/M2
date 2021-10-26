// Copyright 2004.  Michael E. Stillman

#ifndef _gbweight_hpp_
#define _gbweight_hpp_

#include "ringelem.hpp" // TODO: needed for M2_arrayint
#include "newdelete.hpp"

class GBRing;
class gbvector;

class GBWeight : public our_new_delete
// A class to compute the "heuristic weight" of elements
// Mainly for use with Groebner basis computation
{
  M2_arrayint wts_;
  bool use_component_degrees_;
  const FreeModule *F_;

  int nvars_;
  GBRing *R_;

  size_t exp_size;  // length in bytes of temporary exponent vectors
  long *Fdegs_;

 public:
  GBWeight(const FreeModule *F, M2_arrayint wts);

  ~GBWeight() {}
  M2_arrayint get_weights() const { return wts_; }
  // All of these routines expect components in the range 1..N,
  // or, if the component is 0, then it is in the ring, not in the
  // free module F_

  int exponents_weight(const int *exponent_vector, int comp) const;

  int gbvector_term_weight(const gbvector *f) const;

  int gbvector_weight(const gbvector *f) const;

  int gbvector_weight(const gbvector *f, int &lead_term_weight) const;

  int monomial_weight(const int *monom, int comp) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
