// Copyright 2004.  Michael E. Stillman

#ifndef _gbweight_hpp_
#define _gbweight_hpp_

#include "engine.h"
class GBRing;
class gbvector;

class GBWeight
// A class to compute the "heuristic weight" of elements
// Mainly for use with Groebner basis computation
{
  M2_arrayint wts_;
  bool use_component_degrees_;
  const FreeModule *F_;

  int nvars_;
  GBRing *R_;

  int *EXP_;
public:
  GBWeight(const FreeModule *F, M2_arrayint wts);

  ~GBWeight() {}

  int exponents_weight(const int * exponent_vector) const;

  int gbvector_term_weight(const gbvector *f) const;

  int gbvector_weight(const gbvector *f) const;

  int monomial_weight(const int *monom, int comp) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
