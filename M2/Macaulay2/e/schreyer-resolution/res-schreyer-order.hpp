// Copyright 2016 Michael E. Stillman

#ifndef _res_schreyer_order_hpp_
#define _res_schreyer_order_hpp_

#include "schreyer-resolution/res-monomial-types.hpp"  // for component_index
#include <vector>                                      // for vector

struct ResSchreyerOrder
{
  std::vector<res_packed_monomial> mTotalMonom;
  std::vector<component_index> mTieBreaker;
  // keep a memory block for these monomials?  Probably...
};

// Operations to include:
//  . create total monomials and tiebreakers (or, just tiebreakers, given total
//  monomials?)
//  . check that a polynomial is in correct descending order w.r.t. this order
//  . sort a polynomial into this order
//  . provide a comparison operator for two monomials (not in total monomial
//  encoding)
//  . (debug) display data associated to this order

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
