// Copyright 2005  Michael E. Stillman

#ifndef _dmat_LU_hpp_
#define _dmat_LU_hpp_

#include "engine.h"
#include "dmat.hpp"

template <typename CoeffRing>
class DMatOps
{
  typedef typename CoeffRing::elem elem;
public:
  static M2_arrayint LU(DMat<CoeffRing> *A); // modifies A
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
