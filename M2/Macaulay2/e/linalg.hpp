// Copyright 2005-2012  Michael E. Stillman

#ifndef _linalg_hpp_
#define _linalg_hpp_

#include "error.h"

template <typename MT>
struct LinAlg
{
  static bool solve(const MT &A, const MT&b, MT& X)
  {
    ERROR("...");
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

