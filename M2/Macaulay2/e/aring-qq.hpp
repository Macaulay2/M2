// Copyright 2013 Michael E. Stillman.

#ifndef _aring_qq_hpp_
#define _aring_qq_hpp_

#if defined(HAVE_FLINT)
#include "aring-qq-flint.hpp"
#endif
#include "aring-qq-gmp.hpp"

namespace M2 {
  class ARingQQFlint;
  class ARingQQGMP;

#if defined(HAVE_FLINT)
  typedef ARingQQGMP ARingQQ;
#else
  typedef ARingQQGMP ARingQQ;
#endif
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
