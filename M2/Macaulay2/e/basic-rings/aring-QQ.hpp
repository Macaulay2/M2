// Copyright 2013 Michael E. Stillman.

#ifndef M2_BASIC_RINGS_ARING_QQ_HPP_
#define M2_BASIC_RINGS_ARING_QQ_HPP_

#include "basic-rings/aring-QQ-flint.hpp"
#include "basic-rings/aring-QQ-gmp.hpp"

namespace M2 {
class ARingQQFlint;
class ARingQQGMP;

typedef ARingQQGMP ARingQQ;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
