#define mpfr foo
#include "mpreal.h"
#undef mpfr

#include "eigen.hpp"

#include "mpfr.h"


bool SVD(const LMatrixRRR *A,
         LMatrixRRR *Sigma,
         LMatrixRRR *U,
         LMatrixRRR *VT)
{
  return false;
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
