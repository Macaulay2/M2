#include "NCResolutions/nc-res-computation.hpp"
#include "M2FreeAlgebraQuotient.hpp"

NCResComputation::NCResComputation(const FreeAlgebraQuotient& ring,
				   const Matrix& gbMatrix,
				   int max_level)
 : mRing(ring),
   mInputGroebnerBasis(gbMatrix),
   mMaxLevel(max_level)
{
}

ResolutionComputation* createNCRes(const Matrix* gbMatrix,
				   int max_level,
				   int strategy)
{
  const M2FreeAlgebraQuotient* ring = gbMatrix->get_ring()->cast_to_M2FreeAlgebraQuotient();
  if (ring != nullptr)  
    return new NCResComputation(ring->freeAlgebraQuotient(),*gbMatrix,max_level);
  ERROR("Expected a Matrix over a FreeAlgebraQuotient");
  return nullptr;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
