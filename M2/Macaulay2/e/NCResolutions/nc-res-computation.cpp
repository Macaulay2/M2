#include "NCResolutions/nc-res-computation.hpp"

ResolutionComputation* createNCRes(const Matrix* groebnerBasisMatrix,
				   int max_level,
				   int strategy)
{
  return new NCResComputation(groebnerBasisMatrix,max_level);
}
