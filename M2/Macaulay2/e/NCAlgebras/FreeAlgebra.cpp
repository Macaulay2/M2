#include "FreeAlgebra.hpp"

FreeAlgebra* FreeAlgebra::create(const Ring* K,
				 const std::vector<std::string>& names,
				 const PolynomialRing* degreeRing,
				 const std::vector<int>& degrees
				 )
{
  assert(K != nullptr);
  FreeMonoid *M = new FreeMonoid(names, degreeRing, degrees);
  FreeAlgebra* result = new FreeAlgebra(K, M);

  return result;
}

FreeAlgebra::FreeAlgebra(const Ring* K,
                         const FreeMonoid* M
                         )
  : mCoefficientRing(*K),
    mMonoid(*M)
{
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
