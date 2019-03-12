#ifndef _free_algebra_hpp_
#define _free_algebra_hpp_

#include "../polyring.hpp"
#include "../Polynomial.hpp"
#include "FreeMonoid.hpp"

struct CoefficientRingType
{
  typedef ring_elem ElementType;
};

using Poly = Polynomial<CoefficientRingType>;

class FreeAlgebra
{
private:
  FreeAlgebra(const Ring* K, const FreeMonoid* M);

public:
  static FreeAlgebra* create(const Ring* K,
                                   const std::vector<std::string>& names,
                                   const PolynomialRing* degreeRing,
                                   const std::vector<int>& degrees
                                   );

  const Ring* getCoefficientRing() const { return &mCoefficientRing; }
  const FreeMonoid& monoid() const { return mMonoid; }
  const Monoid& degreeMonoid() const { return monoid().degreeMonoid(); }

#if 0
  
  
#endif

private:
  const Ring& mCoefficientRing;
  const FreeMonoid mMonoid;

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

