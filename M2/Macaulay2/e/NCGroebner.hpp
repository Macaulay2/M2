#ifndef _NCGroebner_hpp_
#define _NCGroebner_hpp_

#include "PolynomialAlgebra.hpp"

class NCGroebner
{
private:
  using Poly = PolynomialAlgebra::Poly;
  using PolyList = std::vector<Poly*>;
  using ConstPolyList = std::vector<const Poly*>;

  const PolynomialAlgebra* mRing;
  const ConstPolyList mInput;
  int mTopComputedDegree;
public:
  NCGroebner(const ConstPolyList& input)
    : mInput(input),
      mTopComputedDegree(-1)
  {
  }
  
  void compute(int maxdeg);

  const ConstPolyList* currentValue();
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:

