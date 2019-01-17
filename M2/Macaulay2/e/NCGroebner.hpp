#ifndef _NCGroebner_hpp_
#define _NCGroebner_hpp_

#include "PolynomialAlgebra.hpp"

class NCGroebner
{
private:
  const Matrix* mInput;
  int mTopComputedDegree;
public:
  NCGroebner(const Matrix* input)
    : mInput(input),
      mTopComputedDegree(-1)
  {
  }
  
  void compute(int maxdeg);

  const Matrix* currentValue();

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:

