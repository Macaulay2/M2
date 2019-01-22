#ifndef _NCGroebner_hpp_
#define _NCGroebner_hpp_

#include "PolynomialAlgebra.hpp"

class KnuthMorrisPratt; // defined in NCGroebner.cpp

class NCGroebner
{
public:
  using Poly = PolynomialAlgebra::Poly;
  using PolyList = std::vector<Poly*>;
  using ConstPolyList = std::vector<const Poly*>;

  NCGroebner(const PolynomialAlgebra* A, const ConstPolyList& input)
    : mRing(A),
      mInput(input),
      mTopComputedDegree(-1)
  {
  }
  
  void compute(int maxdeg);

  const ConstPolyList* currentValue();

  static ConstPolyList twoSidedReduction(const PolynomialAlgebra* A,
                                         const ConstPolyList& reducees,
                                         const ConstPolyList& reducers);
private:
  static auto twoSidedReduction(const PolynomialAlgebra* A,
                                const Poly* reducee,
                                const ConstPolyList& reducers,
                                KnuthMorrisPratt& table
                                ) -> Poly*;

private:
  const PolynomialAlgebra* mRing;
  const ConstPolyList mInput;
  int mTopComputedDegree;
  
  // KMP algorithm for subwords

  // given a pair of monomials, find all the overlaps. (suffix of first = prefix of second, order matters)

  // reduction algorithm

  // structure to keep track of which pairs we have to do.

  // during reduction: structure to keep track of what reductions we have done.
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:

