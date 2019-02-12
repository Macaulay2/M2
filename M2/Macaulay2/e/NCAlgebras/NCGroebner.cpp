#include "NCGroebner.hpp"

void NCGroebner::compute(int maxdeg)
{
  // TODO
}

const NCGroebner::ConstPolyList* NCGroebner::currentValue()
{
  // TODO
  return &mInput;
}

auto NCGroebner::twoSidedReduction(const PolynomialAlgebra* A,
                                   const Poly* reducee,
                                   const ConstPolyList& reducers,
                                   const WordTable& W) -> const Poly*
{
  return reducee;
  // TODO
}

auto NCGroebner::twoSidedReduction(const PolynomialAlgebra* A,
                                   const ConstPolyList& reducees,
                                   const ConstPolyList& reducers) -> ConstPolyList
{
  WordTable W;
  for (auto& f : reducers)
    {
      std::vector<int> lt;
      auto i = f->cbegin();
      for (auto v = i.monom().begin()+2; v !=i.monom().end(); ++v)
        lt.push_back(*v);
      W.insert(lt);
    }
  ConstPolyList result;
  for (auto i = reducees.cbegin(); i != reducees.cend(); ++i)
    result.push_back(twoSidedReduction(A, *i, reducers, W));
  return result;
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
