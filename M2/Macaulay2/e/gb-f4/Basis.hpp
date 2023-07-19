#pragma once

#include "MonomialHashTable.hpp"
#include "PolynomialList.hpp"
#include "../VectorArithmetic.hpp"

namespace newf4 {

// not a final choice of statuses
enum class GBPolyStatus { Gen, MinGen, MinGB, NonMinGB, Retired };

// Basis: keeping the Groebner basis (and original generators)
class Basis
{
private:
  std::vector<GBPolyStatus> mGBStatusList;
  PolynomialList mPolynomialList;

 public:
  const PolynomialList& getPolynomialList() const { return mPolynomialList; }

 public:
  // Basis(const VectorArithmetic& VA,
  //       const MonomialHashTable& monHash) : mPolynomialList(VA,monHash) {}
  Basis(const VectorArithmetic& VA) : mPolynomialList(VA) {}
  ~Basis() = default;

  // const VectorArithmetic& vectorArithmetic() const { return mVectorArithmetic; }

  // What functionality do we need here?
  // wipe out a poly from basis
  // add poly to the basis (and adjust other elements whose lead terms are div by the new poly)
  // test divisibility both ways (monomial divisibility class)

  // accessor functions
};

}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
