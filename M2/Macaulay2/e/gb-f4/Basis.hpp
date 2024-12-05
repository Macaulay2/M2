#pragma once

#include "MonomialHashTable.hpp"
#include "PolynomialList.hpp"
#include "../VectorArithmetic.hpp"

namespace newf4 {

// not a final choice of statuses
enum class GBPolyStatus { Gen, MinGen, MinGB, NonMinGB, Retired };

inline std::string toString(GBPolyStatus status) {
   switch (status) {
      case GBPolyStatus::Gen: return "Gen";
      case GBPolyStatus::MinGen: return "MinGen";
      case GBPolyStatus::MinGB: return "MinGB";
      case GBPolyStatus::NonMinGB: return "NonMinGB";
      case GBPolyStatus::Retired: return "Retired";
   };
   return "Undefined";
}

// Basis: keeping the Groebner basis (and original generators)
class Basis
{
private:
  std::vector<GBPolyStatus> mGBStatusList;
  PolynomialList mPolynomialList;

public:
  Basis(const VectorArithmetic& VA,
        MonomialHashTable& monHashTable)
    : mPolynomialList(VA, monHashTable)
  {
  }

  ~Basis() = default;

  const PolynomialList& getPolynomialList() const { return mPolynomialList; }
  const VectorArithmetic& vectorArithmetic() const { return mPolynomialList.vectorArithmetic(); }
  const std::vector<GBPolyStatus>& getGBStatusList() const { return mGBStatusList; }

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
