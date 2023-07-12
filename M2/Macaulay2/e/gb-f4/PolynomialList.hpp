#pragma once

#include "MonomialHashTable.hpp"
#include "MonomialTypes.hpp"
#include "../VectorArithmetic.hpp"

namespace newf4 {

class Polynomial
{
 private:
  ElementArray mCoefficients;
  std::vector<ComponentIndex> mComponents;
  std::vector<MonomialIndex> mMonomials;

 public:
  // creation (output iterator?)
  // iteration (for a const one) (similar to NC Poly)
  // access
};

// This class will store the input to the GB commands, as well as
// any intermediate polynomials encountered along the way.
class PolynomialList
{
 private:
  const VectorArithmetic& mVectorArithmetic;
  const MonomialHashTable mHashTable;
  std::vector<Polynomial> mPolynomials;

 public:
  PolynomialList(const VectorArithmetic& VA, const MonomialHashTable& monHash)
      : mVectorArithmetic(VA), mHashTable(monHash)
  {
  }
};

} // end namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End: