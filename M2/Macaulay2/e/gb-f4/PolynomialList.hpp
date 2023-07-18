#pragma once

#include "MonomialHashTable.hpp"
#include "MonomialTypes.hpp"
#include "../VectorArithmetic.hpp"

namespace newf4 {

class Polynomial
{
  friend class PolynomialListStream;
private:
  ElementArray mCoefficients;
  std::vector<ComponentIndex> mComponents;
  std::vector<MonomialIndex> mMonomials; // each monomial is an index into a vector of polynomials.

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
  MonomialHashTable mHashTable;
  std::vector<Polynomial> mPolynomials;

 public:
  PolynomialList(const VectorArithmetic& VA, const MonomialHashTable& monHash)
      : mVectorArithmetic(VA), mHashTable(monHash)
  {
  }

  const MonomialHashTable& monomialHashTable() const { return mHashTable; }
  MonomialHashTable& monomialHashTable() { return mHashTable; }

  Polynomial& operator[](int index) { return mPolynomials[index]; }
  const Polynomial& operator[](int index) const { return mPolynomials[index]; }

  size_t size() const { return mPolynomials.size(); }
};

} // end namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
