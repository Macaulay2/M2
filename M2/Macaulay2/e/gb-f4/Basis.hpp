#include "../VectorArithmetic.hpp"
#include "MonomialHashTable.hpp"

namespace newf4 {
class GBPolynomial
{
private:  
  ElementArray mCoefficients;
  std::vector<ComponentIndex> mComponents;
  std::vector<MonomialIndex> mMonomials;
public:
  // creation (output iterator?)
  // iteration (for a const one)
  // access
};

/// Basis: keeping the Groebner basis (and original generators)
class Basis
{
private:
  const VectorArithmetic& mVectorArithmetic;
  MonomialHashTable mHashTable; // this is where all the monomials are stored.

  std::vector<GBPolynomial> mPolynomials;
public:
  Basis(const VectorArithmetic& VA) : mVectorArithmetic(VA) {}
  ~Basis() {}

  const VectorArithmetic& vectorArithmetic() const { return mVectorArithmetic; }
  // What functionality do we need here?
};

}  
/// append_to_basis
void append_to_basis(newf4::Basis& B, const Matrix* M);

auto basis_to_M2_matrix(const newf4::Basis & B, const FreeModule* F) -> Matrix*;
auto basis_to_M2_mutable_matrix(const newf4::Basis & B, const FreeModule* F) -> MutableMatrix*;
  
