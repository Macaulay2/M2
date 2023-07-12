#pragma once

#include "../VectorArithmetic.hpp"
#include "MonomialHashTable.hpp"

//#if 0

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
 const MonomialHashTable& mHashTable;
 std::vector<Polynomial> mPolynomials;
public:
 PolynomialList(const VectorArithmetic& VA,
                const MonomialHashTable& monHash)
     : mVectorArithmetic(VA),
       mHashTable(monHash)
 {}
};

// not a final choice of statuses
enum class GBPolyStatus { Gen, MinGen, MinGB, NonMinGB, Retired };

// What classes do we need?
// Macaulay matrix creation

class MacaulayMatrix
{
private:
};

struct Column
{
};

struct Row
{
};

// Basis: keeping the Groebner basis (and original generators)
class Basis
{
private:
  std::vector<GBPolyStatus> mIsMinimal;
  PolynomialList mPolynomialList;

 public:
  const PolynomialList& getPolynomialList() const { return mPolynomialList; }

 public:
  Basis(const VectorArithmetic& VA,
        const MonomialHashTable& monHash) : mPolynomialList(VA,monHash) {}
  ~Basis() = default;

  // const VectorArithmetic& vectorArithmetic() const { return mVectorArithmetic; }

  // What functionality do we need here?
  // wipe out a poly from basis
  // add poly to the basis (and adjust other elements whose lead terms are div by the new poly)
  // test divisibility both ways (monomial divisibility class)

  // accessor functions
};

// MonomialView object?

}  

// append_to_basis
//void append_to_basis(newf4::Basis& B, const Matrix* M);
//auto basis_to_M2_matrix(const newf4::Basis & B, const FreeModule* F) -> Matrix*;
//auto basis_to_M2_mutable_matrix(const newf4::Basis & B, const FreeModule* F) -> MutableMatrix*;
  
//#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
