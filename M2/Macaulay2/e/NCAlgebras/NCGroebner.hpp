#ifndef _NCGroebner_hpp_
#define _NCGroebner_hpp_

#include "../M2FreeAlgebra.hpp"
#include "WordTable.hpp"
#include "SuffixTree.hpp"
#include "OverlapTable.hpp"
#include <iostream>

extern void tryOutMathicCode();

class NCGroebner : public our_new_delete
{
private:
  const M2FreeAlgebra& mM2FreeAlgebra;

  WordTable mWordTable;
  //SuffixTree mWordTable;
  OverlapTable mOverlapTable;
  const ConstPolyList mInput;
  ConstPolyList mGroebner;
  
  int mTopComputedDegree;
  int mHardDegreeLimit;

public:

  NCGroebner(const M2FreeAlgebra& A,
             const ConstPolyList& input,
             int hardDegreeLimit)
    : mM2FreeAlgebra(A),
      mInput(input),
      mTopComputedDegree(-1),
      mHardDegreeLimit(hardDegreeLimit)
  {
    tryOutMathicCode();
    Word tmpWord;
    // process input polynomials
    for (auto i = 0; i < mInput.size(); ++i)
      {
        freeAlgebra().lead_word(tmpWord,*mInput[i]);
        mOverlapTable.insert(freeAlgebra().monoid().wordHeft(tmpWord),
                             true,
                             std::make_tuple(i,-1,-1));
      }
  
  }

  const M2FreeAlgebra& m2FreeAlgebra() const { return mM2FreeAlgebra; }
  const FreeAlgebra& freeAlgebra() const { return m2FreeAlgebra().freeAlgebra(); }
  
  void compute(int softDegreeLimit);

  const ConstPolyList& currentValue();

  static auto twoSidedReduction(const FreeAlgebra& A,
                                const ConstPolyList& reducees,
                                const ConstPolyList& reducers) -> ConstPolyList;

  static auto twoSidedReduction(const FreeAlgebra& A,
                                const Poly* reducee,
                                const ConstPolyList& reducers,
                                const WordTable& W) -> Poly*;

  // static auto twoSidedReduction(const FreeAlgebra& A,
  //                               const Poly* reducee,
  //                               const ConstPolyList& reducers,
  //                               const SuffixTree& W) -> Poly*;

  auto twoSidedReduction(const Poly* reducee) const -> Poly*;
  
  static auto createOverlapPoly(const FreeAlgebra& A,
                                const ConstPolyList& polyList,
                                int polyIndex1,
                                int polyIndex2,
                                int overlapIndex) -> Poly*;
                          
  auto createOverlapPoly(Overlap o) const -> Poly*;
  auto createOverlapLeadWord(Poly& wordAsPoly, Overlap o) const -> void;
  
  auto overlapWordLength(Overlap o) const -> int;
  auto overlapHeft(Overlap o) const -> int;
  
  auto printOverlapData(std::ostream& o, Overlap overlap) const -> void;
  
  auto insertNewOverlaps(std::vector<Overlap>& newOverlaps) -> void;

  auto isOverlapNecessary(Overlap o) const -> bool;
};

#endif

// Notes: 5 March 2019:
// working on naive remainder function.
// might need to refactor monomials, ConstMonomial-->Word.
//   also make a PolynomialALgebra class that is slim, current PolynomialALgebra
//   will be a wrapper on that.
  
//   Notes from 19 Feb 2019.
  
//   For polynomial reduction:

//   Use PolyWithPos as an Entry
//   Use mathic Geobucket code (and/or Heap, TourTree)
//     Use code/ideas from ReducerPack from mathicgb. (will need to crib from this code).
//     Probably: a hash table for all monomials/words in out polynomials.
//     SO: PolyWithPos: array of [ring_elem coeff, pointer to a monomial in the hash table structure, iterator in]

//     PolyWithPos structure
//     add(f, g: PolyWithPos): PolyWithPos;
//     multByTerm(m:Term, f : PolyWithPos, n:Term) : PolyWithPos;
//     remainder(f:PolyWithPos, G:GroebnerList) : PolyWithPos;

//     Poly: will contain the integers defining its monomials
//     PolyHashed: will contain pointers to monomials.
//       PolyHashedWithPos
//     MonomialPool: actual monomials are stored here.
//       add in a new monomial.
//       clear all the monomials.
//       need: hash function for words.
//     MonomialPool: hashtable of monomials
//     Poly: {coeffs: CoefficientArray, monoms: array of pointers}
//     PolyIter{Poly}
//     PolyWithPos: {PolyIter}

#if 0
  class Reductor
  {
  public:
    Reductor(Poly*); // also needs the ring information.

    void subtract(ConstMonomial& left, Poly* gbElement, ConstMonomial& right, Coefficient c);

    // returns true if leadMonom is set, returns false if entire poly represented is zero.
    bool leadMonomial(ConstMonomial& leadMonom); 
  };

  class PolynomialConstructor
  {
  public:
    PolynomialConstructor(); // input should be a ring.
    appendTerm(Coefficient c, ConstMonomial& m);
    appendMonomial(ConstMonomial& m);
    Poly* value();
  };
  // during reduction: structure to keep track of what reductions we have done.

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:

