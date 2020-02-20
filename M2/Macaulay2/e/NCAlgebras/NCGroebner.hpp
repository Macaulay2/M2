#ifndef _NCGroebner_hpp_
#define _NCGroebner_hpp_

#include "../M2FreeAlgebra.hpp"
#include "WordTable.hpp"
//#include "SuffixTree.hpp"
#include "OverlapTable.hpp"
#include "NCReduction.hpp"
#include <iostream>

extern void tryOutMathicCode();

class NCGroebner : public our_new_delete
{
private:
  const FreeAlgebra& mFreeAlgebra;

  WordTable mWordTable;
  //SuffixTree mWordTable;
  OverlapTable mOverlapTable;
  const ConstPolyList mInput;
  ConstPolyList mGroebner;
  mutable std::unique_ptr<PolynomialHeap> mHeap;
  
  int mTopComputedDegree;
  int mHardDegreeLimit;

public:

  NCGroebner(const FreeAlgebra& A,
             const ConstPolyList& input,
             int hardDegreeLimit
             )
    : mFreeAlgebra(A),
      mInput(input),
      mHeap(makePolynomialHeap(HeapType::PriorityQueue,A)),
      mTopComputedDegree(-1),
      mHardDegreeLimit(hardDegreeLimit)
  {
    Word tmpWord;
    // process input polynomials
    for (auto i = 0; i < mInput.size(); ++i)
      {
        tmpWord = freeAlgebra().lead_word(*mInput[i]);
        mOverlapTable.insert(freeAlgebra().monoid().wordHeft(tmpWord),
                             true,
                             std::make_tuple(i,-1,-1));
      }
  }

  const FreeAlgebra& freeAlgebra() const { return mFreeAlgebra; }
  
  void compute(int softDegreeLimit);

  const ConstPolyList& currentValue();

  // old version of reduction code
  auto twoSidedReductionOld(const FreeAlgebra& A,
                            const Poly* reducee,
                            const ConstPolyList& reducers,
                            const WordTable& W) -> Poly*;

  // this is the old version of the SuffixTree reduction code.
  // static auto twoSidedReduction(const FreeAlgebra& A,
  //                               const Poly* reducee,
  //                               const ConstPolyList& reducers,
  //                               const SuffixTree& W) -> Poly*;

  auto twoSidedReduction(const ConstPolyList& reducees) const -> ConstPolyList;

  // this is logically const, but not actually because the heap may be changed by
  // this function.
  auto twoSidedReduction(const Poly* reducee) const -> Poly*;

  void addToGroebnerBasis(Poly* toAdd);

  void updateOverlaps(const Poly* toAdd);

  auto initReductionOnly() -> void;
  
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

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:

