#ifndef _NCGroebner_hpp_
#define _NCGroebner_hpp_

#include "engine-includes.hpp"

#include <algorithm>
#include <iostream>
#include <memory>
#include <tuple>
#include <utility>
#include <vector>

#include "NCAlgebras/FreeAlgebra.hpp"
#include "NCAlgebras/NCReduction.hpp" // for getHeapType
#include "OverlapTable.hpp"
#include "Polynomial.hpp"
#include "Word.hpp"
#include "WordTable.hpp"
#include "text-io.hpp"

extern void tryOutMathicCode();

class NCGroebner : public our_new_delete
{
private:
  const FreeAlgebra& mFreeAlgebra;

  WordTable mWordTable;
  //SuffixTree mWordTable;
  OverlapTable mOverlapTable;

  const ConstPolyList mInput;
  std::vector<int> mGeneratorDegrees; // heft degree or sugar degree of corresponding mGroebner element.

  ConstPolyList mGroebner;
  std::vector<int> mGroebnerDegrees; // sugar degree.  -1 means removed.

  mutable std::unique_ptr<PolynomialHeap> mHeap;

  bool mIsGraded;
  int mTopComputedDegree;
  int mHardDegreeLimit;

public:

  NCGroebner(const FreeAlgebra& A,
             const ConstPolyList& input,
             int hardDegreeLimit,
             int strategy
             )
    : mFreeAlgebra(A),
      mInput(input),
      mHeap(makePolynomialHeap(getHeapType(strategy), A)),
      mTopComputedDegree(-1),
      mHardDegreeLimit(hardDegreeLimit)
  {
    if (M2_gbTrace >= 1)
      {
        buffer o;
        o << "[NCGB] reduction heap:  "
          << getHeapName(getHeapType(strategy))
          << newline;
        emit_line(o.str());
      }
    Word tmpWord;
    // process input polynomials
    mIsGraded = true;
    for (auto i = 0; i < mInput.size(); ++i)
      {
        auto d = freeAlgebra().heft_degree(*mInput[i]);
        mGeneratorDegrees.push_back(d.first);
        if (not d.second)
          mIsGraded = false;
        tmpWord = freeAlgebra().lead_word(*mInput[i]);
        mOverlapTable.insert(d.first, // previously: freeAlgebra().monoid().wordHeft(tmpWord),
                             true,
                             std::make_tuple(i,-1,-1));
      }
    if (M2_gbTrace >= 1)
      {
        buffer o;
        o << "[NCGB] input is " << (mIsGraded ? "homogeneous" : "inhomogeneous") << newline;
        emit_line(o.str());
      }
  }

  const FreeAlgebra& freeAlgebra() const { return mFreeAlgebra; }
  
  void compute(int softDegreeLimit);

  // This function uses sugar degree, and heuristics to deal with non-minimally
  // constructed Groebner basis elements
  void computeInhomogeneous(int softDegreeLimit);
  void computeHomogeneous(int softDegreeLimit);
  
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

