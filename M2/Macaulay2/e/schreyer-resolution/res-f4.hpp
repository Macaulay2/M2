// Copyright 2014-2016 Michael E. Stillman

#ifndef _res_f4_hpp_
#define _res_f4_hpp_

#include "f4/monhashtable.hpp"
#include "schreyer-resolution/res-memblock.hpp"
#include "schreyer-resolution/res-poly-ring.hpp"
#include "monomial-sets.hpp"

#include <cassert>

class SchreyerFrame;

/////////////////////////////////////////////////////////////////////////////

class F4Res
{
  friend class ResColumnsSorter;

 public:
  F4Res(SchreyerFrame& res);

  ~F4Res();

  SchreyerFrame& frame() { return mFrame; }
  const SchreyerFrame& frame() const { return mFrame; }
  // Constructs the elements of the GB at level 'lev', in the given degree.
  // The following must have been done:
  //    construct(lev, degree-1)
  // and
  //    construct(lev-1, degree-1)
  // NOTE: it is not needed to have done: construct(lev-1,degree)
  void construct(int lev, int degree);

  const ResGausser& resGausser() const { return mRing.resGausser(); }
  const ResMonoid& monoid() const { return mRing.monoid(); }
  const ResPolyRing& ring() const { return mRing; }
 private:
  struct Row
  {
    res_packed_monomial
        mLeadTerm;  // monomial (level lev-1) giving rise to this row
    // The following two should have the same length.
    std::vector<ComponentIndex> mComponents;  // indices into mColumns
    CoefficientVector mCoeffs;
    Row() : mLeadTerm(nullptr) {}
  };

  ////////////////////////////////////
  // Functions for construction //////
  ////////////////////////////////////
  void resetMatrix(int lev,
                   int degree);  // remember to clearMatrix before calling this.
  void clearMatrix();
  bool findDivisor(res_const_packed_monomial m, res_packed_monomial result);
  ComponentIndex processCurrentMonomial(res_packed_monomial thisMonom);  // subroutine of processMonomialProduct
  ComponentIndex processMonomialProduct(res_const_packed_monomial m,
                                        res_const_packed_monomial n,
                                        int& result_sign_if_skew);
  // if result_sign_if_skew is set to 0, then result is set to -1.
  void loadRow(Row& r);
  void reorderColumns();
  void makeMatrix();
  void gaussReduce();

  void debugOutputReducers();
  void debugOutputColumns();
  void debugOutputMatrix(std::vector<Row>&);
  void debugOutputMatrixSparse(std::vector<Row>&);
  void debugOutputReducerMatrix();
  void debugOutputSPairMatrix();
  ////////////////////////////////////
  // Data for construct(lev,degree) //
  ////////////////////////////////////

  SchreyerFrame& mFrame;

  const ResPolyRing& mRing;

  // Data used to construct the next matrix
  int mThisLevel;
  int mThisDegree;
  long mNextReducerToProcess;
  //  res_packed_monomial mNextMonom;

  std::unique_ptr<const ResMonomialsWithComponent>
      mSchreyerRes;  // Support structure for mHashTable
  //  const ResMonomialsWithComponent* mSchreyerRes; // Support structure for
  //  mHashTable
  MonomialHashTable<ResMonomialsWithComponent> mHashTable;  // keys: monomials
                                                            // at level lev-2,
                                                            // values: indices
                                                            // into mColumns.
  // or: -1: means is determined to not need to be a column.

  std::vector<Row> mReducers;  // columns: mColumns.  This is a square matrix.
  std::vector<Row>
      mSPairs;  // columns: also mColumns  One row per element at (lev,degree).
  std::vector<long> mSPairComponents;  // index into mFrame.level(mThisLevel)
  std::vector<res_packed_monomial>
      mColumns;  // all the monomials at level lev-2 we need to consider
  //  MemoryBlock<res_monomial_word> mMonomSpace;  // for monomials stored in this
                                               // (lev,degree) in mColumns and
                                               // the lead terms in Row.
  MonomialMemorySpace mMonomSpace2;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
