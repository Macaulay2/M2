// Copyright 2014 Michael E. Stillman

#ifndef _res_f4_hpp_
#define _res_f4_hpp_

#include "memblock.hpp"
#include "res-f4-mem.hpp"
#include "monhashtable.hpp"
#include "res-poly-ring.hpp"
#include <assert.h>
#define M2_ASSERT assert

class ResGausser;
class MonomialInfo;
class SchreyerFrame;
/////////////////////////////////////////////////////////////////////////////

class F4Res
{
  friend class ResColumnsSorter;
public:
  F4Res(
        SchreyerFrame& res
       );

  ~F4Res() {
  }

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
  const MonomialInfo& monoid() const { return mRing.monoid(); }
  const ResPolyRing& ring() const { return mRing; }
  
private:
  struct Row {
    packed_monomial mLeadTerm; // monomial (level lev-1) giving rise to this row
    // The following two should have the same length.
    std::vector<ComponentIndex> mComponents; // indices into mColumns
    std::vector<FieldElement> mCoeffs;
    //ResGausser::CoefficientArray mCoeffs; // from an ResF4Mem..
    Row() : mLeadTerm(nullptr) {}
  };

  ////////////////////////////////////
  // Functions for construction //////
  ////////////////////////////////////
  void resetMatrix(int lev, int degree); // remember to clearMatrix before calling this.
  void clearMatrix();
  bool findDivisor(packed_monomial m, packed_monomial result);
  ComponentIndex processMonomialProduct(packed_monomial m, packed_monomial n);
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
  packed_monomial mNextMonom;

  const MonomialsWithComponent* mSchreyerRes; // Support structure for mHashTable
  MonomialHashTable<MonomialsWithComponent> mHashTable; // keys: monomials at level lev-2, values: indices into mColumns.
  // or: -1: means is determined to not need to be a column.

  std::vector<Row> mReducers;  // columns: mColumns.  This is a square matrix.
  std::vector<Row> mSPairs;  // columns: also mColumns  One row per element at (lev,degree).
  std::vector<long> mSPairComponents; // index into mFrame.level(mThisLevel)
  std::vector<packed_monomial> mColumns; // all the monomials at level lev-2 we need to consider
  MemoryBlock<monomial_word> mMonomSpace; // for monomials stored in this (lev,degree) in mColumns and the lead terms in Row.
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions "
// indent-tabs-mode: nil
// End:
