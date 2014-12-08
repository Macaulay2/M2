// Copyright 2014 Michael E. Stillman

#ifndef _res_f4_hpp_
#define _res_f4_hpp_

#include "res-f4-types.hpp"
#include "f4-mem.hpp"
#include "res-schreyer-frame.hpp"
#include "monhashtable.hpp"

#include <assert.h>
#define M2_ASSERT assert

class Gausser;
class MonomialInfo;

/////////////////////////////////////////////////////////////////////////////

class F4Res
{
public:
  F4Res(
        F4Mem* Mem,
        const Gausser* KK0,
        const MonomialInfo* MI,
        int max_level
       );

  ~F4Res() {
    delete mMem;
  }

  SchreyerFrame& frame() { return mFrame; }

  // Constructs the elements of the GB at level 'lev', in the given degree.
  // The following must have been done:
  //    construct(lev, degree-1)
  // and
  //    construct(lev-1, degree-1)
  // NOTE: it is not needed to have done: construct(lev-1,degree)
  void construct(int lev, int degree);

  M2_arrayint getBetti(int type) const;
  
private:
  SchreyerFrame mFrame;

  const Gausser* mGausser;
  const MonomialInfo* mMonoid;
  const F4Mem* mMem; // Used for what TODO?

  struct Row {
    packed_monomial mLeadTerm; // monomial (level lev-1) giving rise to this row
    int mLength; // common length of the following two parts
    F4CoefficientArray mCoeffs; // from an F4Mem..
    int* mComponents; // where is the space? entries are indices into mColumns.
  };

  ////////////////////////////////////
  // Functions for construction //////
  ////////////////////////////////////
  void resetMatrix(int lev, int degree);
  void clearMatrix();
  bool findDivisor(int lev, packed_monomial m, packed_monomial result);
  long processMonomialProduct(packed_monomial m, packed_monomial n);
  void loadRow(Row& r);
  void makeMatrix();

  ////////////////////////////////////
  // Data for construct(lev,degree) //
  ////////////////////////////////////


  // Data used to construct the next matrix
  int mThisLevel;
  int mThisDegree;
  long mNextReducerToProcess;
  packed_monomial mNextMonom;

  const MonomialsWithComponent* mSchreyerRes;
  MonomialHashTable<MonomialsWithComponent> mHashTable; // keys: monomials at level lev-2, values: indices into mColumns.
  // or: -1: means is determined to not need to be a column.

  std::vector<Row> mReducers;  // columns: mColumns.  This is a square matrix.
  std::vector<Row> mSPairs;  // columns: also mColumns  One row per element at (lev,degree).
  std::vector<packed_monomial> mColumns; // all the monomials at level lev-2 we need to consider
  MemoryBlock<monomial_word> mMonomSpace; // for monomials stored in this (lev,degree) in mColumns and the lead terms in Row.
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
