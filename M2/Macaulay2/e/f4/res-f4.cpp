// Copyright 2014 Michael E. Stillman

#include "res-f4.hpp"
#include "res-gausser.hpp"

F4Res::F4Res(
             ResF4Mem* Mem,
             const ResGausser* KK0,
             const MonomialInfo* MI,
             int max_level
             )
  : mFrame(*MI,max_level),
    mResGausser(KK0),
    mMonoid(MI),
    mMem(Mem),
    mSchreyerRes(new MonomialsWithComponent(*mMonoid)),
    mHashTable(mSchreyerRes)
{
}

M2_arrayint F4Res::getBetti(int type) const
{
  if (type == 1)
    return mFrame.getBettiFrame();
  ERROR("betti display not implemenented yet");
  return 0;
}

void F4Res::resetMatrix(int lev, int degree)
{
  mThisLevel = lev;
  mThisDegree = degree;
  mNextReducerToProcess = 0;

  // mNextMonom[-1] is the reducer corresponding to this monomial
  mNextMonom = mMonomSpace.reserve(1+mMonoid->max_monomial_size());
  mNextMonom++;
}

void F4Res::clearMatrix()
{
  mThisLevel = -1;
  mThisDegree = -1;
  mNextReducerToProcess = 0;

  mMonomSpace.reset();

  mNextMonom = nullptr;
  // TODO: write the rest of me.  What needs to be removed?
}

/// findDivisor
//    m: monomial at level '??'
//    result: monomial at level '??', IF true is returned
//  returns true if 'm' - inG(result), for some (unique) 'result'.
bool F4Res::findDivisor(packed_monomial m, packed_monomial result)
{
  // get component of m
  // find the range of monomials to check
  // for each of these, check divisibility in turn
  //   if one works, then return true, and set result.
  int comp = mMonoid->get_component(m); // component is an index into level mLevel-2
  auto& elem = mFrame.level(mThisLevel-2)[comp];
  auto& lev = mFrame.level(mThisLevel-1);
  for (auto j=elem.mBegin; j<elem.mEnd; ++j)
    {
      // Check divisibility of m by this element
      packed_monomial pj = lev[j].mMonom;
      if (mMonoid->divide(m, pj, result)) // TODO: make sure this returns a valid monomial! Set the component too!!
        return true;
    }
  
  return false;
}

// processMonomialProduct
//     m and n are monomials at level 'mThisLevel'
//     compute their product, and return the column index of this product
//       or -1, if the monomial is not needed.
//     additionally: the product monomial is inserted into the hash table
//     and column array (if it is not already there).
// caveats: this function is only to be used during construction 
//     of the coeff matrices.  It used mThisLevel
long F4Res::processMonomialProduct(packed_monomial m, packed_monomial n)
{
  auto& p = mFrame.level(mThisLevel-2)[mMonoid->get_component(n)];
  if (p.mBegin == p.mEnd) return -1;

  packed_monomial new_m;
  packed_monomial mNextMonom2; // TODO: this needs to be allocated
  mMonoid->unchecked_mult(m,n,mNextMonom);
  if (mHashTable.find_or_insert(mNextMonom, new_m))
    return new_m[-1]; // monom exists, don't save monomial space

  bool has_divisor = findDivisor(mNextMonom, mNextMonom2);
  if (!has_divisor) return -1;

  mMonomSpace.intern(1+mMonoid->monomial_size(mNextMonom));
  long thiscol = mColumns.size();
  mNextMonom[-1] = thiscol; // this is a HACK
  mColumns.push_back(mNextMonom);

  Row row;
  row.mLeadTerm = mNextMonom2; // TODO: result of monomial lookup: who is saving this space?
  mReducers.push_back(row);

  // Now we increment mNextMonom, for the next time
  mNextMonom = mMonomSpace.reserve(1+mMonoid->max_monomial_size());
  mNextMonom++;

  return thiscol;
}

void F4Res::loadRow(Row& r)
{
  long comp = mMonoid->get_component(r.mLeadTerm);
  auto& p = mFrame.level(mThisLevel-1)[comp].mSyzygy;
  for (long i=0; i<p.len; i++)
    {
      int* coeff = 0; // TODO: needs to be taken from the coeff array
      monomial_word* monoms = p.monoms;
      // TODO: this needs to be cleaned up
      long val = processMonomialProduct(r.mLeadTerm, monoms);
      monoms += mMonoid->max_monomial_size();
      if (val < 0) continue;
      appendToRow(r, *coeff, val);
    }
} 

void F4Res::makeMatrix()
{
  auto& myframe = mFrame.level(mThisLevel);
  long r = 0;
  for (auto it = myframe.begin(); it != myframe.end(); ++it, ++r)
    if (it->mDegree == mThisDegree)
      {
        Row& row = mSPairs[r];
        row.mLeadTerm = it->mMonom;
        loadRow(row);
      }

  // Now we process all monomials in the columns array
  while (mNextReducerToProcess < mColumns.size())
    loadRow(mReducers[mNextReducerToProcess++]);

  // Now sort columns (and therefore rows too)
  // TODO
}

void F4Res::construct(int lev, int degree)
{
  resetMatrix(lev, degree);
  makeMatrix();
  //  gaussReduce();
  //  newSyzElems();
  //  clearMatrix();
}

#if 0
  // First step: check that (lev-1,degree-1) and (lev,degree-1) have been finished.

  // Step 1.
  // Loop through monomials in level(lev).  For each
  //  call processMonomial
  auto end = level(lev).end();
  for (auto it=level(lev).begin(); it != end; ++it)
    if (it->mDegree == degree)
      processMonomial(lev, it->mMonom);

  long next = 0;
  while (next < mReducers.size())
    {
      processReducer(lev-1, next); // might increase mReducers
    }
  // Construct the matrix
  //   a. place the lead terms at level lev onto the mTODO list
  //      Insert them into the hash table, and find their (unique) reducer monomial
  //   b. for each reducer monomial in the todo list:
  //      for each monomial in corresponding poly:
  //        if it cannot be a lead term, continue
  //        do the multiplication
  //        look it up in the hash table
  //          if there: add the (monomial,coeff) to the poly. (what monomial?)
  //          if not: lookup the divisor monomial
  //                  if it is not divisible by any elements, then
  //                     insert into the hash table
  //                     continue, ignoring this monomial
  //                  if so: also insert it into the hash table, construct the quotient element, insert into TODO list
  //   c. Maybe sort the row list
  //   d. This is the matrix, I guess
  // 
  // Reduce the Spairs with respect to this matrix (or, do a solve)
  //   For each spair:
  //     create the dense array
  //     reduce it to 0, keep the (reducer,coeff) result poly
  //     store that result into the frame

}
#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

