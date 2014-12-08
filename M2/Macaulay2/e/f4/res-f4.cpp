// Copyright 2014 Michael E. Stillman

#include "res-f4.hpp"
#include "gausser.hpp"

F4Res::F4Res(
             F4Mem* Mem,
             const Gausser* KK0,
             const MonomialInfo* MI,
             int max_level
             )
  : mFrame(*MI,max_level),
    mGausser(KK0),
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
  // TODO: write me
}

bool F4Res::findDivisor(int lev, packed_monomial m, packed_monomial result)
{
  // TODO: write me
  return false;
}

long F4Res::processMonomialProduct(packed_monomial m, packed_monomial n)
{
  //    what this does: returns column index for this product, or -1 if the monomial is not needed
  //    additionally: it inserts the product monomial into the hash table, and column array.
  auto& p = mFrame.level(mThisLevel-2)[mMonoid->get_component(n)];
  if (p.mBegin == p.mEnd) return -1;

  packed_monomial new_m;
  packed_monomial mNextMonom2;
  mMonoid->unchecked_mult(m,n,mNextMonom);
  if (mHashTable.find_or_insert(mNextMonom, new_m))
    return new_m[-1]; // monom exists, don't save monomial space

  bool has_divisor = findDivisor(mThisLevel-1, mNextMonom, mNextMonom2);
  if (!has_divisor) return -1;

  mMonomSpace.intern(1+mMonoid->monomial_size(mNextMonom));
  long thiscol = mColumns.size();
  mNextMonom[-1] = thiscol; // this is a HACK
  mColumns.push_back(mNextMonom);

  Row row;
  row.mLeadTerm = mNextMonom2; // TODO: result of monomial lookup
  mReducers.push_back(row);

  // Now we increment mNextMonom, for the next time
  mNextMonom = mMonomSpace.reserve(1+mMonoid->max_monomial_size());
  mNextMonom++;

  return thiscol;
}

void F4Res::loadRow(Row& r)
{
  long comp = mMonoid->get_component(r.mLeadTerm);
  auto& p = mFrame.level(mThisLevel-1)[comp];
#if 0
  for (long i=0; i<p.mLength; i++)
    {
      long val = processMonomialProduct(r.mLeadTerm, p.mMonomials[i]);
      if (val < 0) continue;
      // TODO append val to r.mMonomials
      // TODO append coeff to r.mCoefficients
      // TODO increment r.mLength
    }
#endif
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

