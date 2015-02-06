// Copyright 2014 Michael E. Stillman

#include "res-f4.hpp"
#include "res-gausser.hpp"
#include <iostream>

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
//    m: monomial at level mThisLevel-1
//    result: monomial at level mThisLevel, IF true is returned
//  returns true if 'm' - inG(result), for some (unique) 'result'.
bool F4Res::findDivisor(packed_monomial m, packed_monomial result)
{
  // get component of m
  // find the range of monomials to check
  // for each of these, check divisibility in turn
  //   if one works, then return true, and set result.
  long comp = mMonoid->get_component(m); // component is an index into level mLevel-2
  auto& elem = mFrame.level(mThisLevel-2)[comp];
  auto& lev = mFrame.level(mThisLevel-1);
  for (auto j=elem.mBegin; j<elem.mEnd; ++j)
    {
      // Check divisibility of m by this element
      packed_monomial pj = lev[j].mMonom;
      if (mMonoid->divide(m, pj, result)) // this sets the component to be 0
        {
          mMonoid->set_component(j, result);
          return true;
        }
    }
  
  return false;
}

// A monomial at level lev has the following form:
// m[-1] index of a divisor for this monomial, -1 if no divisor exists
//    this is only used for monomials being placed into the hash table...
// m[0] is a hash value
// m[1] is the component, an index into the lev-1 part of the frame.
// m[2] is the degree,
// m[3..3+#vars-1] is the monomial.
//   Is m[-1] always present

// processMonomialProduct
//     m is a monomial, component is ignored (it determined the possible n's being used here)
//     n is a monomial at level 'mThisLevel-1'
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

#if 0
  std::cout << "monomialProduct(";
  mMonoid->showAlpha(m);
  std::cout << ", ";
  mMonoid->showAlpha(n);
  std::cout << ")" << std::endl;
#endif
  
  packed_monomial new_m; // a pointer to a monomial we are visiting
  mMonoid->unchecked_mult(m,n,mNextMonom);
  // the component is wrong, after this operation, as it adds components
  // So fix that:
  mMonoid->set_component(mMonoid->get_component(n), mNextMonom);
  if (mHashTable.find_or_insert(mNextMonom, new_m))
    return new_m[-1]; // monom exists, don't save monomial space

#if 0
  std::cout << "find_or_insert returned:  ";
  mMonoid->showAlpha(new_m);
  std::cout << std::endl;
#endif
  
  // intern the monomial just inserted into the hash table
  mMonomSpace.intern(1+mMonoid->monomial_size(mNextMonom));
  
  // At this point, we have a new monomial, and a pointer to mNextMonom
  // has been placed into mHashTable.
  // We need to see now what the divisor monomial for mNextMonom is.
  // and if there is one, we need to create a new row for the mReducers matrix.
  // (at least, put it on the queue to be created).
  mNextMonom = mMonomSpace.reserve(1+mMonoid->max_monomial_size());
  mNextMonom++;
  
  bool has_divisor = findDivisor(new_m, mNextMonom);
  if (!has_divisor)
    {
      new_m[-1] = -1; // no divisor exists
      return -1;
    }

#if 0
  std::cout << "findDivisor returned:  ";
  mMonoid->showAlpha(mNextMonom);
  std::cout << std::endl;
#endif
  
  mMonomSpace.intern(1+mMonoid->monomial_size(mNextMonom));
  
  long thiscol = mColumns.size();
  new_m[-1] = thiscol; // this is a HACK: where we keep the divisor
  mColumns.push_back(new_m);

  Row row;
  row.mLeadTerm = mNextMonom;
  mReducers.push_back(row);

#if 0
  std::cout << "added ";
  mMonoid->showAlpha(row.mLeadTerm);
  std::cout << " to reducer todo list" << std::endl;
#endif
  
  // Now we increment mNextMonom, for the next time
  mNextMonom = mMonomSpace.reserve(1+mMonoid->max_monomial_size());
  mNextMonom++;

  return thiscol;
}

void F4Res::loadRow(Row& r)
{
#if 0
  std::cout << "loadRow: entering ";
  mMonoid->showAlpha(r.mLeadTerm);
  std::cout << std::endl;
#endif
  
  long comp = mMonoid->get_component(r.mLeadTerm);
  auto& p = mFrame.level(mThisLevel-1)[comp].mSyzygy;
  monomial_word* this_monom = p.monoms;
  for (long i=0; i<p.len; i++)
    {

#if 0
      std::cout << "loadRow: index i=" << i << std::endl;
#endif

      // TODO: this needs to be cleaned up: we also need to set coeff[]!
      long val = processMonomialProduct(r.mLeadTerm, this_monom);
      this_monom += mMonoid->max_monomial_size(); // part of the iterator
      if (val < 0) continue;
      //      appendToRow(r, *coeff, val); // TODO
    }
} 

void F4Res::makeMatrix()
{
  auto& myframe = mFrame.level(mThisLevel);
  long r = 0;

#if 0  
  std::cout << "makeMatrix: start to load spairs" << std::endl;
#endif
  
  for (auto it = myframe.begin(); it != myframe.end(); ++it)
    if (it->mDegree == mThisDegree)
      {
        mSPairs.push_back(Row());
        Row& row = mSPairs[r];
        r++;
        row.mLeadTerm = it->mMonom;
#if 0
        std::cout << "about to call loadRow" << std::endl;
#endif
        loadRow(row);
      }

#if 0
  std::cout << "makeMatrix: start to load reducers" << std::endl;
#endif
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
  std::cout << "-- rows --" << std::endl;
  debugOutputReducers();
  std::cout << "-- columns --" << std::endl;
  debugOutputColumns();
}

void F4Res::debugOutputReducers()
{
  auto end = mReducers.cend();
  for (auto i=mReducers.cbegin(); i != end; ++i)
    {
      mMonoid->showAlpha((*i).mLeadTerm);
      std::cout << std::endl;
    }
}
void F4Res::debugOutputColumns()
{
  auto end = mColumns.cend();
  for (auto i=mColumns.cbegin(); i != end; ++i)
    {
      mMonoid->showAlpha((*i));
      std::cout << std::endl;
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

