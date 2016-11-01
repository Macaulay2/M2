// Copyright 2014-2016 Michael E. Stillman

#include "res-f4.hpp"
#include "res-gausser.hpp"
#include "res-schreyer-frame.hpp"

#include <iostream>
#include <ctime>
#include <algorithm>
#include "../timing.hpp"

F4Res::F4Res(
             SchreyerFrame& res
             )
  : mFrame(res),
    mRing(res.ring()),
    mSchreyerRes(new ResMonomialsWithComponent(res.ring().monoid())),
    mHashTable(mSchreyerRes.get(), 10)
{
}

F4Res::~F4Res()
{
  // Nothing to free here.
}

void F4Res::resetMatrix(int lev, int degree)
{
  mThisLevel = lev;
  mThisDegree = degree;
  mNextReducerToProcess = 0;

  // mNextMonom[-1] is the reducer corresponding to this monomial
  mNextMonom = mMonomSpace.reserve(1+monoid().max_monomial_size());
  mNextMonom++;
}

void F4Res::clearMatrix()
{
  mThisLevel = -1;
  mThisDegree = -1;
  mNextReducerToProcess = 0;
  mNextMonom = nullptr;

  auto timeA = timer();
  mHashTable.reset();
  auto timeB = timer();
  mFrame.timeResetHashTable += seconds(timeB-timeA);
  
  mReducers.clear();
  mSPairs.clear();
  mSPairComponents.clear();
  mColumns.clear();
    
  mMonomSpace.reset();
}

/// findDivisor
//    m: monomial at level mThisLevel-1
//    result: monomial at level mThisLevel, IF true is returned
//  returns true if 'm' == inG(result), for some (unique) 'result'.
bool F4Res::findDivisor(res_packed_monomial m, res_packed_monomial result)
{
  // get component of m
  // find the range of monomials to check
  // for each of these, check divisibility in turn
  //   if one works, then return true, and set result.
  long comp = monoid().get_component(m); // component is an index into level mLevel-2
  auto& elem = mFrame.level(mThisLevel-2)[comp];
  auto& lev = mFrame.level(mThisLevel-1);
  for (auto j=elem.mBegin; j<elem.mEnd; ++j)
    {
      // Check divisibility of m by this element
      res_packed_monomial pj = lev[j].mMonom;
      if (monoid().divide(m, pj, result)) // this sets the component to be 0
        {
          monoid().set_component(j, result); // this sets component correctly
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
//     of the coeff matrices.  It uses mThisLevel.
//
// If the ring has skew commuting variables, then result_sign_if_skew is set to 0, 1, or -1.
ComponentIndex F4Res::processMonomialProduct(res_packed_monomial m, res_packed_monomial n, int& result_sign_if_skew)
{
  result_sign_if_skew = 1;
  auto x = monoid().get_component(n);
  auto& p = mFrame.level(mThisLevel-2)[x];
  if (p.mBegin == p.mEnd) return -1;

  monoid().unchecked_mult(m,n,mNextMonom);
  // the component is wrong, after this operation, as it adds components
  // So fix that:
  monoid().set_component(x, mNextMonom);

  if (ring().isSkewCommutative())
    {
      result_sign_if_skew = monoid().skew_mult_sign(ring().skewInfo(), m, n);
      if (result_sign_if_skew == 0)
        return -1;
    }
  return processCurrentMonomial();
}

// new_m is a monomial that we have just created.  There are several
// things that can happen:
//  (1) new_m is already in the hash table (as is).
//       i.e. we have already processed this monomial.
//       in this case new_m[-1] is the index of the divisor for this monomial
//       (possibly -1).
//  (2) new_m is a newly seen monomial in this degree.
//       insert it into the hash table as a seen monomial
//       we set the divisor for new_m: either -1 or some index
//       If there is no divisor, return -1.
//       If there is: create a row, and push onto the mReducers list.
//    (2A) 
ComponentIndex F4Res::processCurrentMonomial()
{
  res_packed_monomial new_m; // a pointer to a monomial we are visiting
  if (mHashTable.find_or_insert(mNextMonom, new_m))
    return static_cast<ComponentIndex>(new_m[-1]); // monom exists, don't save monomial space
  
  // intern the monomial just inserted into the hash table
  mMonomSpace.intern(1+monoid().monomial_size(mNextMonom));

  // leave room for the next monomial.  This might be set below.
  mNextMonom = mMonomSpace.reserve(1+monoid().max_monomial_size());
  mNextMonom++;
  
  bool has_divisor = findDivisor(new_m, mNextMonom);
  if (!has_divisor)
    {
      new_m[-1] = -1; // no divisor exists
      return -1;
    }

  mMonomSpace.intern(1+monoid().monomial_size(mNextMonom));
  
  ComponentIndex thiscol = static_cast<ComponentIndex>(mColumns.size());
  new_m[-1] = thiscol; // this is a HACK: where we keep the divisor
  mColumns.push_back(new_m);

  Row row;
  row.mLeadTerm = mNextMonom;
  mReducers.push_back(row);
  
  // Now we increment mNextMonom, for the next time
  mNextMonom = mMonomSpace.reserve(1+monoid().max_monomial_size());
  mNextMonom++;

  return thiscol;
}
void F4Res::loadRow(Row& r)
{
  //  std::cout << "loadRow: ";
  //  monoid().showAlpha(r.mLeadTerm);
  //  std::cout << std::endl;
  int skew_sign; // will be set to 1, unless ring().isSkewCommutative() is true, then it can be -1,0,1.
  // however, if it is 0, then "val" below will also be -1.
  FieldElement one;
  resGausser().set_one(one);
  long comp = monoid().get_component(r.mLeadTerm);
  auto& thiselement = mFrame.level(mThisLevel-1)[comp];
  //std::cout << "  comp=" << comp << " mDegree=" << thiselement.mDegree << " mThisDegree=" << mThisDegree << std::endl;
  if (thiselement.mDegree == mThisDegree)
    {
      // We only need to add in the current monomial
      //fprintf(stdout, "USING degree 0 monomial\n");
      ComponentIndex val = processMonomialProduct(r.mLeadTerm, thiselement.mMonom, skew_sign);
      if (val < 0) fprintf(stderr, "ERROR: expected monomial to live\n");
      r.mComponents.push_back(val);
      if (skew_sign > 0)
        r.mCoeffs.push_back(one);
      else
        {
          // Only happens if we are in a skew commuting ring.
          FieldElement c;
          ring().resGausser().negate(one, c);
          r.mCoeffs.push_back(c);
        }
      return;
    }
  auto& p = thiselement.mSyzygy;
  auto end = poly_iter(mRing, p, 1);
  auto i = poly_iter(mRing, p);
  for ( ; i != end; ++i)
    {
      ComponentIndex val = processMonomialProduct(r.mLeadTerm, i.monomial(), skew_sign);
      //std::cout << "  monom: " << val << " skewsign=" << skew_sign << " mColumns.size=" << mColumns.size() << std::endl;
      if (val < 0) continue;
      r.mComponents.push_back(val);
      if (skew_sign > 0)
        r.mCoeffs.push_back(i.coefficient());
      else
        {
          // Only happens if we are in a skew commuting ring.
          FieldElement c;
          ring().resGausser().negate(i.coefficient(), c);
          r.mCoeffs.push_back(c);
        }
    }
} 

class ResColumnsSorter
{
public:
  typedef ResMonoid::value monomial;
  typedef ComponentIndex value;
private:
  const ResMonoid &M;
  const F4Res& mComputation;
  const std::vector<res_packed_monomial>& cols;
  int lev;
  const ResSchreyerOrder& myorder;
  //  const std::vector<SchreyerFrame::FrameElement>& myframe;
  
  static long ncmps;
  static long ncmps0;
public:
  int compare(value a, value b)
  {
    ncmps ++;
    fprintf(stdout, "ERROR: should not get here\n");
    return M.compare_grevlex(cols[a],cols[b]);
  }

  bool operator()(value a, value b)
  {
    ncmps0++;
    long comp1 = M.get_component(cols[a]);
    long comp2 = M.get_component(cols[b]);
#if 0
    fprintf(stdout, "comp1 = %ld comp2 = %ld\n", comp1, comp2);

    printf("compare_schreyer: ");
    printf("  m=");
    M.showAlpha(cols[a]);
    printf("\n  n=");    
    M.showAlpha(cols[b]);
    printf("\n  m0=");    
    M.showAlpha(myorder.mTotalMonom[comp1]);
    printf("\n  n0=");    
    M.showAlpha(myorder.mTotalMonom[comp2]);
    printf("\n  tiebreakers: %ld %ld\n",  myorder.mTieBreaker[comp1], myorder.mTieBreaker[comp2]);
#endif
    
    bool result = (M.compare_schreyer(cols[a],cols[b],
                                      myorder.mTotalMonom[comp1], myorder.mTotalMonom[comp2],
                                      myorder.mTieBreaker[comp1], myorder.mTieBreaker[comp2]) == LT);
#if 0
    printf("result = %d\n", result);
#endif
    return result;
  }

  ResColumnsSorter(const ResMonoid& M0, const F4Res& comp, int lev0)
    : M(M0),
      mComputation(comp),
      cols(comp.mColumns),
      lev(lev0),
      myorder(comp.frame().schreyerOrder(lev0-1))
  {
    //printf("Creating a ResColumnsSorter with level = %ld, length = %ld\n", lev, myframe.size());
  }

  long ncomparisons() const { return ncmps; }
  long ncomparisons0() const { return ncmps0; }
  void reset_ncomparisons() { ncmps0 = 0; ncmps = 0; }

  ~ResColumnsSorter() {}
};

static void applyPermutation(ComponentIndex* permutation, std::vector<ComponentIndex>& entries)
{
  // TODO: permutation should be a std::vector too,
  // and we should check the values of the permutation.
  for (ComponentIndex i=0; i<entries.size(); i++)
    entries[i] = permutation[entries[i]];

  // The next is just a consistency check, that maybe can be removed later.
  for (ComponentIndex i=1; i<entries.size(); i++)
    {
      if (entries[i] <= entries[i-1])
        {
          fprintf(stderr, "Internal error: array out of order\n");
          break;
        }
      
    }
}

long ResColumnsSorter::ncmps = 0;
long ResColumnsSorter::ncmps0 = 0;

void F4Res::reorderColumns()
{
  // Set up to sort the columns.
  // Result is an array 0..ncols-1, giving the new order.
  // Find the inverse of this permutation: place values into "ord" column fields.
  // Loop through every element of the matrix, changing its comp array.

#if 0
  std::cout << "-- rows --" << std::endl;
  debugOutputReducers();
  std::cout << "-- columns --" << std::endl;
  debugOutputColumns();
  
  std::cout << "reorderColumns" << std::endl;
#endif
  ComponentIndex ncols = static_cast<ComponentIndex>(mColumns.size());

  // sort the columns

  auto timeA = timer();

  ComponentIndex* column_order = new ComponentIndex[ncols];
  ComponentIndex* ord = new ComponentIndex[ncols];
  ResColumnsSorter C(monoid(), *this, mThisLevel-1);
  
  C.reset_ncomparisons();

  for (ComponentIndex i=0; i<ncols; i++)
    {
      column_order[i] = i;
    }

  if (M2_gbTrace >= 2)
    fprintf(stderr, "  ncomparisons = ");

  std::sort(column_order, column_order+ncols, C);

  auto timeB = timer();
  mFrame.timeSortMatrix += seconds(timeB-timeA);
  
  if (M2_gbTrace >= 2)
    fprintf(stderr, "%ld, ", C.ncomparisons0());

  if (M2_gbTrace >= 2)
    std::cout << " sort time: " << seconds(timeB-timeA) << std::endl;

  timeA = timer();
  ////////////////////////////

  for (ComponentIndex i=0; i<ncols; i++)
    {
      ord[column_order[i]] = i;
    }

#if 0
  std::cout << "column_order: ";
  for (ComponentIndex i=0; i<ncols; i++) std::cout << " " << column_order[i];
  std::cout <<  std::endl;
  std::cout << "ord: ";
  for (ComponentIndex i=0; i<ncols; i++) std::cout << " " << ord[i];
  std::cout <<  std::endl;
#endif
  // Now move the columns into position
  std::vector<res_packed_monomial> sortedColumnArray;
  std::vector<Row> sortedRowArray;

  sortedColumnArray.reserve(ncols);
  sortedRowArray.reserve(ncols);

  for (ComponentIndex i=0; i<ncols; i++)
    {
      ComponentIndex newc = column_order[i];
      sortedColumnArray.push_back(mColumns[newc]);
      sortedRowArray.push_back(Row());
      std::swap(sortedRowArray[i], mReducers[newc]);
    }

  std::swap(mColumns, sortedColumnArray);
  std::swap(mReducers, sortedRowArray);

#if 0
  std::cout << "applying permutation to reducers" << std::endl;
#endif

  for (ComponentIndex i=0; i<mReducers.size(); i++)
    {
#if 0
      std::cout << "reducer " << i << " before:";
      for (ComponentIndex j=0; j<mReducers[i].mComponents.size(); j++) std::cout << " " << mReducers[i].mComponents[j];
      std::cout << std::endl;
#endif      
      applyPermutation(ord, mReducers[i].mComponents);
#if 0
      std::cout << "reducer " << i << " after:";
      for (ComponentIndex j=0; j<mReducers[i].mComponents.size(); j++) std::cout << " " << mReducers[i].mComponents[j];
      std::cout << std::endl;
#endif      
    }
#if 0
  std::cout << "applying permutation to spairs" << std::endl;
#endif
  for (ComponentIndex i=0; i<mSPairs.size(); i++)
    {
#if 0
      std::cout << "spair " << i << " before:";
      for (ComponentIndex j=0; j<mSPairs[i].mComponents.size(); j++) std::cout << " " << mSPairs[i].mComponents[j];
      std::cout << std::endl;
#endif      
      applyPermutation(ord, mSPairs[i].mComponents);
#if 0
      std::cout << "spair " << i << " after:";
      for (ComponentIndex j=0; j<mSPairs[i].mComponents.size(); j++) std::cout << " " << mSPairs[i].mComponents[j];
      std::cout << std::endl;
#endif      
    }

  timeB = timer();
  mFrame.timeReorderMatrix += seconds(timeB-timeA);
  delete [] column_order;
  delete [] ord;
}

void F4Res::makeMatrix()
{
  auto& myframe = mFrame.level(mThisLevel);
  long r = 0;
  long comp = 0;
  for (auto it = myframe.begin(); it != myframe.end(); ++it)
    {
      if (it->mDegree == mThisDegree)
        {
          mSPairs.push_back(Row());
          mSPairComponents.push_back(comp);
          Row& row = mSPairs[r];
          r++;
          row.mLeadTerm = it->mMonom;
          loadRow(row);
          if (M2_gbTrace >= 4)
            if (r % 5000 == 0)
              std::cout << "makeMatrix  sp: " << r << " #rows = " << mColumns.size() << std::endl;
        }
      comp++;
    }
  // Now we process all monomials in the columns array
  while (mNextReducerToProcess < mColumns.size())
    {
      // Warning: mReducers is being appended to during 'loadRow', and 
      // since we act on the Row directly, it might get moved on us!
      // (actually, it did get moved, which prompted this fix)
      Row thisrow;
      std::swap(mReducers[mNextReducerToProcess], thisrow);
      loadRow(thisrow);
      std::swap(mReducers[mNextReducerToProcess], thisrow);
      mNextReducerToProcess++;
      if (M2_gbTrace >= 4)
        if (mNextReducerToProcess % 5000 == 0)
          std::cout << "makeMatrix red: " << mNextReducerToProcess << " #rows = " << mReducers.size() << std::endl;
    }

#if 0
  std :: cout << "-- reducer matrix --" << std::endl;
  debugOutputMatrix(mReducers);
  debugOutputMatrixSparse(mReducers);

  std :: cout << "-- spair matrix --" << std::endl;
  debugOutputMatrix(mSPairs);
  debugOutputMatrixSparse(mSPairs);
#endif
  reorderColumns();
}

void F4Res::gaussReduce()
{
  // Reduce to zero every spair. Recording creates the
  // corresponding syzygy, which is auto-reduced and correctly ordered.

  // allocate a dense row, of correct size
  //std::cout << "gaussReduce: entering" << std::flush;
  ResGausser::dense_row gauss_row;
  mRing.resGausser().dense_row_allocate(gauss_row, static_cast<ComponentIndex>(mColumns.size()));
  FieldElement one;
  mRing.resGausser().set_one(one);

  for (long i=0; i<mSPairs.size(); i++)
    {
      #if 0
      std::cout << "reducing row " << i << std::endl;
      #endif
      // Reduce spair #i
      // fill in dense row with this element.

      poly_constructor result(mRing);

      Row& r = mSPairs[i]; // row to be reduced.
      long comp = mSPairComponents[i];
      result.appendTerm(mFrame.level(mThisLevel)[comp].mMonom,
                        one);
      ///mFrame.mAllMonomials.accountForMonomial(mFrame.level(mThisLevel)[comp].mMonom);
      
      auto& syz = mFrame.level(mThisLevel)[comp].mSyzygy; // this is the element we will fill out

      #if 0
      std::cout << "about to fill from sparse " << i << std::endl;
      #endif

      // Note: in the polynomial ring case, the row r is non-zero.
      // BUT: for skew commuting variables, it can happen that r is zero
      // (e.g. a.(acd<0>) = 0).  In this case we have nothing to reduce.
      if (!r.mComponents.empty())
        {
          ComponentIndex firstcol = r.mComponents[0];
          ComponentIndex lastcol = static_cast<ComponentIndex>(mColumns.size()-1); // maybe: r.mComponents[r.mComponents.size()-1];
          mRing.resGausser().dense_row_fill_from_sparse(gauss_row,
                                                        static_cast<ComponentIndex>(r.mComponents.size()),
                                                        & r.mCoeffs[0],
                                                        & r.mComponents[0]); // FIX: not correct call
          
          while (firstcol <= lastcol)
            {
              #if 0
              std::cout << "about to reduce with col " << firstcol << std::endl;
              #endif
          
              FieldElement elem;

              #if 0
              for (ComponentIndex p=0; p<mColumns.size(); p++)
                {
                  fprintf(stdout, " %d", mRing.resGausser().coeff_to_int(gauss_row.coeffs[p]));
                }
              fprintf(stdout, "\n");
              #endif

#if 0             
              mRing.resGausser().negate(gauss_row.coeffs[firstcol], elem);
              result.appendTerm(mReducers[firstcol].mLeadTerm, elem);
              ///mFrame.mAllMonomials.accountForMonomial(mReducers[firstcol].mLeadTerm);
              mRing.resGausser().dense_row_cancel_sparse_monic(gauss_row,
                                                         static_cast<ComponentIndex>(mReducers[firstcol].mCoeffs.size()),
                                                         & mReducers[firstcol].mCoeffs[0],
                                                         & mReducers[firstcol].mComponents[0]
                                                         );
#else
              mRing.resGausser().dense_row_cancel_sparse(gauss_row,
                                                         static_cast<ComponentIndex>(mReducers[firstcol].mCoeffs.size()),
                                                         mReducers[firstcol].mCoeffs.data(),
                                                         mReducers[firstcol].mComponents.data(),
                                                         elem
                                                         );
              result.appendTerm(mReducers[firstcol].mLeadTerm, elem);
#endif
              firstcol = mRing.resGausser().dense_row_next_nonzero(gauss_row, firstcol+1, lastcol);
            }
        }
      result.setPoly(syz);
    }

  mRing.resGausser().dense_row_deallocate(gauss_row);
}

void F4Res::construct(int lev, int degree)
{
  decltype(timer()) timeA, timeB;

  resetMatrix(lev, degree);
   
  timeA = timer();
  makeMatrix();
  timeB = timer();
  mFrame.timeMakeMatrix += seconds(timeB-timeA);  

  if (M2_gbTrace >= 2)
    std::cout << "  make matrix time: " << seconds(timeB-timeA) << " sec"  << std::endl;

#if 0
  std::cout << "-- rows --" << std::endl;
  debugOutputReducers();
  std::cout << "-- columns --" << std::endl;
  debugOutputColumns();
  std :: cout << "-- reducer matrix --" << std::endl;
  if (true or lev <= 2)
    debugOutputMatrix(mReducers);
  else
    debugOutputMatrixSparse(mReducers);

  std :: cout << "-- reducer matrix --" << std::endl;
  debugOutputMatrix(mReducers);
  debugOutputMatrixSparse(mReducers);

  std :: cout << "-- spair matrix --" << std::endl;
  debugOutputMatrix(mSPairs);
  debugOutputMatrixSparse(mSPairs);
#endif

  if (M2_gbTrace >= 2)
    std::cout << "  (degree,level)=("
            << (mThisDegree-mThisLevel) << ","
            << mThisLevel 
            << ") #spairs="
            << mSPairs.size()
            << " reducer= "
            << mReducers.size() << " x " << mReducers.size()
            << std::endl;

  if (M2_gbTrace >= 2)
    std::cout << "  gauss reduce matrix" << std::endl;

  timeA = timer();
  gaussReduce();
  timeB = timer();
  mFrame.timeGaussMatrix += seconds(timeB-timeA);
  
  if (M2_gbTrace >= 2)
    std::cout << "    time: " << seconds(timeB-timeA) << " sec"  << std::endl;
  //  mFrame.show(-1);

  timeA = timer();
  clearMatrix();
  timeB = timer();
  mFrame.timeClearMatrix += seconds(timeB-timeA);
}

void F4Res::debugOutputReducers()
{
  auto end = mReducers.cend();
  for (auto i=mReducers.cbegin(); i != end; ++i)
    {
      monoid().showAlpha((*i).mLeadTerm);
      std::cout << std::endl;
    }
}
void F4Res::debugOutputColumns()
{
  auto end = mColumns.cend();
  for (auto i=mColumns.cbegin(); i != end; ++i)
    {
      monoid().showAlpha((*i));
      std::cout << std::endl;
    }
}

void F4Res::debugOutputMatrixSparse(std::vector<Row>& rows)
{
  for (ComponentIndex i=0; i<rows.size(); i++)
    {
      fprintf(stdout, "coeffs = ");
      for (long j=0; j<rows[i].mCoeffs.size(); ++j)
        fprintf(stdout, " %d", mRing.resGausser().coeff_to_int(rows[i].mCoeffs[j]));
      fprintf(stdout, "\n comps = ");
      for (long j=0; j<rows[i].mComponents.size(); ++j)
        fprintf(stdout, " %d", rows[i].mComponents[j]);
      fprintf(stdout, "\n");
    }
}

void F4Res::debugOutputMatrix(std::vector<Row>& rows)
{
  for (ComponentIndex i=0; i<rows.size(); i++)
    {
      auto coeff = rows[i].mCoeffs.begin();
      auto end = rows[i].mCoeffs.end();
      auto monom = rows[i].mComponents.begin();
      for (ComponentIndex c=0; c<mColumns.size(); c++)
        {
          if (coeff == end or *monom != c)
            fprintf(stdout, " .");
          else
            {
              fprintf(stdout, " %d", mRing.resGausser().coeff_to_int(*coeff));
              ++coeff;
              ++monom;
            }
        }
      fprintf(stdout, "\n");
    }
}
void F4Res::debugOutputReducerMatrix()
{
  debugOutputMatrix(mReducers);
}
void F4Res::debugOutputSPairMatrix()
{
  debugOutputMatrix(mSPairs);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

