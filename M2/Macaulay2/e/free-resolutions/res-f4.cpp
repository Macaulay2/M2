// Copyright 2014 Michael E. Stillman

#include "stdinc.hpp"

#include "res-f4.hpp"
#include "res-gausser.hpp"
#include "res-schreyer-frame.hpp"

#include <iostream>
#include <ctime>
#include <algorithm>

F4Res::F4Res(
             SchreyerFrame& res
             )
  : mFrame(res),
    mRing(res.ring()),
    mSchreyerRes(new MonomialsWithComponent(res.ring().monoid())),
    mHashTable(mSchreyerRes)
{
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

  mHashTable.reset();
  mReducers.clear();
  mSPairs.clear();
  mSPairComponents.clear();
  mColumns.clear();
    
  mMonomSpace.reset();
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
  long comp = monoid().get_component(m); // component is an index into level mLevel-2
  auto& elem = mFrame.level(mThisLevel-2)[comp];
  auto& lev = mFrame.level(mThisLevel-1);
  for (auto j=elem.mBegin; j<elem.mEnd; ++j)
    {
      // Check divisibility of m by this element
      packed_monomial pj = lev[j].mMonom;
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
//     of the coeff matrices.  It used mThisLevel
ComponentIndex F4Res::processMonomialProduct(packed_monomial m, packed_monomial n)
{
  auto& p = mFrame.level(mThisLevel-2)[monoid().get_component(n)];
  if (p.mBegin == p.mEnd) return -1;

#if 0
  std::cout << "monomialProduct(";
  monoid().showAlpha(m);
  std::cout << ", ";
  monoid().showAlpha(n);
  std::cout << ")" << std::endl;
#endif
  
  packed_monomial new_m; // a pointer to a monomial we are visiting
  monoid().unchecked_mult(m,n,mNextMonom);
  // the component is wrong, after this operation, as it adds components
  // So fix that:
  monoid().set_component(monoid().get_component(n), mNextMonom);
  if (mHashTable.find_or_insert(mNextMonom, new_m))
    return static_cast<ComponentIndex>(new_m[-1]); // monom exists, don't save monomial space

#if 0
  std::cout << "find_or_insert returned:  ";
  monoid().showAlpha(new_m);
  std::cout << std::endl;
#endif
  
  // intern the monomial just inserted into the hash table
  mMonomSpace.intern(1+monoid().monomial_size(mNextMonom));
  
  // At this point, we have a new monomial, and a pointer to mNextMonom
  // has been placed into mHashTable.
  // We need to see now what the divisor monomial for mNextMonom is.
  // and if there is one, we need to create a new row for the mReducers matrix.
  // (at least, put it on the queue to be created).
  mNextMonom = mMonomSpace.reserve(1+monoid().max_monomial_size());
  mNextMonom++;
  
  bool has_divisor = findDivisor(new_m, mNextMonom);
  if (!has_divisor)
    {
      new_m[-1] = -1; // no divisor exists
      return -1;
    }

#if 0
  std::cout << "findDivisor returned:  ";
  monoid().showAlpha(mNextMonom);
  std::cout << std::endl;
#endif
  
  mMonomSpace.intern(1+monoid().monomial_size(mNextMonom));
  
  ComponentIndex thiscol = static_cast<ComponentIndex>(mColumns.size());
  new_m[-1] = thiscol; // this is a HACK: where we keep the divisor
  mColumns.push_back(new_m);

  Row row;
  row.mLeadTerm = mNextMonom;
  mReducers.push_back(row);

#if 0
  std::cout << "added ";
  monoid().showAlpha(row.mLeadTerm);
  std::cout << " to reducer todo list" << std::endl;
#endif
  
  // Now we increment mNextMonom, for the next time
  mNextMonom = mMonomSpace.reserve(1+monoid().max_monomial_size());
  mNextMonom++;

  return thiscol;
}

void F4Res::loadRow(Row& r)
{
  FieldElement one;
  resGausser().set_one(one);
  long comp = monoid().get_component(r.mLeadTerm);
  auto& thiselement = mFrame.level(mThisLevel-1)[comp];
  if (thiselement.mDegree == mThisDegree)
    {
      // We only need to add in the current monomial
      //fprintf(stdout, "USING degree 0 monomial\n");
      ComponentIndex val = processMonomialProduct(r.mLeadTerm, thiselement.mMonom);
      if (val < 0) fprintf(stderr, "ERROR: expected monomial to live\n");
      r.mComponents.push_back(val);
      r.mCoeffs.push_back(one);
      return;
    }
  auto& p = thiselement.mSyzygy;
  auto end = poly_iter(mRing, p, 1);
  auto i = poly_iter(mRing, p);
  for ( ; i != end; ++i)
    {
      ComponentIndex val = processMonomialProduct(r.mLeadTerm, i.monomial());
      if (val < 0) continue;
      r.mComponents.push_back(val);
      r.mCoeffs.push_back(i.coefficient());
    }
} 

// IMPORTANT TODO!!!!
// This class needs to implement Schreyer order comparison.
class ResColumnsSorter
{
public:
  typedef MonomialInfo::value monomial;
  typedef ComponentIndex value;
private:
  const MonomialInfo &M;
  const F4Res& mComputation;
  const std::vector<packed_monomial>& cols;
  int lev;
  const std::vector<SchreyerFrame::FrameElement>& myframe;
  
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
    M.showAlpha(myframe[comp1].mTotalMonom);
    printf("\n  n0=");    
    M.showAlpha(myframe[comp2].mTotalMonom);
    printf("\n  tiebreakers: %ld %ld\n",  myframe[comp1].mTiebreaker, myframe[comp2].mTiebreaker);
#endif
    
    bool result = (M.compare_schreyer(cols[a],cols[b],
                               myframe[comp1].mTotalMonom, myframe[comp2].mTotalMonom,
                                      myframe[comp1].mTiebreaker, myframe[comp2].mTiebreaker) < 0); // TODO: make sure this is LT !!
    #if 0
    printf("result = %d\n", result);
    #endif
    return result;
  }

  ResColumnsSorter(const MonomialInfo& M0, const F4Res& comp, int lev0)
    : M(M0),
      mComputation(comp),
      cols(comp.mColumns),
      lev(lev0),
      myframe(comp.frame().level(lev0-1))
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

  ComponentIndex* column_order = new ComponentIndex[ncols];
  ComponentIndex* ord = new ComponentIndex[ncols];

  ResColumnsSorter C(monoid(), *this, mThisLevel-1);

  C.reset_ncomparisons();

  clock_t begin_time0 = clock();
  for (ComponentIndex i=0; i<ncols; i++)
    {
      column_order[i] = i;
    }

  if (M2_gbTrace >= 2)
    fprintf(stderr, "ncomparisons = ");

  std::sort(column_order, column_order+ncols, C);

  clock_t end_time0 = clock();
  if (M2_gbTrace >= 2)
    fprintf(stderr, "%ld, ", C.ncomparisons0());
  double nsecs0 = (double)(end_time0 - begin_time0)/CLOCKS_PER_SEC;
  //clock_sort_columns += nsecs0;
  if (M2_gbTrace >= 2)
    fprintf(stderr, " sort time = %f\n", nsecs0);

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
  std::vector<packed_monomial> sortedColumnArray;
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
  
  for (ComponentIndex i=0; i<mReducers.size(); i++)
    {
      applyPermutation(ord, mReducers[i].mComponents);
    }
  for (ComponentIndex i=0; i<mSPairs.size(); i++)
    {
      applyPermutation(ord, mSPairs[i].mComponents);
    }

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
      mFrame.mAllMonomials.accountForMonomial(mFrame.level(mThisLevel)[comp].mMonom);
      
      auto& syz = mFrame.level(mThisLevel)[comp].mSyzygy; // this is the element we will fill out

      #if 0
      std::cout << "about to fill from sparse " << i << std::endl;
      #endif
      
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
          
          mRing.resGausser().negate(gauss_row.coeffs[firstcol], elem);
          result.appendTerm(mReducers[firstcol].mLeadTerm, elem);
          mFrame.mAllMonomials.accountForMonomial(mReducers[firstcol].mLeadTerm);
          mRing.resGausser().dense_row_cancel_sparse(gauss_row,
                                                     static_cast<ComponentIndex>(mReducers[firstcol].mCoeffs.size()),
                                                     & mReducers[firstcol].mCoeffs[0],
                                                     & mReducers[firstcol].mComponents[0]
                                                     );
          firstcol = mRing.resGausser().dense_row_next_nonzero(gauss_row, firstcol+1, lastcol);
        }
      result.setPoly(syz);
    }

  mRing.resGausser().dense_row_deallocate(gauss_row);
}

void F4Res::construct(int lev, int degree)
{
  resetMatrix(lev, degree);

  if (M2_gbTrace >= 2)
    std::cout << "make matrix" << std::endl;
  clock_t begin_time0 = clock();
  makeMatrix();
  clock_t end_time0 = clock();
  double nsecs0 = (double)(end_time0 - begin_time0)/CLOCKS_PER_SEC;
  if (M2_gbTrace >= 2)
    std::cout << "  time: " << nsecs0 << std::endl;

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
  std::cout << "(level,degree)=("
            << mThisLevel << ","
            << mThisDegree
            << ") #spairs="
            << mSPairs.size()
            << " reducer= "
            << mReducers.size() << " x " << mReducers.size()
            << std::endl;

  if (M2_gbTrace >= 2)
    std::cout << "gauss reduce matrix" << std::endl;
  begin_time0 = clock();
  gaussReduce();
  end_time0 = clock();
  nsecs0 = (double)(end_time0 - begin_time0)/CLOCKS_PER_SEC;
  if (M2_gbTrace >= 2)
    std::cout << "  time: " << nsecs0 << std::endl;
  //  mFrame.show(-1);
  clearMatrix();
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
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions "
// indent-tabs-mode: nil
// End:

