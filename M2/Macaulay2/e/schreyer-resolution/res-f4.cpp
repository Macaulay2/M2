// Copyright 2014-2016 Michael E. Stillman

#include "schreyer-resolution/res-f4.hpp"
#include "schreyer-resolution/res-gausser.hpp"
#include "schreyer-resolution/res-schreyer-frame.hpp"

#include "schreyer-resolution/res-monomial-sorter.hpp"

#include "monoid.hpp"
#include "ntuple.hpp"
#include "memtailor.h"
#include "text-io.hpp"

#include <iostream>
#include <ctime>
#include <algorithm>
#include "timing.hpp"

#if 0
// This is test code which should be removed, or placed elsewhere!
#include <thread>
#include "../../system/supervisor.hpp"
#include "../../system/supervisorinterface.h"
static long val[2];
static void* testFcn1(void* vint)
{
  long v = reinterpret_cast<long>(vint);
  std::cout << "starting fcn1 with v = " << v << std::endl;
  val[0] = 10;
  return nullptr;
}
static void* testFcn2(void* vint)
{
  long v = reinterpret_cast<long>(vint);
  std::cout << "starting fcn2 with v = " << v << std::endl;
  val[1] = 20;
  return nullptr;
}
void testTasks()
{
  val[0] = 666;
  val[1] = 666;
  ThreadTask* t1 = createThreadTask("task 1", testFcn1, reinterpret_cast<void*>(1L), 0, 0, 0);
  ThreadTask* t2 = createThreadTask("task 1", testFcn2, reinterpret_cast<void*>(2L), 0, 0, 0);
  pushTask(t1);
  pushTask(t2);
  waitOnTask(t2);
  std::cout << val[0] << " " << val[1] << std::endl;
}
#endif

F4Res::F4Res(SchreyerFrame& res)
    : mFrame(res),
      mRing(res.ring()),
      mSchreyerRes(new ResMonomialsWithComponent(res.ring().monoid())),
      mHashTable(mSchreyerRes.get(), 10)
{
#if 0
  std::cout << "hardware threads: " << std::thread::hardware_concurrency() << std::endl;
  std::cout << "testing thread tasks" << std::endl;
  testTasks();
  std::cout << "  done testing thread tasks" << std::endl;
#endif
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
}

void F4Res::clearMatrix()
{
  mThisLevel = -1;
  mThisDegree = -1;
  mNextReducerToProcess = 0;

  auto timeA = timer();
  mHashTable.reset();
  auto timeB = timer();
  mFrame.timeResetHashTable += seconds(timeB - timeA);

  for (auto& f : mReducers)
    {
      mRing.resGausser().deallocate(f.mCoeffs);
    }

  for (auto& f : mSPairs)
    {
      mRing.resGausser().deallocate(f.mCoeffs);
    }

  mReducers.clear();
  mSPairs.clear();
  mSPairComponents.clear();
  mColumns.clear();

  mMonomSpace2.freeAllAllocs();
}

/// findDivisor
//    m: monomial at level mThisLevel-1
//    result: monomial at level mThisLevel, IF true is returned
//  returns true if 'm' == inG(result), for some (unique) 'result'.
bool F4Res::findDivisor(res_const_packed_monomial m, res_packed_monomial result)
{
  // get component of m
  // find the range of monomials to check
  // for each of these, check divisibility in turn
  //   if one works, then return true, and set result.
  long comp =
      monoid().get_component(m);  // component is an index into level mLevel-2
  auto& elem = mFrame.level(mThisLevel - 2)[comp];
  auto& lev = mFrame.level(mThisLevel - 1);
  for (auto j = elem.mBegin; j < elem.mEnd; ++j)
    {
      // Check divisibility of m by this element
      res_packed_monomial pj = lev[j].mMonom;
      if (monoid().divide(m, pj, result))  // this sets the component to be 0
        {
          monoid().set_component(j, result);  // this sets component correctly
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
//     m is a monomial, component is ignored (it determined the possible n's
//     being used here)
//     n is a monomial at level 'mThisLevel-1'
//     compute their product, and return the column index of this product
//       or -1, if the monomial is not needed.
//     additionally: the product monomial is inserted into the hash table
//     and column array (if it is not already there).
// caveats: this function is only to be used during construction
//     of the coeff matrices.  It uses mThisLevel.
//
// If the ring has skew commuting variables, then result_sign_if_skew is set to
// 0, 1, or -1.
ComponentIndex F4Res::processMonomialProduct(res_const_packed_monomial m,
                                             res_const_packed_monomial n,
                                             int& result_sign_if_skew)
{
  result_sign_if_skew = 1;
  auto x = monoid().get_component(n);
  auto& p = mFrame.level(mThisLevel - 2)[x];
  if (p.mBegin == p.mEnd) return -1;

  int* thisMonom = mMonomSpace2.allocate(1 + monoid().max_monomial_size());
  thisMonom++; // so thisMonom[-1] exists, but is not part of the monomial, as far as
  monoid().unchecked_mult(m, n, thisMonom);
  monoid().set_component(x, thisMonom);

  if (ring().isSkewCommutative())
    {
      result_sign_if_skew = monoid().skew_mult_sign(ring().skewInfo(), m, n);
      if (result_sign_if_skew == 0)
        {
          mMonomSpace2.popLastAlloc(thisMonom-1); // we did not need this monomial!
          return -1;
        }
    }
  return processCurrentMonomial(thisMonom);
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
ComponentIndex F4Res::processCurrentMonomial(res_packed_monomial thisMonom)
{
  res_packed_monomial new_m;  // a pointer to a monomial we are visiting
  if (mHashTable.find_or_insert(thisMonom, new_m))
    {
      // we did not need the monomial. So pop it.
      mMonomSpace2.popLastAlloc(thisMonom-1);
      return static_cast<ComponentIndex>(
                                         new_m[-1]);  // monom exists, don't save monomial space
    }

  // At this point thisMonom has been inserted.  We keep it.

  thisMonom = mMonomSpace2.allocate(1 + monoid().max_monomial_size());
  thisMonom++; // so thisMonom[-1] exists, but is not part of the monomial, as far as
  
  bool has_divisor = findDivisor(new_m, thisMonom);
  if (!has_divisor)
    {
      mMonomSpace2.popLastAlloc(thisMonom-1);
      new_m[-1] = -1;  // no divisor exists
      return -1;
    }

  ComponentIndex thiscol = static_cast<ComponentIndex>(mColumns.size());
  new_m[-1] = thiscol;  // this is a HACK: where we keep the divisor
  mColumns.push_back(new_m);

  Row row;
  row.mLeadTerm = thisMonom;
  mReducers.push_back(row);

  return thiscol;
}
void F4Res::loadRow(Row& r)
{
  //  std::cout << "loadRow: " << std::endl;

  r.mCoeffs = resGausser().allocateCoefficientVector();

  //  monoid().showAlpha(r.mLeadTerm);
  //  std::cout << std::endl;
  int skew_sign;  // will be set to 1, unless ring().isSkewCommutative() is
                  // true, then it can be -1,0,1.
  // however, if it is 0, then "val" below will also be -1.
  long comp = monoid().get_component(r.mLeadTerm);
  auto& thiselement = mFrame.level(mThisLevel - 1)[comp];
  // std::cout << "  comp=" << comp << " mDegree=" << thiselement.mDegree << "
  // mThisDegree=" << mThisDegree << std::endl;
  if (thiselement.mDegree == mThisDegree)
    {
      // We only need to add in the current monomial
      // fprintf(stdout, "USING degree 0 monomial\n");
      ComponentIndex val =
          processMonomialProduct(r.mLeadTerm, thiselement.mMonom, skew_sign);
      if (val < 0) fprintf(stderr, "ERROR: expected monomial to live\n");
      r.mComponents.push_back(val);
      if (skew_sign > 0)
        mRing.resGausser().pushBackOne(r.mCoeffs);
      else
        {
          // Only happens if we are in a skew commuting ring.
          ring().resGausser().pushBackMinusOne(r.mCoeffs);
        }
      return;
    }
  auto& p = thiselement.mSyzygy;
  auto end = ResPolynomialIterator(mRing, p, 1);
  auto i = ResPolynomialIterator(mRing, p);
  for (; i != end; ++i)
    {
      ComponentIndex val =
          processMonomialProduct(r.mLeadTerm, i.monomial(), skew_sign);
      // std::cout << "  monom: " << val << " skewsign=" << skew_sign << "
      // mColumns.size=" << mColumns.size() << std::endl;
      if (val < 0) continue;
      r.mComponents.push_back(val);
      if (skew_sign > 0)
        mRing.resGausser().pushBackElement(
            r.mCoeffs, p.coeffs, i.coefficient_index());
      else
        {
          // Only happens if we are in a skew commuting ring.
          mRing.resGausser().pushBackNegatedElement(
              r.mCoeffs, p.coeffs, i.coefficient_index());
        }
    }
}

static void applyPermutation(ComponentIndex* permutation,
                             std::vector<ComponentIndex>& entries)
{
  // TODO: permutation should be a std::vector too,
  // and we should check the values of the permutation.
  for (ComponentIndex i = 0; i < entries.size(); i++)
    entries[i] = permutation[entries[i]];

  // The next is just a consistency check, that maybe can be removed later.
  for (ComponentIndex i = 1; i < entries.size(); i++)
    {
      if (entries[i] <= entries[i - 1])
        {
          fprintf(stderr, "Internal error: array out of order\n");
          break;
        }
    }
}

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

  ComponentIndex* ord = new ComponentIndex[ncols];

  auto timeA = timer();
  ResMonomialSorter sorter(ring().originalMonoid(), monoid(), frame().schreyerOrder(mThisLevel-2), mColumns);
  auto column_order = sorter.sort();
  auto timeB = timer();
  double nsec_sort2 = seconds(timeB - timeA);
  mFrame.timeSortMatrix += nsec_sort2;
  auto ncompares = sorter.numComparisons();
  
  if (M2_gbTrace >= 2)
    std::cout << "  #comparisons sorting " << ncols << " columns = " << ncompares << " ";
  
  if (M2_gbTrace >= 1)
    std::cout << " sort time: " << nsec_sort2 << std::endl;

  timeA = timer();
  ////////////////////////////

  for (ComponentIndex i = 0; i < ncols; i++)
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

  for (ComponentIndex i = 0; i < ncols; i++)
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

  for (ComponentIndex i = 0; i < mReducers.size(); i++)
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
  for (ComponentIndex i = 0; i < mSPairs.size(); i++)
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
  mFrame.timeReorderMatrix += seconds(timeB - timeA);
  delete[] ord;
}

void F4Res::makeMatrix()
{
  // std::cout << "entering makeMatrix()" << std::endl;
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
              std::cout << "makeMatrix  sp: " << r
                        << " #rows = " << mColumns.size() << std::endl;
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
          std::cout << "makeMatrix red: " << mNextReducerToProcess
                    << " #rows = " << mReducers.size() << std::endl;
    }

  reorderColumns();

#if 0
  debugOutputReducers();
  debugOutputColumns();
  std :: cout << "-- reducer matrix --" << std::endl;
  debugOutputMatrix(mReducers);
  debugOutputMatrixSparse(mReducers);

  std :: cout << "-- spair matrix --" << std::endl;
  debugOutputMatrix(mSPairs);
  debugOutputMatrixSparse(mSPairs);
#endif
}

//#define DEBUG_GAUSS

void F4Res::gaussReduce()
{
  bool onlyConstantMaps = false;
  std::vector<bool> track(mReducers.size());
  if (onlyConstantMaps)  // and not exterior algebra?
    {
      for (auto i = 0; i < mReducers.size(); i++)
        {
          track[i] = monoid().is_divisible_by_var_in_range(
              mReducers[i].mLeadTerm,
              monoid().n_vars() - mThisLevel + 1,
              monoid().n_vars() - 1);
        }
    }

  // Reduce to zero every spair. Recording creates the
  // corresponding syzygy, which is auto-reduced and correctly ordered.

  // allocate a dense row, of correct size
  CoefficientVector gauss_row = mRing.resGausser().allocateCoefficientVector(
      static_cast<ComponentIndex>(mColumns.size()));
  //  std::cout << "gauss_row: " << (gauss_row.isNull() ? "null" : "not-null")
  //  << std::endl;
  //  std::cout << "gauss_row size: " << mRing.resGausser().size(gauss_row) <<
  //  std::endl;

  for (long i = 0; i < mSPairs.size(); i++)
    {
#ifdef DEBUG_GAUSS
      std::cout << "reducing row " << i << std::endl;
#endif
      // Reduce spair #i
      // fill in dense row with this element.

      ResPolynomialConstructor result(mRing);

      Row& r = mSPairs[i];  // row to be reduced.
      long comp = mSPairComponents[i];
      result.appendMonicTerm(mFrame.level(mThisLevel)[comp].mMonom);

      auto& syz = mFrame.level(mThisLevel)[comp]
                      .mSyzygy;  // this is the element we will fill out

      // Note: in the polynomial ring case, the row r is non-zero.
      // BUT: for skew commuting variables, it can happen that r is zero
      // (e.g. a.(acd<0>) = 0).  In this case we have nothing to reduce.
      if (!r.mComponents.empty())
        {
          ComponentIndex firstcol = r.mComponents[0];
          ComponentIndex lastcol = static_cast<ComponentIndex>(
              mColumns.size() -
              1);  // maybe: r.mComponents[r.mComponents.size()-1];

#ifdef DEBUG_GAUSS
          std::cout << "about to fill from sparse " << i << std::endl;
#endif

          mRing.resGausser().fillFromSparse(
              gauss_row,
              static_cast<ComponentIndex>(r.mComponents.size()),
              r.mCoeffs,
              &r.mComponents[0]);  // FIX: not correct call

          while (firstcol <= lastcol)
            {
#ifdef DEBUG_GAUSS
              std::cout << "about to reduce with col " << firstcol << std::endl;
              std::cout << "gauss_row: "
                        << (gauss_row.isNull() ? "null" : "not-null")
                        << std::endl;
              std::cout << "mReducers[" << firstcol << "]: "
                        << (mReducers[firstcol].mCoeffs.isNull() ? "null"
                                                                 : "not-null")
                        << std::endl;
              std::cout << "result: " << (result.coefficientInserter().isNull()
                                              ? "null"
                                              : "not-null")
                        << std::endl;
              std::cout << "  dense: ";
              mRing.resGausser().debugDisplay(std::cout, gauss_row)
                  << std::endl;
              mRing.resGausser().debugDisplay(std::cout,
                                              mReducers[firstcol].mCoeffs);
              std::cout << std::endl;
              mRing.resGausser().debugDisplay(std::cout,
                                              result.coefficientInserter());
              std::cout << std::endl;
#endif

              if (onlyConstantMaps and not track[firstcol])
                {
                  mRing.resGausser().sparseCancel(
                      gauss_row,
                      mReducers[firstcol].mCoeffs,
                      mReducers[firstcol].mComponents.data());
                }
              else
                {
                  mRing.resGausser().sparseCancel(
                      gauss_row,
                      mReducers[firstcol].mCoeffs,
                      mReducers[firstcol].mComponents.data(),
                      result.coefficientInserter());

#ifdef DEBUG_GAUSS
                  std::cout << "  done with sparseCancel" << std::endl;
                  mRing.resGausser().debugDisplay(std::cout, gauss_row)
                      << std::endl;
                  mRing.resGausser().debugDisplay(std::cout,
                                                  mReducers[firstcol].mCoeffs)
                      << std::endl;
                  mRing.resGausser().debugDisplay(std::cout,
                                                  result.coefficientInserter())
                      << std::endl;
                  std::cout << "  about to push back term" << std::endl;
#endif

                  result.pushBackTerm(mReducers[firstcol].mLeadTerm);

#ifdef DEBUG_GAUSS
                  std::cout << "done with col " << firstcol << std::endl;
#endif
                }
              firstcol = mRing.resGausser().nextNonzero(
                  gauss_row, firstcol + 1, lastcol);
            }
        }
#ifdef DEBUG_GAUSS
      std::cout << "about to set syz" << std::endl;
#endif
      result.setPoly(syz);
#ifdef DEBUG_GAUSS
      std::cout << "just set syz" << std::endl;
#endif
    }
  mRing.resGausser().deallocate(gauss_row);
}

void F4Res::construct(int lev, int degree)
{
  decltype(timer()) timeA, timeB;

  resetMatrix(lev, degree);

  timeA = timer();
  makeMatrix();
  timeB = timer();
  mFrame.timeMakeMatrix += seconds(timeB - timeA);

  if (M2_gbTrace >= 2) mHashTable.dump();

  if (M2_gbTrace >= 2)
    std::cout << "  make matrix time: " << seconds(timeB - timeA) << " sec"
              << std::endl;

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
    std::cout << "  (degree,level)=(" << (mThisDegree - mThisLevel) << ","
              << mThisLevel << ") #spairs=" << mSPairs.size()
              << " reducer= " << mReducers.size() << " x " << mReducers.size()
              << std::endl;

  if (M2_gbTrace >= 2) std::cout << "  gauss reduce matrix" << std::endl;

  timeA = timer();
  gaussReduce();
  timeB = timer();
  mFrame.timeGaussMatrix += seconds(timeB - timeA);

  if (M2_gbTrace >= 2)
    std::cout << "    time: " << seconds(timeB - timeA) << " sec" << std::endl;
  //  mFrame.show(-1);

  timeA = timer();
  clearMatrix();
  timeB = timer();
  mFrame.timeClearMatrix += seconds(timeB - timeA);
}

void F4Res::debugOutputReducers()
{
  std::cout << "-- reducers(rows) -- " << std::endl;
  auto end = mReducers.cend();
  for (auto i = mReducers.cbegin(); i != end; ++i)
    {
      monoid().showAlpha((*i).mLeadTerm);
      std::cout << std::endl;
    }
}
void F4Res::debugOutputColumns()
{
  std::cout << "-- columns --" << std::endl;
  auto end = mColumns.cend();
  for (auto i = mColumns.cbegin(); i != end; ++i)
    {
      monoid().showAlpha((*i));
      std::cout << std::endl;
    }
}

void F4Res::debugOutputMatrixSparse(std::vector<Row>& rows)
{
  for (ComponentIndex i = 0; i < rows.size(); i++)
    {
      std::cout << "coeffs[" << i << "] = ";
      mRing.resGausser().debugDisplay(std::cout, rows[i].mCoeffs);
      std::cout << " comps = ";
      for (long j = 0; j < rows[i].mComponents.size(); ++j)
        std::cout << rows[i].mComponents[j] << " ";
      std::cout << std::endl;
    }
}

void F4Res::debugOutputMatrix(std::vector<Row>& rows)
{
  for (ComponentIndex i = 0; i < rows.size(); i++)
    {
      mRing.resGausser().debugDisplayRow(std::cout,
                                         static_cast<int>(mColumns.size()),
                                         rows[i].mComponents,
                                         rows[i].mCoeffs);
    }
}
void F4Res::debugOutputReducerMatrix() { debugOutputMatrix(mReducers); }
void F4Res::debugOutputSPairMatrix() { debugOutputMatrix(mSPairs); }

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
