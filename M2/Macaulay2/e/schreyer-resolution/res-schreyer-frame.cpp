// Copyright 2014-2016 Michael E. Stillman

#include "schreyer-resolution/res-schreyer-frame.hpp"
#include "error.h"                                        // for ERROR
#include "f4/moninfo.hpp"                                 // for monomial_word
#include "interface/computation.h"                        // for StopConditions
#include "schreyer-resolution/res-f4.hpp"                 // for F4Res
#include "schreyer-resolution/res-f4-monlookup.hpp"       // for ResF4Monomi...
#include "schreyer-resolution/res-gausser.hpp"            // for Coefficient...
#include "schreyer-resolution/res-varpower-monomial.hpp"  // for res_varpowe...
#include "style.hpp"                                      // for LT, GT
#include "timing.hpp"                                     // for timer, seconds

#include <cassert>                                        // for assert
#include <cstdint>                                        // for int32_t
#include <algorithm>                                      // for stable_sort
#include <chrono>                                         // for common_type...
#include <iomanip>                                        // for operator<<
#include <iostream>                                       // for operator<<
#include <type_traits>                                    // for swap

namespace {
class PreElementSorter
{
 public:
  typedef SchreyerFrameTypes::PreElement* value;

 private:
  static long ncmps;

 public:
  int compare(value a, value b)
  {
    ncmps++;
    if (a->degree > b->degree) return GT;
    if (a->degree < b->degree) return LT;
    return res_varpower_monomials::compare(a->vp, b->vp);
  }

  bool operator()(value a, value b)
  {
    ncmps++;
    if (a->degree > b->degree) return false;
    if (a->degree < b->degree) return true;
    return res_varpower_monomials::compare(a->vp, b->vp) == LT;
  }

  PreElementSorter() {}
  void reset_ncomparisons() { ncmps = 0; }
  long ncomparisons() const { return ncmps; }
  ~PreElementSorter() {}
};

long PreElementSorter::ncmps = 0;
};

SchreyerFrame::SchreyerFrame(const ResPolyRing& R, int max_level)
    : mRing(R),
      mState(Initializing),
      mCurrentLevel(0),
      mSlantedDegree(0),
      mLoSlantedDegree(0),
      mHiSlantedDegree(0),
      mComputer(new F4Res(*this))
{
  mFrame.mLevels.resize(max_level + 1);
  mMaxVPSize = 2 * monoid().n_vars() + 1;

  timeMakeMatrix = 0.0;
  timeSortMatrix = 0.0;
  timeReorderMatrix = 0.0;
  timeGaussMatrix = 0.0;
  timeClearMatrix = 0.0;
  timeResetHashTable = 0.0;
  timeComputeRanks = 0.0;
  timeComputeSparseRanks = 0.0;
}

// Destruct the frame
SchreyerFrame::~SchreyerFrame()
{
  // Nothing to do here yet

  // the monomial block will free itself
  // as will the std::vector's

  // NO!! what about the frame polynomials??
}

// Computes the entire frame, unless interrupted.
// Returns true if whole frame has been completed, otherwise returns false.
bool SchreyerFrame::computeFrame()
{
  if (mState == Initializing)
    {
      std::cout << "error: calling computeFrame too soon" << std::endl;
    }
  if (mState != Frame) return true;  // already computed

  // Uses mCurrentLevel
  while (mCurrentLevel < mFrame.mLevels.size())
    {
      if (M2_gbTrace >= 1)
        std::cout << "maxsize = " << mFrame.mLevels.size()
                  << " and mCurrentLevel = " << mCurrentLevel << std::endl;
      if (computeNextLevel() == 0) break;  // increments mCurrentLevel
      //      if (interrupted) return false;
    }
  //  show(-1);
  // Now change the state of the computation

  mState = Matrices;
  mCurrentLevel = 2;
  getBounds(mLoSlantedDegree, mHiSlantedDegree, mMaxLength);
  mSlantedDegree = mLoSlantedDegree;
  setBettiDisplays();  // Also sets mMinimalizeTODO
  if (M2_gbTrace >= 1)
    {
      std::cout << "non-minimal betti: " << std::endl;
      mBettiNonminimal.output();
    }

  // for (int i=0; i<mMinimalizeTODO.size(); i++)
  //  {
  //     auto a = mMinimalizeTODO[i];
  //     std::cout << "(" << a.first << "," << a.second << ") ";
  //  }
  // std::cout << std::endl;
  return true;
}
BettiDisplay SchreyerFrame::minimalBettiNumbers(bool stop_after_degree,
                                                int top_slanted_degree,
                                                int length_limit)
{
  // The lo degree will be: mLoSlantedDegree.
  // The highest slanted degree will either be mHiSlantedDegree, or
  // top_slanted_degree (minimum of these two).
  // The length we need to compute to is either maxLevel(), or length_limit+1.
  // We set maxlevel to length_limit.  We insist that length_limit <= maxLevel()
  // - 2.
  // Here is what needs to be computed:
  //  lo: . . . . . . .
  //      . . . . . . .
  /// hi: . . . . . .
  // Each dot in all rows other than 'hi' needs to have syzygies computed for
  // it.
  // if hi == mHiSlantedDegree, then we do NOT need to compute syzygies in this
  // last row.
  //   else we need to compute syzygies in these rows, EXCEPT not at level
  //   maxlevel+1

  computeFrame();

  int top_degree;  // slanted degree
  if (stop_after_degree)
    {
      top_degree = std::min(top_slanted_degree, mHiSlantedDegree);
      top_degree = std::max(mLoSlantedDegree, top_degree);
    }
  else
    {
      top_degree = mHiSlantedDegree;
    }
  // First: if length_limit is too low, extend the Frame
  if (length_limit >= maxLevel())
    {
      std::cout << "WARNING: cannot extend resolution length" << std::endl;
      length_limit = maxLevel() - 1;
      // Extend the length of the Frame, change mMaxLength, possibly
      // mHiSlantedDegree
      // increase mComputationStatus if needed, mMinimalBetti, ...
      // computeFrame()
    }

  // What needs to be computed?
  // lodeg..hideg, level: 0..maxlevel.  Note: need to compute at level
  // maxlevel+1 in order to get min betti numbers at
  //   level maxlevel.
  // Also note: if hideg is the highest degree that occurs in the frame, we do
  // not need to compute any matrices for these.

  for (int deg = mLoSlantedDegree; deg <= top_degree - 1; deg++)
    for (int lev = 1; lev <= length_limit + 1; lev++)
      {
        computeRank(deg, lev);
      }

  for (int lev = 1; lev <= length_limit; lev++)
    {
      computeRank(top_degree, lev);
    }

  if (M2_gbTrace >= 1)
    {
      std::cout << "displaying stats" << std::endl;
      showMemoryUsage();
      monoid().show();
      std::cout << "total setPoly: " << ResPolynomialConstructor::ncalls << std::endl;
      std::cout << "total setPolyFromArray: "
                << ResPolynomialConstructor::ncalls_fromarray << std::endl;
      std::cout << "total ~ResPolynomial: " << ResPolynomial::npoly_destructor << std::endl;

      std::cout << "total time for make matrix: " << timeMakeMatrix
                << std::endl;
      std::cout << "total time for sort matrix: " << timeSortMatrix
                << std::endl;
      std::cout << "total time for reorder matrix: " << timeReorderMatrix
                << std::endl;
      std::cout << "total time for gauss matrix: " << timeGaussMatrix
                << std::endl;
      std::cout << "total time for clear matrix: " << timeClearMatrix
                << std::endl;
      std::cout << "total time for reset hash table: " << timeResetHashTable
                << std::endl;
      std::cout << "total time for computing ranks: " << timeComputeRanks
                << std::endl;
      std::cout << "total time for computing sparse ranks: " << timeComputeSparseRanks
                << std::endl;
    }

  BettiDisplay B(mBettiMinimal);  // copy
  B.resize(mLoSlantedDegree, top_degree, length_limit);

  return B;
}

void SchreyerFrame::start_computation(StopConditions& stop)
{
  // This is the computation of the non-minimal maps themselves
  decltype(timer()) timeA, timeB;
  //  if (level(0).size() == 0)
  //    mState = Done;;
  computeFrame();
  if (M2_gbTrace >= 1)
    {
      std::cout << "computation status after computing frame: " << std::endl;
      mComputationStatus.output();
    }

  int top_slanted_degree = mHiSlantedDegree;
  if (stop.stop_after_degree and mHiSlantedDegree > stop.degree_limit->array[0])
    top_slanted_degree = stop.degree_limit->array[0];

  computeSyzygies(top_slanted_degree, mMaxLength);

  if (M2_gbTrace >= 1)
    {
      showMemoryUsage();
      std::cout << "total time for make matrix: " << timeMakeMatrix
                << std::endl;
      std::cout << "total time for sort matrix: " << timeSortMatrix
                << std::endl;
      std::cout << "total time for reorder matrix: " << timeReorderMatrix
                << std::endl;
      std::cout << "total time for gauss matrix: " << timeGaussMatrix
                << std::endl;
      std::cout << "total time for clear matrix: " << timeClearMatrix
                << std::endl;
      std::cout << "total time for reset hash table: " << timeResetHashTable
                << std::endl;
      std::cout << "total time for computing ranks: " << timeComputeRanks
                << std::endl;
      std::cout << "total time for computing sparse ranks: " << timeComputeSparseRanks
                << std::endl;
    }

  return;
#if 0  
  if (M2_gbTrace >= 1)
    {
      std::cout << "computation status after computing syzygies: " << std::endl;
      mComputationStatus.output();
    }
  timeA = timer();
  computeRanks(mHiSlantedDegree, mMaxLength);
  timeB = timer();
  timeComputeRanks += seconds(timeB-timeA);
  if (M2_gbTrace >= 1)
    {
      std::cout << "computation status after computing ranks: " << std::endl;
      mComputationStatus.output();
    }


  // This next part needs to be computed after the frame, as otherwise mHiSlantedDegree isn't yet set.
  int top_slanted_degree = 0;

  top_slanted_degree = mHiSlantedDegree;
  if (stop.stop_after_degree and mHiSlantedDegree > stop.degree_limit->array[0])
    top_slanted_degree = stop.degree_limit->array[0];

  while (true)
    {
      switch (mState) {
      case Initializing:
        break;
      case Frame:
        std::cerr << "ERROR: should not get to this point anymore..." << std::endl;
        if (M2_gbTrace >= 1)
          std::cout << "maxsize = " << mFrame.mLevels.size() << " and mCurrentLevel = " << mCurrentLevel << std::endl;
        if (mCurrentLevel >= mFrame.mLevels.size() or computeNextLevel() == 0)
          {
            //show(6);
            mState = Matrices;
            mCurrentLevel = 2;
            getBounds(mLoSlantedDegree, mHiSlantedDegree, mMaxLength);
            mSlantedDegree = mLoSlantedDegree;
            setBettiDisplays();
            if (M2_gbTrace >= 1)
              {
                std::cout << "non-minimal betti: " << std::endl;
                mBettiNonminimal.output();
              }
            //for (int i=0; i<mMinimalizeTODO.size(); i++)
            //  {
            //     auto a = mMinimalizeTODO[i];
            //     std::cout << "(" << a.first << "," << a.second << ") ";
            //  }
            // std::cout << std::endl;
          }
        break;
      case Matrices:
        if (M2_gbTrace >= 1)
          std::cout << "start_computation: entering matrices(" << mSlantedDegree << ", " << mCurrentLevel << ")" << std::endl;
        if (stop.always_stop) return;
        
        if (mCurrentLevel > mMaxLength)
          {
            mCurrentLevel = 2;
            mSlantedDegree++;
            if (mSlantedDegree > top_slanted_degree)
              {
                if (M2_gbTrace >= 1)
                  showMemoryUsage();
#if 0                
                debugCheckOrderAll();
#endif
                timeA = timer();
                for (auto it=mMinimalizeTODO.cbegin(); it != mMinimalizeTODO.cend(); ++it)
                  {
                    int rk = rank(it->first, it->second);
                    mBettiMinimal.entry(it->first, it->second) -= rk;
                    mBettiMinimal.entry(it->first+1, it->second-1) -= rk;
                  }
                timeB = timer();
                timeComputeRanks += seconds(timeB-timeA);
                mState = Done;
                if (M2_gbTrace >= 1)
                  mBettiMinimal.output();
                 break;
              }
            //            if (stop.stop_after_degree and mSlantedDegree > stop.degree_limit->array[0])
            //              return;
          }
        if (M2_gbTrace >= 2)
          {
            std::cout << "construct(" << mSlantedDegree << ", " << mCurrentLevel << ")..." << std::flush;
          }
        mComputer->construct(mCurrentLevel, mSlantedDegree+mCurrentLevel);
        if (M2_gbTrace >= 2)
          {
            std::cout << "done" << std::endl;
          }
        ///std::cout << "Number of distinct monomials so far = " << mAllMonomials.count() << std::endl;
        mCurrentLevel++;
        break;
      case Done:
        if (M2_gbTrace >= 1)
          {
            std::cout << "total time for make matrix: " << timeMakeMatrix << std::endl;
            std::cout << "total time for sort matrix: " << timeSortMatrix << std::endl;
            std::cout << "total time for reorder matrix: " << timeReorderMatrix << std::endl;
            std::cout << "total time for gauss matrix: " << timeGaussMatrix << std::endl;
            std::cout << "total time for clear matrix: " << timeClearMatrix << std::endl;
            std::cout << "total time for reset hash table: " << timeResetHashTable << std::endl; 
            std::cout << "total time for computing ranks: " << timeComputeRanks << std::endl;
          }
        return;
      default:
        break;
      }
    }
#endif
}

M2_arrayint SchreyerFrame::getBetti(int type)
{
  if (type == 4)
    {
      computeFrame();
      decltype(timer()) timeA, timeB;
      timeA = timer();
      computeRanks(mHiSlantedDegree, maxLevel());
      timeB = timer();
      timeComputeRanks += seconds(timeB - timeA);

      return mBettiMinimal.getBetti();
    }
  if (type == 0 or type == 1) return getBettiFrame();
  if (type == 5) return mComputationStatus.getBetti();

  ERROR("betti display not implemenented yet");
  return 0;
}

void SchreyerFrame::endLevel()
{
  setSchreyerOrder(mCurrentLevel);
  mCurrentLevel++;
  if (mCurrentLevel == 2)
    {
      mState = Frame;
    }
}

SchreyerFrame::PreElement* SchreyerFrame::createQuotientElement(
    res_packed_monomial m1,
    res_packed_monomial m)
{
  PreElement* vp = mPreElements.allocate();
  vp->vp = mVarpowers.reserve(mMaxVPSize);
  monoid().quotient_as_vp(m1, m, vp->vp);
  vp->degree = monoid().degree_of_vp(vp->vp);
  int len = static_cast<int>(res_varpower_monomials::length(vp->vp));
  mVarpowers.intern(len);
  return vp;
}
component_index SchreyerFrame::computeIdealQuotient(int lev,
                                                    component_index begin,
                                                    component_index elem)
{
  ///  std::cout << "computeIdealQuotient(" << lev << "," << begin << "," <<
  ///  elem << ")" << std::endl;
  // Returns the number of elements added
  res_packed_monomial m = monomial(lev, elem);
  std::vector<PreElement*> elements;
  if (ring().isSkewCommutative())
    {
      auto skewvars = new int[ring().monoid().n_vars()];
      int a = ring().monoid().skew_vars(ring().skewInfo(), m, skewvars);
      // std::cout << "adding " << a << " syz from skew" << std::endl;
      for (int i = 0; i < a; ++i)
        {
          PreElement* vp = mPreElements.allocate();
          vp->vp = mVarpowers.reserve(mMaxVPSize);
          monoid().variable_as_vp(skewvars[i], vp->vp);
          vp->degree = monoid().degree_of_vp(vp->vp);
          int len = static_cast<int>(res_varpower_monomials::length(vp->vp));
          mVarpowers.intern(len);

          elements.push_back(vp);
        }
      delete[] skewvars;
    }
  for (component_index i = begin; i < elem; i++)
    elements.push_back(createQuotientElement(monomial(lev, i), m));
  typedef ResF4MonomialLookupTableT<int32_t> MonomialLookupTable;
  MonomialLookupTable montab(monoid().n_vars());

#if 0
  std::cout << "  #pre elements = " << elements.size() << std::endl;
  for (auto i=elements.begin(); i != elements.end(); ++i)
    {
      varpower_monomials::elem_text_out(stdout, (*i)->vp);
      fprintf(stdout, "\n");
    }
#endif
  PreElementSorter C;
  std::stable_sort(elements.begin(), elements.end(), C);

  component_index n_elems = 0;
  for (auto i = elements.begin(); i != elements.end(); ++i)
    {
      int32_t not_used;
      bool inideal = montab.find_one_divisor_vp(0, (*i)->vp, not_used);
      if (inideal) continue;
      // Now we create a res_packed_monomial, and insert it into 'lev+1'
      montab.insert_minimal_vp(0, (*i)->vp, 0);
      res_packed_monomial monom =
          monomialBlock().allocate(monoid().max_monomial_size());
      monoid().from_varpower_monomial((*i)->vp, elem, monom);
      // Now insert it into the frame
      insertBasic(currentLevel(),
                  monom,
                  (*i)->degree + degree(currentLevel() - 1,
                                        monoid().get_component(monom)));
      n_elems++;
    }
  // std::cout << "  returns " << n_elems << std::endl;
  return n_elems;
}

component_index SchreyerFrame::computeNextLevel()
{
  if (currentLevel() == 1) return 0;
  if (currentLevel() >= mFrame.mLevels.size()) return 0;
  //  std::cout << "computeNextLevel: level = " << currentLevel() << std::endl;
  // loop through all the elements at level currentLevel()-2
  int level0 = currentLevel() - 2;
  int level1 = level0 + 1;
  component_index n_elems_added = 0;
  for (auto i = level(level0).begin(); i != level(level0).end(); ++i)
    {
      component_index begin = (*i).mBegin;
      component_index end = (*i).mEnd;
      for (component_index i = begin; i < end; ++i)
        {
          auto& elem = level(level1)[i];
          elem.mBegin = n_elems_added;
          n_elems_added += computeIdealQuotient(level1, begin, i);
          elem.mEnd = n_elems_added;
        }
    }
  setSchreyerOrder(mCurrentLevel);
  mCurrentLevel++;

  return n_elems_added;
}

void SchreyerFrame::setSchreyerOrder(int lev)
{
  auto& myframe = level(lev);
  auto& myorder = schreyerOrder(lev);
  myorder.mTieBreaker.resize(myframe.size());
  //  std::cout << "setSchreyerOrder: entering, lev=" << lev << ", nelems=" <<
  //  myframe.size() << std::endl;
  if (lev == 0)
    {
      for (component_index i = 0; i < myorder.mTieBreaker.size(); i++)
        myorder.mTieBreaker[i] = i;
      return;
    }

  auto& prevorder = schreyerOrder(lev - 1);
  long* tiebreakers = new long[myframe.size()];

  auto n_frame_elems = myframe.size();
  for (component_index i = 0; i < n_frame_elems; i++)
    {
      component_index comp = monoid().get_component(myframe[i].mMonom);
      tiebreakers[i] = i + n_frame_elems * prevorder.mTieBreaker[comp];
    }
  std::stable_sort(tiebreakers, tiebreakers + n_frame_elems);

  for (component_index i = 0; i < n_frame_elems; i++)
    {
      myorder.mTieBreaker[tiebreakers[i] % n_frame_elems] = i;
    }
  delete[] tiebreakers;
  //  std::cout << "  setSchreyerOrder: exiting, lev=" << lev << std::endl;
}

void SchreyerFrame::insertBasic(int lev, res_packed_monomial monom, int degree)
{
  assert(lev >= 1);  // Should not be called with lev==0.
  // if lev >= 2, then level(lev-1)[comp].(mBegin,mEnd) is set separately.
  auto& myframe = level(lev);
  long idx = myframe.size();
  myframe.emplace_back(FrameElement(monom, degree));
  auto& myelem = myframe[idx];
  myelem.mSyzygy.coeffs = gausser().allocateCoefficientVector();
  // The rest of this code simply sets the total monomial for the Schreyer order
  // and should be moved out of here. (MES 3 Feb 2016)
  auto& myorder = schreyerOrder(lev);
  auto myTotalMonom = monomialBlock().allocate(monoid().max_monomial_size());
  auto& prevorder = schreyerOrder(lev - 1);
  component_index comp = monoid().get_component(myelem.mMonom);
  monoid().unchecked_mult(
      myelem.mMonom, prevorder.mTotalMonom[comp], myTotalMonom);
  monoid().set_component(monoid().get_component(prevorder.mTotalMonom[comp]),
                         myTotalMonom);
  myorder.mTotalMonom.push_back(myTotalMonom);
}

void SchreyerFrame::insertLevelZero(res_packed_monomial monom,
                                    int degree,
                                    int maxdeglevel0)
{
  auto& myframe = level(0);
  long idx = myframe.size();
  myframe.emplace_back(FrameElement(monom, degree));
  auto& myelem = myframe[idx];

  auto& myorder = schreyerOrder(0);
  auto myTotalMonom =
      monomialBlock().allocate(monoid().monomial_size(myelem.mMonom));
  // Create the total monomial.  It is monom * (firstvar)^(maxdeglevel0-degree)
  monoid().copy(myelem.mMonom, myTotalMonom);
  myorder.mTotalMonom.push_back(myTotalMonom);
}
bool SchreyerFrame::insertLevelOne(res_packed_monomial monom,
                                   int deg,
                                   ResPolynomial& syzygy)
{
  insertBasic(1, monom, deg);  // deg is the actual degree of this element.
  component_index comp = monoid().get_component(monom);
  auto last = static_cast<component_index>(level(1).size());
  auto& p = level(0)[comp];
  if (p.mBegin == -1) p.mBegin = last - 1;
  p.mEnd = last;
  if (!check_poly(ring(), syzygy, schreyerOrder(0)))
    {
      if (M2_gbTrace >= 1)
        {
          std::cout
              << "Error: expected terms of polynomial to be in order, in poly#"
              << last << ": ";
          display_poly(std::cout, ring(), syzygy);
          std::cout << std::endl;
        }
      return false;
    }
  std::swap(level(1)[level(1).size() - 1].mSyzygy, syzygy);
  return true;
}

bool SchreyerFrame::debugCheckOrder(int lev) const
{
  if (lev == 0) return true;
  bool result = true;
  auto& mylevel = level(lev);
  auto& myorder = schreyerOrder(lev - 1);
  int which = 0;
  for (auto i = mylevel.cbegin(); i != mylevel.cend(); ++i, ++which)
    {
      if (!check_poly(ring(), i->mSyzygy, myorder))
        {
          std::cout << "Error: terms of polynomial at level " << lev
                    << " location " << which << " not in order" << std::endl;
          std::cout << "  poly = ";
          display_poly(std::cout, ring(), i->mSyzygy);
          std::cout << std::endl;
          result = false;
        }
    }
  return result;
}
bool SchreyerFrame::debugCheckOrderAll() const
{
  std::cout
      << "checking that all input and constructed polynomials are in order...";
  bool result = true;
  for (auto i = 1; i < maxLevel(); ++i)
    if (!debugCheckOrder(i)) result = false;
  if (result) std::cout << "ok" << std::endl;
  return result;
}

long SchreyerFrame::memoryUsage() const
{
  long result = mMonomialSpace.memoryUsage();
  for (int i = 0; i < mFrame.mLevels.size(); i++)
    {
      result += level(i).capacity() * sizeof(FrameElement);
    }
  return result;
}

void SchreyerFrame::showMemoryUsage() const
{
  std::cout << "Frame memory usage" << std::endl;
  // widths: level: 6, #elems: 8, used: 6, allocated: 11
  std::cout << " level"
            << "   #elems"
            << "   used"
            << "   allocated"
            << "     nterms"
            << "       poly"
            << "   polalloc" << std::endl;
  long alloc = 0;
  long used = 0;
  long nelems = 0;
  long poly_used = 0;
  long poly_alloc = 0;
  long poly_nterms = 0;
  long poly_used_level = 0;
  long poly_alloc_level = 0;
  long poly_nterms_level = 0;
  for (int i = 0; i < mFrame.mLevels.size(); i++)
    {
      long nelems_level = level(i).size();
      if (nelems_level == 0) continue;
      long used_level = nelems_level * sizeof(FrameElement);
      long alloc_level = level(i).capacity() * sizeof(FrameElement);
      poly_nterms_level = 0;
      poly_used_level = 0;
      poly_alloc_level = 0;
      for (int j = 0; j < nelems_level; j++)
        {
          ring().memUsage(level(i)[j].mSyzygy,
                          poly_nterms_level,
                          poly_used_level,
                          poly_alloc_level);
        }
      poly_nterms += poly_nterms_level;
      poly_used += poly_used_level;
      poly_alloc += poly_alloc_level;
      std::cout << std::setw(6) << i << " " << std::setw(8) << nelems_level
                << " " << std::setw(6) << used_level << " " << std::setw(11)
                << alloc_level << " " << std::setw(10) << poly_nterms_level
                << " " << std::setw(10) << poly_used_level << " "
                << std::setw(10) << poly_alloc_level << std::endl;
      nelems += nelems_level;
      used += used_level;
      alloc += alloc_level;
    }
  std::cout << "   all"
            << " " << std::setw(8) << nelems << " " << std::setw(6) << used
            << " " << std::setw(11) << alloc << " " << std::setw(10)
            << poly_nterms << " " << std::setw(10) << poly_used << " "
            << std::setw(10) << poly_alloc << std::endl;

  long monomSpace = mMonomialSpace.memoryUsage();
  long monomUsed =
      nelems * monoid().max_monomial_size() * sizeof(monomial_word);
  std::cout << "monomials     " << std::setw(6) << monomUsed << " "
            << std::setw(11) << monomSpace << std::endl;
  std::cout << "total mem     " << std::setw(6)
            << (used + monomUsed + poly_used) << " " << std::setw(11)
            << (alloc + monomSpace + poly_alloc) << std::endl;
}

void SchreyerFrame::show(int len) const
{
  std::cout << "#levels=" << mFrame.mLevels.size()
            << " currentLevel=" << currentLevel() << std::endl;
  for (int i = 0; i < mFrame.mLevels.size(); i++)
    {
      auto& myframe = level(i);
      auto& myorder = schreyerOrder(i);
      if (myframe.size() == 0) continue;
      std::cout << "--- level " << i << " ------" << std::endl;
      for (int j = 0; j < myframe.size(); j++)
        {
          std::cout << "    " << j << " " << myframe[j].mDegree << " ("
                    << myframe[j].mBegin << "," << myframe[j].mEnd << ") "
                    << std::flush;
          if (myframe[j].mSyzygy.coeffs.isNull())
            std::cout << "coeffs=null " << std::flush;
          std::cout << "(size:" << myframe[j].mSyzygy.len << ") [";
          monoid().showAlpha(myorder.mTotalMonom[j]);
          std::cout << "  " << myorder.mTieBreaker[j] << "] ";
          if (len == 0 or myframe[j].mSyzygy.len == 0)
            monoid().showAlpha(myframe[j].mMonom);
          else
            {
              display_poly(std::cout, ring(), myframe[j].mSyzygy);
            }
          std::cout << std::endl;
        }
    }
  showMemoryUsage();
}

void SchreyerFrame::getBounds(int& loDegree, int& hiDegree, int& length) const
{
  if (mFrame.mLevels.size() == 0 or mFrame.mLevels[0].mElements.size() == 0)
    {
      loDegree = 0;
      hiDegree = -1;
      length = 0;
      return;
    }
  auto& lev0 = level(0);
  loDegree = hiDegree = static_cast<int>(lev0[0].mDegree);
  for (int lev = 0; lev < mFrame.mLevels.size(); lev++)
    {
      auto& myframe = level(lev);
      if (myframe.size() == 0) return;
      length = lev;
      for (auto p = myframe.begin(); p != myframe.end(); ++p)
        {
          int deg = p->mDegree;
          deg -= lev;  // slanted degree
          if (deg < loDegree) loDegree = deg;
          if (deg > hiDegree) hiDegree = deg;
        }
    }
  //  show();
}

void SchreyerFrame::setBettiDisplays()
{
  int lo, hi, len;
  getBounds(lo, hi, len);
  // std::cout << "bounds: lo=" << lo << " hi=" << hi << " len=" << len <<
  // std::endl;
  mBettiNonminimal = BettiDisplay(lo, hi, len);
  mBettiMinimal = BettiDisplay(lo, hi, len);
  mComputationStatus = BettiDisplay(lo, hi, maxLevel());

  for (int lev = 0; lev <= len; lev++)
    {
      auto& myframe = level(lev);
      for (auto p = myframe.begin(); p != myframe.end(); ++p)
        {
          int deg = p->mDegree;  // this is actual degree, not slanted degree
          mBettiNonminimal.entry(deg - lev, lev)++;
          mBettiMinimal.entry(deg - lev, lev)++;
        }
    }

#if 0  
  // Now set the todo list of pairs (degree, level) for minimalization.
  for (int slanted_degree = lo; slanted_degree < hi; slanted_degree++)
    {
      for (int lev = 1; lev <= maxLevel()-1; lev++)
        {
          if (mBettiNonminimal.entry(slanted_degree, lev) > 0 and mBettiNonminimal.entry(slanted_degree+1, lev-1) > 0)
            {
              mMinimalizeTODO.push_back(std::make_pair(slanted_degree, lev));
            }
          
        }
    }
#endif
  // Meaning: 0: no syzygies in that (degree,lev)
  //          1:  there are some, but syzygies have not been constructed yet
  //          2:  syzygies have been constructed
  //          3:  syzygies have been constructed AND rank from (deg,lev) to
  //          (deg+1,lev-1) has been
  //              computed, and the ranks taken into account in mMinimalBetti.
  for (int slanted_degree = lo; slanted_degree <= hi; slanted_degree++)
    {
      if (len >= 0)
        {
          if (mBettiNonminimal.entry(slanted_degree, 0) == 0)
            mComputationStatus.entry(slanted_degree, 0) = 0;
          else
            mComputationStatus.entry(slanted_degree, 0) = 3;
        }

      if (len >= 1)
        {
          if (mBettiNonminimal.entry(slanted_degree, 1) == 0)
            mComputationStatus.entry(slanted_degree, 1) = 0;
          else
            mComputationStatus.entry(slanted_degree, 1) = 2;
        }

      for (int lev = 2; lev <= maxLevel(); lev++)
        {
          if ((lev > len) or mBettiNonminimal.entry(slanted_degree, lev) == 0)
            mComputationStatus.entry(slanted_degree, lev) = 0;
          else
            mComputationStatus.entry(slanted_degree, lev) = 1;
        }
    }
}

void SchreyerFrame::computeSyzygies(int slanted_degree, int maxlevel)
{
  // Compute everything up to this point
  int toplevel = (maxlevel < maxLevel() ? maxlevel : maxLevel());
  for (int deg = mLoSlantedDegree; deg <= slanted_degree; deg++)
    for (int lev = 2; lev <= toplevel; lev++)
      {
        fillinSyzygies(deg, lev);
      }
  //  show(-1);
}
void SchreyerFrame::computeRanks(int slanted_degree, int maxlevel)
{
  // Compute all needed ranks to get the minimal Betti numbers in the range
  // deg <= slanted_degree, lev <=maxlevel.
  // This means: we need to compute ranks to level maxlevel+1 (or largest that
  // exists)
  // in degrees <= slanted_degree, EXCEPT we don't need to compute at
  // (slanted_degree,maxlevel+1).
  int toplevel = (maxlevel < maxLevel() ? maxlevel - 1 : mMaxLength);
  for (int deg = mLoSlantedDegree; deg <= slanted_degree; deg++)
    for (int lev = 1; lev <= toplevel; lev++) computeRank(deg, lev);
}
void SchreyerFrame::fillinSyzygies(int slanted_deg, int lev)
{
  // Fill in syzygies of slanted degree mSlantedDegree, at level mCurrentLevel =
  // 2.
  // Assumption/prereq:
  // Compute the matrix at this level, where lev >= 2. (lev=0,1 have already
  // been filled in).
  // Prereqs: fillin(i,lev-1) has been called, for all i <= slanted_degree.
  // WARNING: this is not currently checked or remembered.

  int& status = mComputationStatus.entry(slanted_deg, lev);
  if (status != 1) return;

  if (M2_gbTrace >= 2)
    {
      std::cout << "construct(" << slanted_deg << ", " << lev << ")..."
                << std::flush;
    }
  mComputer->construct(lev, slanted_deg + lev);
  status = 2;

  if (M2_gbTrace >= 2)
    {
      std::cout << "done" << std::endl;
      std::cout << "#additions so far: " << gausser().getNumAdditions()
                << std::endl;
    }
}
void SchreyerFrame::computeRank(int slanted_degree, int lev)
{
  //  std::cout << "computeRank(" << slanted_degree << "," << lev << ")" <<
  //  std::endl;
  int& status = mComputationStatus.entry(slanted_degree, lev);
  if (status == 0) return;  // Nothing here
  if (status == 1)
    {
      fillinSyzygies(slanted_degree, lev);
    }
  if (status == 3) return;  // already done
  int rk = rank(slanted_degree, lev);
  if (rk > 0)
    {
      mBettiMinimal.entry(slanted_degree, lev) -= rk;
      if (slanted_degree <= mHiSlantedDegree and lev > 0)
        mBettiMinimal.entry(slanted_degree + 1, lev - 1) -= rk;
    }
  status = 3;
}

M2_arrayint SchreyerFrame::getBettiFrame() const
{
  int lo, hi, len;
  getBounds(lo, hi, len);
  //  std::cout << "bounds: lo=" << lo << " hi=" << hi << " len=" << len <<
  //  std::endl;
  BettiDisplay B(lo, hi, len);
  // now set B

  for (int lev = 0; lev <= len; lev++)
    {
      auto& myframe = level(lev);
      for (auto p = myframe.begin(); p != myframe.end(); ++p)
        {
          int deg = p->mDegree;  // this is actual degree, not slanted degree
          B.entry(deg - lev, lev)++;
        }
    }

  return B.getBetti();
}

// local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
