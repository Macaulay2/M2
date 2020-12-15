// Copyright 2014-2015 Michael E. Stillman

// to do list
//  - display of "poly" elements in the resolution
//  - get_matrix: should get these elements (if they are there yet)
//  - CoefficientArray: need to be able to build one incrementally
//  - monomial lookup routine
//  - should monomials be varpowers?  or even lists of variables?
//  - make sure monomials are always constructed with that "extra space" for the
//  reducer number.

// res-f4-m2-interface: needs to be rewritten:
//   . don't use gc
//   . gb_array: 3 routines commented out, due to using these.
//   . need a display routine for Polynomial, also one that limits the number of
//   monomials displayed
// res-gausser
//   . don't use gc
//   . perhaps define CoefficientArray in this class, so it is easily changed
//   . don't yet use ARing stuff.
// res-f4-types.hpp
//   . has lots of junk (most already commented out)

#ifndef _res_schreyer_frame_hpp_
#define _res_schreyer_frame_hpp_

#include "res-schreyer-frame.hpp"

#include <vector>

#include "interface/computation.h"

#include "res-moninfo.hpp"
#include "res-memblock.hpp"
#include "res-varpower-monomial.hpp"
#include "res-poly-ring.hpp"
#include "res-schreyer-order.hpp"
#include "res-f4.hpp"
#include "monhashtable.hpp"

#include "../betti.hpp"

class F4Res;

typedef int ComponentIndex;  // index into f4 matrices over kk.  These tend to
                             // be larger, not sure if they
// will ever be > 2billion, but probably...

typedef int FieldElement;

namespace SchreyerFrameTypes {
struct FrameElement
{
  res_packed_monomial mMonom;  // has component, degree too
  //    res_packed_monomial mTotalMonom; // used for Schreyer order
  //    long mTiebreaker; // used for Schreyer order
  int mDegree;             // actual degree, not slanted degree
  component_index mBegin;  // points into next level's elements
  component_index mEnd;
  poly mSyzygy;
  FrameElement() {}
  FrameElement(res_packed_monomial monom)
      : mMonom(monom), mDegree(0), mBegin(-1), mEnd(-1)
  {
  }
  FrameElement(res_packed_monomial monom, int deg)
      : mMonom(monom), mDegree(deg), mBegin(-1), mEnd(-1)
  {
  }
};

struct PreElement
{
  res_varpower_monomial vp;
  int degree;
};
};

class SchreyerFrame
{
 public:
  friend class F4Res;
  typedef SchreyerFrameTypes::FrameElement FrameElement;
  typedef SchreyerFrameTypes::PreElement PreElement;

  // Construct an empty frame
  SchreyerFrame(const ResPolyRing& R, int max_level);

  // Destruct the frame
  ~SchreyerFrame();

  const ResMonoid& monoid() const { return mRing.monoid(); }
  const ResPolyRing& ring() const { return mRing; }
  const ResGausser& gausser() const { return mRing.resGausser(); }
  // This is where we place the monomials in the frame
  // This requires some care from people calling this function
  MemoryBlock<res_monomial_word>& monomialBlock() { return mMonomialSpace; }
  // Debugging, Memory info //
  void show(int len) const;  // len is how much of the polynomials to display
                             // (len=-1 means all, len=0 means just the frame)
  long memoryUsage() const;  // Return number of bytes in use.
  void showMemoryUsage() const;
  bool debugCheckOrder(int lev)
      const;  // make sure polynomials constructed, at level 'lev' are in order.
  bool debugCheckOrderAll() const;

  M2_arrayint getBetti(int type);  // will compute ranks of matrices, if needed.

  void getBounds(int& loDegree, int& hiDegree, int& length) const;

  void insertLevelZero(res_packed_monomial monom, int degree, int maxdeglevel0);

  bool insertLevelOne(res_packed_monomial monom,
                      int degree,
                      poly& syzygy);  // grabs syzygy
  // insertLevelOne: insert element.  If the elements are not in order, then
  // false is returned.

  void endLevel();  // done with the frame for the current level: set's the
                    // begin/end's
                    // for each element at previous level
  void start_computation(StopConditions& stop);

  void insertBasic(int lev, res_packed_monomial monom, int degree);

  void setSchreyerOrder(int lev);

  component_index computeNextLevel();  // returns # of new elements added

  res_packed_monomial monomial(int lev, component_index component)
  {
    return level(lev)[component].mMonom;
  }

  M2_arrayint getBettiFrame() const;
  void setBettiDisplays();
  int rank(int slanted_degree, int lev);  // rank of the degree 'degree' map of
                                          // scalars level 'lev' to 'lev-1'.
  //  int rankUsingSparseMatrix(int slanted_degree, int lev);

  template<typename Gen>
  int rankUsingSparseMatrix(Gen& D);

  template<typename Gen>
  int rankUsingDenseMatrix(Gen& D);
  
  
  bool computeFrame();  // returns true if the whole frame is created.  false if
                        // interrupted.

  void computeSyzygies(int slanted_degree, int maxlevel);
  void computeRanks(int slanted_degree, int maxlevel);

  BettiDisplay minimalBettiNumbers(bool stop_after_degree,
                                   int top_slanted_degree,
                                   int length_limit);

 private:
  struct Level
  {
    std::vector<FrameElement> mElements;
    ResSchreyerOrder mSchreyerOrder;
  };
  struct Frame
  {
    std::vector<Level> mLevels;
  };

  int currentLevel() const { return mCurrentLevel; }
  int degree(int lev, component_index component) const
  {
    return level(lev)[component].mDegree;
  }

 public:
  ResSchreyerOrder& schreyerOrder(int lev)
  {
    return mFrame.mLevels[lev].mSchreyerOrder;
  }
  const ResSchreyerOrder& schreyerOrder(int lev) const
  {
    return mFrame.mLevels[lev].mSchreyerOrder;
  }
  int maxLevel() const { return static_cast<int>(mFrame.mLevels.size() - 1); }
  std::vector<FrameElement>& level(int lev)
  {
    return mFrame.mLevels[lev].mElements;
  }
  const std::vector<FrameElement>& level(int lev) const
  {
    return mFrame.mLevels[lev].mElements;
  }

 private:
  void fillinSyzygies(int slanted_deg, int lev);
  void computeRank(int slanted_degree, int lev);

  //////////////////////////////////////////////
  // Private functions for frame construction //
  //////////////////////////////////////////////
  PreElement* createQuotientElement(res_packed_monomial m1,
                                    res_packed_monomial m);
  component_index computeIdealQuotient(int lev,
                                       component_index begin,
                                       component_index elem);
  component_index insertElements(int lev, component_index elem);

 private:
  ///////////////////
  // Private Data ///
  ///////////////////
  const ResPolyRing& mRing;
  Frame mFrame;
  MemoryBlock<res_monomial_word>
      mMonomialSpace;  // We keep all of the monomials here, in order

  // This class contains, and allows one to uniquify, all degrees appearing
  // It contains: memt::Arena, and unordered_set. WORKING ON
  //  MonomialCollection mDegrees;
  
  // Betti tables: set after the frame has been constructed.
  BettiDisplay mBettiNonminimal;
  BettiDisplay mBettiMinimal;
  BettiDisplay mComputationStatus;  // -1: no entries, 0: frame only so far, 1:
                                    // syzygies computed, 2: rank taken into
                                    // account.
  std::vector<std::pair<int, int>> mMinimalizeTODO;  // a list of (slanted deg,
                                                     // level) for which to
                                                     // compute min betti
                                                     // numbers.

  // Computation control
  enum { Initializing, Frame, Matrices, Done } mState;
  int mCurrentLevel;
  // The following are only valid once we are done with "Frame".
  int mSlantedDegree;  // The next degree to be considered, when
                       // "start_computation" is called next.
  int mLoSlantedDegree;
  int mHiSlantedDegree;
  int mMaxLength;

  // These are used during frame construction

  MemoryBlock<PreElement> mPreElements;
  MemoryBlock<res_varpower_word> mVarpowers;
  int mMaxVPSize;

  // These are used during matrix computation
  F4Res mComputer;  // used to construct (level,degree) part of the resolution
  // this is a separate class because there could be several of these, running
  // in parallel.

 public:
  // To allow res-f4.cpp to add to timings.
  double timeMakeMatrix;
  double timeSortMatrix;
  double timeReorderMatrix;
  double timeGaussMatrix;
  double timeClearMatrix;
  double timeResetHashTable;
  double timeComputeRanks;
  double timeComputeSparseRanks;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
