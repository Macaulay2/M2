// to do list
//  - display of "poly" elements in the resolution
//  - get_matrix: should get these elements (if they are there yet)
//  - CoefficientArray: need to be able to build one incrementally
//  - monomial lookup routine
//  - should monomials be varpowers?  or even lists of variables?
//  - make sure monomials are always constructed with that "extra space" for the reducer number.

// res-f4-m2-interface: needs to be rewritten:
//   . don't use gc 
//   . gb_array: 3 routines commented out, due to using these.
//   . need a display routine for Polynomial, also one that limits the number of monomials displayed
// res-gausser
//   . don't use gc
//   . perhaps define CoefficientArray in this class, so it is easily changed
//   . don't yet use ARing stuff.
// res-f4-types.hpp
//   . has lots of junk (most already commented out)






// Copyright 2014 Michael E. Stillman

#ifndef _res_schreyer_frame_hpp_
#define _res_schreyer_frame_hpp_

#include "moninfo.hpp"
#include "memblock.hpp"
#include "varpower-monomial.hpp"
#include "res-f4-types.hpp"
#include <vector>

class F4Res;

class SchreyerFrame
{
public:
  friend class F4Res;
  struct FrameElement
  {
    packed_monomial mMonom; // has component, degree too
    long mDegree;
    long mBegin; // points into next level's elements
    long mEnd;
    poly mSyzygy;
    FrameElement() {}
    FrameElement(packed_monomial monom) : mMonom(monom), mDegree(0), mBegin(-1), mEnd(-1) {}
    FrameElement(packed_monomial monom, long deg) : mMonom(monom), mDegree(deg), mBegin(-1), mEnd(-1) {}
  };

  // Construct an empty frame
  SchreyerFrame(const MonomialInfo& MI, int max_level);
  
  // Destruct the frame
  ~SchreyerFrame();

  // This is where we place the monomials in the frame
  // This requires some care from people calling this function
  MemoryBlock<monomial_word>& monomialBlock() { return mMonomialSpace; }

  // Debugging //
  void show(int len, const ResGausser& G) const; // len is how much of the polynomials to display (len=-1 means all, len=0 means just the frame)

  // Return number of bytes in use.
  long memoryUsage() const;

  // Display memory usage for this computation
  void showMemoryUsage() const;

  // Actual useful functions //
  void endLevel(); // done with the frame for the current level: set's the begin/end's 
                   // for each element at previous level

  long computeNextLevel(); // returns true if new elements are constructed

  long insertLevelZero(packed_monomial monom, long degree);
  long insertLevelOne(packed_monomial monom, poly& syzygy); // grabs syzygy
  long insert(packed_monomial monom);
  long insertBasic(int lev, packed_monomial monom, long degree);

  packed_monomial monomial(int lev, long component) { return level(lev)[component].mMonom; }

  void getBounds(int& loDegree, int& hiDegree, int& length) const;
  M2_arrayint getBettiFrame() const;
  ///////////////////////
  // Display functions //
  ///////////////////////
  // Betti (for non-minimal Betti)
private:
  struct PreElement
  {
    varpower_monomial vp;
    int degree;
  };

  class PreElementSorter
  {
  public:
    typedef PreElement* value;
  private:
    static long ncmps;
  public:
    int compare(value a, value b)
    {
      ncmps ++;
      if (a->degree > b->degree) return GT;
      if (a->degree < b->degree) return LT;
      return varpower_monomials::compare(a->vp, b->vp);
    }
    
    bool operator()(value a, value b)
    {
      ncmps ++;
      if (a->degree > b->degree) return false;
      if (a->degree < b->degree) return true;
      return varpower_monomials::compare(a->vp, b->vp) == LT;
    }
    
    PreElementSorter() {}
    
    void reset_ncomparisons() { ncmps = 0; }
    long ncomparisons() const { return ncmps; }
    
    ~PreElementSorter() {}
  };
  
  struct Level
  {
    std::vector<FrameElement> mElements;
  };
  struct Frame
  {
    std::vector<Level> mLevels;
  };

  int currentLevel() const { return mCurrentLevel; }
  long degree(int lev, long component) const { return level(lev)[component].mDegree; }
  long degree(int lev, packed_monomial m) const { return m[2] + degree(lev-1, m[1]); }
  std::vector<FrameElement>& level(int lev) { return mFrame.mLevels[lev].mElements; }
  const std::vector<FrameElement>& level(int lev) const { return mFrame.mLevels[lev].mElements; }

  PreElement* createQuotientElement(packed_monomial m1, packed_monomial m);
  long computeIdealQuotient(int lev, long begin, long elem);
  long insertElements(int lev, long elem);


  // Private Data
  const MonomialInfo& mMonoid;
  Frame mFrame;
  MemoryBlock<monomial_word> mMonomialSpace; // We keep all of the monomials here, in order

  // These are used during frame construction
  int mCurrentLevel;
  MemoryBlock<PreElement> mPreElements;
  MemoryBlock<varpower_word> mVarpowers;
  int mMaxVPSize;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

