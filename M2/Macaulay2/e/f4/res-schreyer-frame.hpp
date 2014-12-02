// Copyright 2014 Michael E. Stillman

#ifndef _res_schreyer_frame_hpp_
#define _res_schreyer_frame_hpp_

#include "moninfo.hpp"
#include "memblock.hpp"
#include <vector>

class SchreyerFrame
{
public:
  struct FrameElement
  {
    packed_monomial mMonom; // has component, degree too
    long mDegree;
    long mBegin; // points into next level's elements
    long mEnd;
    FrameElement() {}
    FrameElement(packed_monomial monom) : mMonom(monom), mDegree(0), mBegin(0), mEnd(0) {}
    FrameElement(packed_monomial monom, long deg) : mMonom(monom), mDegree(deg), mBegin(0), mEnd(0) {}
  };
  struct Level
  {
    std::vector<FrameElement> mElements;
  };
  struct Frame
  {
    std::vector<Level> mLevels;
  };

  // Operations:

  // Construct an empty frame
  SchreyerFrame(const MonomialInfo& MI, int max_level);
  
  // Destruct the frame
  ~SchreyerFrame();

  MemoryBlock<monomial_word>& monomialBlock() { return mMonomialSpace; }

  // Informational //
  long degree(int level, long component) const { return mFrame.mLevels[level].mElements[component].mDegree; }
  long degree(int level, packed_monomial m) const { return m[2] + degree(level-1, m[1]); }

  // Debugging //
  void show() const;

  // Actual useful functions //
  int currentLevel() const { return mCurrentLevel; }
  void endLevel(); // done with the frame for the current level: set's the begin/end's 
                   // for each element at previous level
  long insert(packed_monomial monom, long degree);
  long insert(packed_monomial monom); // computes the degree

  long divides(Level Li, Level Liplus1, packed_monomial monom); // returns the index of the element which divides monom.
  // Question: should this also return the quotient?
  // For the moment: this looks linearly through, finding a divisor
  // Later, we can get fancy, if it seems like a good idea

  void computeQuotients(Level L_i, long first_i, long current_i, Level Liplus1); // first, current are indices into Li

  ///////////////////////
  // Display functions //
  ///////////////////////
  // Betti (for non-minimal Betti)
private:
  std::vector<FrameElement>& level(int lev) { return mFrame.mLevels[lev].mElements; }
  const std::vector<FrameElement>& level(int lev) const { return mFrame.mLevels[lev].mElements; }

  const MonomialInfo& mMonoid;
  Frame mFrame;
  int mCurrentLevel;
  MemoryBlock<monomial_word> mMonomialSpace; // We keep all of the monomials here, in order
  
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

