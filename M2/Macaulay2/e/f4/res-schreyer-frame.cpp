// Copyright 2014 Michael E. Stillman

#include "res-schreyer-frame.hpp"
#include <iostream>

SchreyerFrame::SchreyerFrame(const MonomialInfo& MI, int max_level)
  : mMonoid(MI)
{
  mFrame.mLevels.resize(max_level);
}
  
// Destruct the frame
SchreyerFrame::~SchreyerFrame() 
{
  // Nothing to do here yet
  // the monomial block will free itself
  // as will the std::vector's
}

long SchreyerFrame::insert(int level, packed_monomial monom, long degree)
{
  M2_ASSERT(mFrame.mLevels.size() > level);
  auto& myElements = mFrame.mLevels[level].mElements;
  myElements.emplace_back(FrameElement(monom,degree));
  return myElements.size();
}

long SchreyerFrame::insert(int level, packed_monomial monom)
{
  return insert(level, monom, degree(level, monom));
}

void SchreyerFrame::show() const
{
  std::cout << "#levels = " << mFrame.mLevels.size() << std::endl;
  for (int i=0; i<mFrame.mLevels.size(); i++)
    {
      auto& myframe = mFrame.mLevels[i].mElements;
      std::cout << "--- level " << i << " ------" << std::endl;
      for (int j=0; j<myframe.size(); j++)
        {
          std::cout << "    " << j << " " << myframe[j].mDegree << std::flush;
          mMonoid.show(myframe[j].mMonom);
          std::cout << std::endl;
        }
    }
}

// local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
