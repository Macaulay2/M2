// Copyright 2014 Michael E. Stillman

#include "res-schreyer-frame.hpp"
#include <iostream>

SchreyerFrame::SchreyerFrame(const MonomialInfo& MI, int max_level)
  : mMonoid(MI),
    mCurrentLevel(0)
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

void SchreyerFrame::endLevel()
{
  /* TODO: this should be made much cleaner! */
  std::cout << "current level: " << currentLevel() << std::endl;
  if (currentLevel() > 0)
    {
      auto this_elem = level(currentLevel()-1).begin();
      (*this_elem).mBegin = 0; // These should already be set, but we'll reset them
      (*this_elem).mEnd = 0;
      long prev_comp = 0;
      long count = 0;
      auto end = level(currentLevel()).end();
      for (auto i = level(currentLevel()).begin(); i != end; ++i, ++count)
        {
          auto this_comp = mMonoid.get_component((*i).mMonom);
          (*this_elem).mEnd = count;
          if (this_comp != prev_comp) 
            {
              // get next elem:
              ++this_elem;
              (*this_elem).mBegin = this_comp;
              (*this_elem).mEnd = this_comp;
            }
          //std::cout << this_comp << " " << prev_comp << " " << count << std::endl;
        }
      (*this_elem).mEnd = count;
    }
  mCurrentLevel++;
}
 
long SchreyerFrame::insert(packed_monomial monom, long degree)
{
  auto& myframe = level(currentLevel());
  myframe.emplace_back(FrameElement(monom,degree));
  return myframe.size();
}

long SchreyerFrame::insert(packed_monomial monom)
{
  return insert(monom, degree(currentLevel(), monom));
}

void SchreyerFrame::show() const
{
  std::cout << "#levels = " << mFrame.mLevels.size() << std::endl;
  for (int i=0; i<mFrame.mLevels.size(); i++)
    {
      auto& myframe = level(i);
      std::cout << "--- level " << i << " ------" << std::endl;
      for (int j=0; j<myframe.size(); j++)
        {
          std::cout << "    " << j << " " << myframe[j].mDegree << "(" << myframe[j].mBegin << "," << myframe[j].mEnd << ") " << std::flush;
          mMonoid.show(myframe[j].mMonom);
          std::cout << std::endl;
        }
    }
}

// local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
