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

long SchreyerFrame::memoryUsage() const
{
  long result = mMonomialSpace.memoryUsage();
  for (int i=0; i<mFrame.mLevels.size(); i++)
    {
      result += level(i).capacity() * sizeof(FrameElement);
    }
  return result;
}

void SchreyerFrame::showMemoryUsage() const
{
  std::cout << "Frame memory usage" << std::endl;
  std::cout << "  level\t\t#elems\tused\tallocated" << std::endl;
  long alloc = 0;
  long used = 0;
  long nelems = 0;
  for (int i=0; i<mFrame.mLevels.size(); i++)
    {
      long nelems_level = level(i).size();
      if (nelems_level == 0) continue;
      long used_level = nelems_level * sizeof(FrameElement);
      long alloc_level = level(i).capacity() * sizeof(FrameElement);
      std::cout << "  " << i << "\t\t\t" << nelems_level << "\t\t" << used_level << "\t\t" << alloc_level << std::endl;
      nelems += nelems_level;
      used += used_level;
      alloc += alloc_level;
    }
  std::cout << "  all lev\t" << nelems << "\t\t" << used << "\t\t" << alloc << std::endl;
  long monomSpace = mMonomialSpace.memoryUsage();
  long monomUsed = nelems * mMonoid.max_monomial_size() * sizeof(monomial_word);
  std::cout << "  monomials   \t\t" << monomUsed << "\t" << monomSpace << std::endl;
  std::cout << "  total       \t\t" << (used+monomUsed) << "\t" << (alloc+monomSpace) << std::endl;
}

void SchreyerFrame::show() const
{
  std::cout << "#levels = " << mFrame.mLevels.size() << std::endl;
  for (int i=0; i<mFrame.mLevels.size(); i++)
    {
      auto& myframe = level(i);
      if (myframe.size() == 0) continue;
      std::cout << "--- level " << i << " ------" << std::endl;
      for (int j=0; j<myframe.size(); j++)
        {
          std::cout << "    " << j << " " << myframe[j].mDegree << "(" << myframe[j].mBegin << "," << myframe[j].mEnd << ") " << std::flush;
          mMonoid.show(myframe[j].mMonom);
          std::cout << std::endl;
        }
    }
  showMemoryUsage();
}

// local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
