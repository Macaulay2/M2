// Copyright 2014 Michael E. Stillman

#include "res-schreyer-frame.hpp"
#include "f4-monlookup.hpp"

#include <iostream>
#include <algorithm>

long SchreyerFrame::PreElementSorter::ncmps = 0;

SchreyerFrame::SchreyerFrame(const MonomialInfo& MI, int max_level)
  : mMonoid(MI),
    mCurrentLevel(0)
{
  mFrame.mLevels.resize(max_level);
  mMaxVPSize = 2*mMonoid.n_vars() + 1;
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

SchreyerFrame::PreElement* SchreyerFrame::createQuotientElement(packed_monomial m1, packed_monomial m)
{
  bool not_used;
  PreElement* vp = mPreElements.allocate();
  vp->vp = mVarpowers.reserve(mMaxVPSize);
  mMonoid.quotient_as_vp(m1, m, vp->vp, vp->degree, not_used);
  int len = static_cast<int>(varpower_monomials::length(vp->vp));
  mVarpowers.intern(len);
  return vp;
}
long SchreyerFrame::computeIdealQuotient(int lev, long begin, long elem)
{
  ///  std::cout << "computeIdealQuotient(" << lev << "," << begin << "," << elem << ")" << std::endl;
  // Returns the number of elements added
  packed_monomial m = monomial(lev, elem); 
  std::vector<PreElement*> elements;
  for (long i=begin; i<elem; i++)
    elements.push_back(createQuotientElement(monomial(lev,i), m));
  typedef F4MonomialLookupTableT<int32_t> MonomialLookupTable;
  MonomialLookupTable montab(mMonoid.n_vars());

#if 0
  ///std::cout << "  #pre elements = " << elements.size() << std::endl;
  for (auto i=elements.begin(); i != elements.end(); ++i)
    {
      varpower_monomials::elem_text_out(stdout, (*i)->vp);
      fprintf(stdout, "\n");
    }
#endif
  PreElementSorter C;
  std::sort(elements.begin(), elements.end(), C);

  long n_elems = 0;
  for (auto i = elements.begin(); i != elements.end(); ++i)
    {
      int32_t not_used;
      bool inideal = montab.find_one_divisor_vp(0, (*i)->vp, not_used);
      if (inideal) continue;
      // Now we create a packed_monomial, and insert it into 'lev+1'
      montab.insert_minimal_vp(0, (*i)->vp, 0);
      packed_monomial monom = monomialBlock().allocate(mMonoid.max_monomial_size());
      mMonoid.from_varpower_monomial((*i)->vp, elem, monom);
      // Now insert it into the frame
      insert(monom);
      n_elems++;
    }
  //std::cout << "  returns " << n_elems << std::endl;
  return n_elems;
}

long SchreyerFrame::computeNextLevel()
{
  M2_ASSERT(currentLevel() >= 2);
  std::cout << "computeNextLevel: level = " << currentLevel() << std::endl;
  // loop through all the elements at level currentLevel()-2
  int level0 = currentLevel()-2;
  int level1 = level0+1;
  long n_elems_added = 0;
  for (auto i = level(level0).begin(); i != level(level0).end(); ++i)
    {
      long begin = (*i).mBegin;
      long end = (*i).mEnd;
      for (long i=begin+1; i<end; ++i)
        {
          auto& elem = level(level1)[i];
          elem.mBegin = n_elems_added;
          n_elems_added += computeIdealQuotient(level1, begin, i);
          elem.mEnd = n_elems_added;
        }
    }
  //show();
  mCurrentLevel++;
  return n_elems_added;
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
  std::cout << "#levels=" << mFrame.mLevels.size() << " currentLevel=" << currentLevel() << std::endl;
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
