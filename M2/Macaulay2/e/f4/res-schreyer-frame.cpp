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

SchreyerFrame::PreElement* SchreyerFrame::createQuotientElement(packed_monomial m1, packed_monomial m)
{
  bool not_used;
  PreElement* vp = mPreElements.allocate();
  vp->vp = mVarpowers.reserve(mMaxVPSize);
  mMonoid.quotient_as_vp(m1, m, vp->vp, vp->degree, not_used);
  int len = static_cast<int>(vp->vp[0]);
  mVarpowers.intern(len);
  return vp;
}
long SchreyerFrame::computeIdealQuotient(int lev, long begin, long elem)
{
  // Returns the number of elements added
  packed_monomial m = monomial(lev, elem); 
  std::vector<PreElement*> elements;
  for (long i=begin; i<elem; i++)
    elements.push_back(createQuotientElement(monomial(lev,i), m));
  // Now sort these?

  return 0;
}
/** insertElements
    input: lev >= 1
           component: at level 'lev'
    consequences: modified level(lev+1): adds new lead terms of syzygies at that level.
                  also modifies level(lev)[component].mBegin, mEnd.
    returns: number of new elements added at level lev+1
 */ 
long SchreyerFrame::insertElements(int lev, long elem)
{
  auto& myelem = level(lev)[elem];
  long mycomp = mMonoid.get_component(myelem.mMonom);
  auto& prevelem = level(lev-1)[mycomp];
  long begin = prevelem.mBegin;
  if (elem == begin) return 0; // No lead terms at next level for this element
  M2_ASSERT(elem > begin && elem < prevelem.mEnd);
  return computeIdealQuotient(lev, begin, elem);
  // Plan is the following:
  // have: at level 'lev': an element.
  //   find first element at level 'lev' with the same component as 'component' has
  // For each element m in the range: [begin, end) at level 'lev'
  //   compute (m1,...,m(i-1)) : m(i) each as a VP
  //   sort them
  //   for each, in order:
  //     check if it is in the monideal of the prev ones
  //     if so: continue to the next
  //     if not: add it in to the monideal
  //             add it to 'level+1': need monomial as packed_monomial, its degree.
  //             modify the element at level 'lev': mEnd changes
  // At the end: reset the memory blocks
}
#if 0
int F4SPairSet::construct_pairs(bool remove_disjoints)
{
  if (gb.size() == 0) return 0;

  VP.reset();
  PS.reset();
  gbelem *me = gb[gb.size()-1];
  int me_component = static_cast<int>(M->get_component(me->f.monoms));

  typedef VECTOR(pre_spair *) spairs;

  VECTOR( VECTOR(pre_spair *) ) bins;

  // Loop through each element of gb, and create the pre_spair
  for (int i=0; i<gb.size()-1; i++)
    {
      if (gb[i]->minlevel == ELEM_NON_MIN_GB) continue;
      if (me_component != M->get_component(gb[i]->f.monoms)) continue;
      pre_spair *p = create_pre_spair(i);
      insert_pre_spair(bins, p);
    }

  ////////////////////////////
  // Now minimalize the set //
  ////////////////////////////
  MonomialLookupTable *montab = new MonomialLookupTable(M->n_vars());

  PreSPairSorter C;
  int n_new_pairs = 0;
  for (int i=0; i<bins.size(); i++)
    {
      if (bins[i].size() == 0) continue;
      // First sort the monomials of this degree

      //TODO: MES remove all uses of QuickSorter here.
      //      QuickSorter<PreSPairSorter>::sort(&C, &(bins[i])[0], bins[i].size());

      std::sort(bins[i].begin(), bins[i].end(), C);

      // Loop through each degree and potentially insert...
      spairs::iterator first = bins[i].begin();
      spairs::iterator next = first;
      spairs::iterator end = bins[i].end();
      for ( ; first != end; first = next)
        {
          next = first+1;
          pre_spair *chosen = *first;
          while (next != end)
            {
              pre_spair *p = *next;
              if (!varpower_monomials::equal(chosen->quot, p->quot)) break;
              next++;
            }
          /* At this point: [first,next) is the range of equal monomials */

          int32_t junk;
          bool inideal = montab->find_one_divisor_vp(0, chosen->quot, junk);
          if (!inideal)
            {
              // MES: Maybe choose another of the equal monomials...
              montab->insert_minimal_vp(0, chosen->quot, 0);
              // The following condition is that gcd is not one
              if (!remove_disjoints || !chosen->are_disjoint)
                {
                  insert_spair(chosen, INTSIZE(gb)-1);
                  n_new_pairs++;
                }
            }
        }
    }
  delete montab;

  return n_new_pairs;
}
#endif

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
