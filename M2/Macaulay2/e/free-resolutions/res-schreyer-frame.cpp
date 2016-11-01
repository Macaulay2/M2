// Copyright 2014 Michael E. Stillman

#include "stdinc.hpp"
#include "res-schreyer-frame.hpp"

#include <iostream>
#include <iomanip>
#include <algorithm>

MonomialCounter::MonomialCounter(const MonomialInfo& M)
  : mIgnoreMonomials(new MonomialsIgnoringComponent(M)),
    mAllMonomials(mIgnoreMonomials),
    mNumAllMonomials(0),
    mNextMonom(nullptr),
    mMonoid(M)
{
  // start out mNextMonom
  mNextMonom = mMonomSpace.reserve(monoid().max_monomial_size());
}
void MonomialCounter::accountForMonomial(const packed_monomial mon)
{
  // First copy monomial
  // Then call find_or_insert
  // If not there, increment number of monomials
  // If there: intern monomial

  monoid().copy(mon, mNextMonom);
  packed_monomial not_used;
  if (mAllMonomials.find_or_insert(mNextMonom, not_used))
    {
      // true, means that it was already there
      // nothing needs to be done
    }
  else
    {
      // false: new monomial
      mNumAllMonomials++;
      mMonomSpace.intern(monoid().monomial_size(mNextMonom));
      mNextMonom = mMonomSpace.reserve(monoid().max_monomial_size());      
    }
}



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
      ncmps ++;
      if (a->degree > b->degree) return GT;
      if (a->degree < b->degree) return LT;
      return varpower_monomials_compare(a->vp, b->vp);
    }
    
    bool operator()(value a, value b)
    {
      ncmps ++;
      if (a->degree > b->degree) return false;
      if (a->degree < b->degree) return true;
      return varpower_monomials_compare(a->vp, b->vp) == LT;
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
    mComputer(*this),
    mAllMonomials(R.monoid())
{
  mFrame.mLevels.resize(max_level);
  mMaxVPSize = 2*monoid().numVariables() + 1;
}
  
// Destruct the frame
SchreyerFrame::~SchreyerFrame() 
{
  // Nothing to do here yet
  // the monomial block will free itself
  // as will the std::vector's
}

void SchreyerFrame::start_computation(StopConditions& stop)
{
  while (true)
    {
      switch (mState) {
      case Initializing:
        break;
      case Frame:
                std::cout << "maxsize = " << mFrame.mLevels.size() << " and mCurrentLevel = " << mCurrentLevel << std::endl;
        if (mCurrentLevel >= mFrame.mLevels.size() or computeNextLevel() == 0)
          {
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
            for (int i=0; i<mMinimalizeTODO.size(); i++)
              {
                auto a = mMinimalizeTODO[i];
                //                std::cout << "(" << a.first << "," << a.second << ") ";
              }
            // std::cout << std::endl;
          }
        break;
      case Matrices:
        std::cout << "start_computation: entering Matrices(" << mCurrentLevel << ", " << mSlantedDegree << ")" << std::endl;
        if (stop.always_stop) return;
        if (mCurrentLevel > mMaxLength)
          {
            mCurrentLevel = 2;
            mSlantedDegree++;
            if (mSlantedDegree > mHiSlantedDegree)
              {
                showMemoryUsage();
                for (auto it=mMinimalizeTODO.cbegin(); it != mMinimalizeTODO.cend(); ++it)
                  {
                    int rk = rank(it->first, it->second);
                    mBettiMinimal.entry(it->first, it->second) -= rk;
                    mBettiMinimal.entry(it->first+1, it->second-1) -= rk;
                  }
                mState = Done;
                mBettiMinimal.output();
                break;
              }
            if (stop.stop_after_degree and mSlantedDegree > stop.degree_limit)
              return;
          }
        mComputer.construct(mCurrentLevel, mSlantedDegree+mCurrentLevel);
        std::cout << "Number of distinct monomials so far = " << mAllMonomials.count() << std::endl;
        mCurrentLevel++;
        break;
      case Done:
        return;
      default:
        break;
      }
    }
}

std::vector<int> SchreyerFrame::getBetti(int type) const
{
  if (type == 0)
    return mBettiMinimal.getBetti();
  if (type == 1)
    return getBettiFrame();
  
  std::cerr << "betti display not implemenented yet" << std::endl;
  std::vector<int> nothing;
  return nothing;
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

SchreyerFrame::PreElement* SchreyerFrame::createQuotientElement(packed_monomial m1, packed_monomial m)
{
  bool not_used;
  PreElement* vp = mPreElements.allocate();
  vp->vp = mVarpowers.reserve(mMaxVPSize);
  monoid().quotient_as_vp(m1, m, vp->vp, vp->degree, not_used);
  int len = static_cast<int>(varpower_length(vp->vp));
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
  MonomialLookupTable montab(monoid().numVariables());

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
      packed_monomial monom = monomialBlock().allocate(monoid().max_monomial_size());
      monoid().from_varpower_monomial((*i)->vp, elem, monom);
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
  if (currentLevel() >= mFrame.mLevels.size()) return 0;
  //  std::cout << "computeNextLevel: level = " << currentLevel() << std::endl;
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
  setSchreyerOrder(mCurrentLevel);
  mCurrentLevel++;
  return n_elems_added;
}

void SchreyerFrame::setSchreyerOrder(int lev)
{
  auto& myframe = level(lev);
  if (lev == 0)
    {
      for (long i=0; i<myframe.size(); i++)
        myframe[i].mTiebreaker = i;
      return;
    }

  auto& prevframe = level(lev-1);
  long* tiebreakers = new long[myframe.size()];
  
  for (long i=0; i<myframe.size(); i++)
    {
      long comp = monoid().get_component(myframe[i].mMonom);
      tiebreakers[i] = i + myframe.size() * prevframe[comp].mTiebreaker;
    }
  std::sort(tiebreakers, tiebreakers + myframe.size());
  for (long i=0; i<myframe.size(); i++)
    {
      myframe[tiebreakers[i] % myframe.size()].mTiebreaker = i;
    }
}

long SchreyerFrame::insertBasic(int lev, packed_monomial monom, int degree)
{
  // if lev >= 2, then level(lev-1)[comp].(mBegin,mEnd) is set separately.
  auto& myframe = level(lev);
  long idx = myframe.size();
  myframe.emplace_back(FrameElement(monom,degree));
  auto& myelem = myframe[idx];
  myelem.mTotalMonom = monomialBlock().allocate(monoid().max_monomial_size());
  if (lev > 0)
    {
      auto& prevlevel = level(lev-1);
      long comp = monoid().get_component(myelem.mMonom);
      monoid().unchecked_mult(myelem.mMonom, prevlevel[comp].mTotalMonom, myelem.mTotalMonom);
      monoid().set_component(monoid().get_component(prevlevel[comp].mTotalMonom), myelem.mTotalMonom);
    }
  else
    {
      monoid().copy(myelem.mMonom, myelem.mTotalMonom);
    }
  return myframe.size();
}

long SchreyerFrame::insertLevelZero(packed_monomial monom, int degree)
{
  return insertBasic(0, monom, degree);
}
long SchreyerFrame::insertLevelOne(packed_monomial monom, poly& syzygy)
{

  long last = insertBasic(1, monom, degree(1, monom));
  long comp = monoid().get_component(monom);
  auto& p = level(0)[comp];
  if (p.mBegin == -1)
    p.mBegin = last-1;
  p.mEnd = last;
  std::swap(level(1)[level(1).size()-1].mSyzygy, syzygy);
  return last;
}
long SchreyerFrame::insert(packed_monomial monom)
{
  return insertBasic(currentLevel(), monom, degree(currentLevel(), monom));
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

#if 0
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
      std::cout << "  " << i << "\t\t\t " << nelems_level << "\t\t " << used_level << "\t\t " << alloc_level << std::endl;
      nelems += nelems_level;
      used += used_level;
      alloc += alloc_level;
    }
  std::cout << "  all lev\t" << nelems << "\t\t" << used << "\t\t" << alloc << std::endl;
  long monomSpace = mMonomialSpace.memoryUsage();
  long monomUsed = nelems * monoid().max_monomial_size() * sizeof(monomial_word);
  std::cout << "  monomials   \t\t" << monomUsed << "\t" << monomSpace << std::endl;
  std::cout << "  total       \t\t" << (used+monomUsed) << "\t" << (alloc+monomSpace) << std::endl;
}
#endif
void SchreyerFrame::showMemoryUsage() const
{
  std::cout << "Frame memory usage" << std::endl;
  // widths: level: 6, #elems: 8, used: 6, allocated: 11
  std::cout << " level" << "   #elems" << "   used" << "   allocated" << "     nterms" << "       poly" << "   polalloc" << std::endl;
  long alloc = 0;
  long used = 0;
  long nelems = 0;
  long poly_used = 0;
  long poly_alloc = 0;
  long poly_nterms = 0;
  long poly_used_level = 0;
  long poly_alloc_level = 0;
  long poly_nterms_level = 0;
  for (int i=0; i<mFrame.mLevels.size(); i++)
    {
      long nelems_level = level(i).size();
      if (nelems_level == 0) continue;
      long used_level = nelems_level * sizeof(FrameElement);
      long alloc_level = level(i).capacity() * sizeof(FrameElement);
      poly_nterms_level = 0;
      poly_used_level = 0;
      poly_alloc_level = 0;
      for (int j=0; j<nelems_level; j++)
        {
          ring().memUsage(level(i)[j].mSyzygy, poly_nterms_level, poly_used_level, poly_alloc_level);
        }
      poly_nterms += poly_nterms_level;
      poly_used += poly_used_level;
      poly_alloc += poly_alloc_level;
      std::cout << std::setw(6) << i
                << " " << std::setw(8) << nelems_level
                << " " << std::setw(6) << used_level
                << " " << std::setw(11) << alloc_level
                << " " << std::setw(10) << poly_nterms_level        
                << " " << std::setw(10) << poly_used_level
                << " " << std::setw(10) << poly_alloc_level
                << std::endl;
      nelems += nelems_level;
      used += used_level;
      alloc += alloc_level;
    }
  std::cout << "   all"
            << " " << std::setw(8) << nelems
            << " " << std::setw(6) << used
            << " " << std::setw(11) << alloc
            << " " << std::setw(10) << poly_nterms
            << " " << std::setw(10) << poly_used
            << " " << std::setw(10) << poly_alloc
            << std::endl;

  long monomSpace = mMonomialSpace.memoryUsage();
  long monomUsed = nelems * monoid().max_monomial_size() * sizeof(monomial_word);
  std::cout << "monomials     "
            << std::setw(6) << monomUsed
            << " " << std::setw(11) << monomSpace
            << std::endl;
  std::cout << "total mem     "
            << std::setw(6) << (used+monomUsed+poly_used)
            << " " << std::setw(11) << (alloc+monomSpace+poly_alloc)
            << std::endl;
}

void SchreyerFrame::show(int len) const
{
  std::cout << "#levels=" << mFrame.mLevels.size() << " currentLevel=" << currentLevel() << std::endl;
  for (int i=0; i<mFrame.mLevels.size(); i++)
    {
      auto& myframe = level(i);
      if (myframe.size() == 0) continue;
      std::cout << "--- level " << i << " ------" << std::endl;
      for (int j=0; j<myframe.size(); j++)
        {
          std::cout << "    " << j << " " << myframe[j].mDegree 
                    << " (" << myframe[j].mBegin << "," << myframe[j].mEnd << ") " << std::flush;
          std::cout << "(size:" << myframe[j].mSyzygy.len << ") [";
          monoid().showAlpha(myframe[j].mTotalMonom);
          std::cout << "  " << myframe[j].mTiebreaker << "] ";
          if (len == 0 or myframe[j].mSyzygy.len == 0)
            monoid().showAlpha(myframe[j].mMonom);
          else
            display_poly(stdout, ring(), myframe[j].mSyzygy);
          std::cout << std::endl;
        }
    }
  showMemoryUsage();
}

void SchreyerFrame::getBounds(int& loDegree, int& hiDegree, int& length) const
{
  auto lev0 = level(0);
  loDegree = hiDegree = static_cast<int>(lev0[0].mDegree);
  for (int lev=0; lev<mFrame.mLevels.size(); lev++)
    {
      auto& myframe = level(lev);
      if (myframe.size() == 0) return;
      length = lev;
      for (auto p=myframe.begin(); p != myframe.end(); ++p)
        {
          int deg = p->mDegree;
          deg -= lev; // slanted degree
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
  //std::cout << "bounds: lo=" << lo << " hi=" << hi << " len=" << len << std::endl;
  mBettiNonminimal = BettiDisplay(lo,hi,len);
  mBettiMinimal = BettiDisplay(lo,hi,len);

  for (int lev=0; lev<=len; lev++)
    {
      auto& myframe = level(lev);
      for (auto p=myframe.begin(); p != myframe.end(); ++p)
        {
          int deg = p->mDegree; // this is actual degree, not slanted degree
          mBettiNonminimal.entry(deg-lev,lev) ++ ;
          mBettiMinimal.entry(deg-lev,lev) ++ ;
        }
    }

  // Now set the todo list of pairs (degree, level) for minimalization.
  for (int slanted_degree = lo; slanted_degree < hi; slanted_degree++)
    {
      for (int lev = 1; lev <= len; lev++)
        {
          if (mBettiNonminimal.entry(slanted_degree, lev) > 0 and mBettiNonminimal.entry(slanted_degree+1, lev-1) > 0)
            {
              mMinimalizeTODO.push_back(std::make_pair(slanted_degree, lev));
            }
        }
    }
}


std::vector<int> SchreyerFrame::getBettiFrame() const
{
  int lo, hi, len;
  getBounds(lo, hi, len);
  //  std::cout << "bounds: lo=" << lo << " hi=" << hi << " len=" << len << std::endl;
  BettiDisplay B(lo,hi,len);
  // now set B

  for (int lev=0; lev<=len; lev++)
    {
      auto& myframe = level(lev);
      for (auto p=myframe.begin(); p != myframe.end(); ++p)
        {
          int deg = p->mDegree; // this is actual degree, not slanted degree
          B.entry(deg-lev,lev) ++ ;
        }
    }

  return B.getBetti();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions "
// indent-tabs-mode: nil
// End:
