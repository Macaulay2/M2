// Copyright 2005-2021 Michael E. Stillman

#include "f4/f4-spairs.hpp"

#include "mem.hpp"            // for stash
#include "style.hpp"          // for INTSIZE

#include <gc/gc_allocator.h>  // for gc_allocator
#include <stdint.h>           // for int32_t
#include <stdio.h>            // for fprintf, stderr, size_t
#include <algorithm>          // for stable_sort
#include <vector>             // for vector, vector<>::iterator

#if defined(WITH_TBB)
F4SPairSet::F4SPairSet(const MonomialInfo *M0, const gb_array &gb0, mtbb::task_arena& scheduler)
#else
F4SPairSet::F4SPairSet(const MonomialInfo *M0, const gb_array &gb0)
#endif
  : M(M0),
    gb(gb0),
    mSPairCompare(mSPairs),
    mSPairQueue(mSPairCompare),
    nsaved_unneeded(0),
    mMinimizePairsSeconds(0)
#if defined(WITH_TBB)
  , mScheduler(scheduler)
#endif
{
  max_varpower_size = 2 * M->n_vars() + 1;

  //spair *used_to_determine_size = nullptr;
  //mSPairSizeInBytes = sizeofspair(used_to_determine_size, M->max_monomial_size());
}

F4SPairSet::~F4SPairSet()
{
  // Deleting the stash deletes all memory used here
  // PS, VP are deleted automatically.
  M = nullptr;
}

void F4SPairSet::insert_spair(pre_spair *p, int me)
{
  int j = p->j;
  int deg = p->deg1 + gb[me]->deg;
  // int me_component = M->get_component(gb[me]->f.monoms);

  spair result {SPairType::SPair, deg, me, j, nullptr};
  
  auto allocRange = mSPairLCMs.allocateArray<monomial_word>(M->max_monomial_size());
  result.lcm = allocRange.first;
  
  M->from_varpower_monomial(p->quot, 0, result.lcm);
  M->unchecked_mult(result.lcm, gb[me]->f.monoms, result.lcm);

  mSPairLCMs.shrinkLastAllocate(allocRange.first,
                                allocRange.second,
                                allocRange.first + M->monomial_size(result.lcm));

  auto sPairIndex = mSPairs.size();
  mSPairs.push_back(result);  
  mSPairQueue.push(sPairIndex);
  
}

void F4SPairSet::delete_spair(spair *p) { delete p; }
void F4SPairSet::insert_generator(int deg, packed_monomial lcm, int col)
{
  spair result {SPairType::Generator,deg,col,-1,nullptr};

  auto allocRange = mSPairLCMs.allocateArray<monomial_word>(M->monomial_size(lcm));
  result.lcm = allocRange.first;
  
  M->copy(lcm, result.lcm);

  auto sPairIndex = mSPairs.size();
  mSPairs.push_back(result);  
  mSPairQueue.push(sPairIndex);
}

bool F4SPairSet::pair_not_needed(spair *p, gbelem *m)
{
  // if (p->type != SPairType::SPair && p->type != SPairType::Ring) return false;
  if (p->type != SPairType::SPair) return false;
  if (M->get_component(p->lcm) != M->get_component(m->f.monoms)) return false;
  return M->unnecessary(
      m->f.monoms, gb[p->i]->f.monoms, gb[p->j]->f.monoms, p->lcm);
}

int F4SPairSet::remove_unneeded_pairs()
{
  // Loop through every spair, determine if it can be jettisoned
  // and do so.  Return the number removed.

  // MES: Check the ones in this_set? Probably not needed...

  if (gb.size() == 0) return 0;
  
  gbelem *m = gb[gb.size() - 1];
  //long nremoved = 0;

#if defined(WITH_TBB)
  mScheduler.execute([&] {
    mtbb::parallel_for(mtbb::blocked_range<int>{0, INTSIZE(mSPairs)},
                       [&](const mtbb::blocked_range<int>& r)
                       {
                         for (auto i = r.begin(); i != r.end(); ++i)
                           {
                             if (pair_not_needed(&mSPairs[i],m))
                               {
                                 mSPairs[i].type = SPairType::Retired;
                               }
                           }
                       });
  });
#else  
  for (auto& p : mSPairs)
  {
    if (pair_not_needed(&p,m))
    {
      p.type = SPairType::Retired;
      //++nremoved;
    }
  }
#endif  
  return 0;
  //return nremoved;
  
}

std::pair<bool,int> F4SPairSet::setThisDegree()
{
  if (mSPairQueue.empty()) return {false, 0};
  
  auto& queueTop = mSPairQueue.top();
  mThisDegree = mSPairs[queueTop].deg;
  return {true,mThisDegree};
}

//spair *F4SPairSet::get_next_pair()
std::pair<bool,spair> F4SPairSet::get_next_pair()
// get the next pair in this degree (the one 'prepare_next_degree' set up')
{
  if (mSPairQueue.empty()) return {false, {}};
  auto result = mSPairQueue.top();
  if (mSPairs[result].deg != mThisDegree) return {false, {} };
  mSPairQueue.pop();
  return {true,mSPairs[result]};
}

void F4SPairSet::discardSPairsInCurrentDegree()
{
  while (not mSPairQueue.empty())
    {
      auto result = mSPairQueue.top();
      if (mSPairs[result].deg != mThisDegree) return;
      mSPairQueue.pop();
    }
}

int F4SPairSet::find_new_pairs(bool remove_disjoints)
// returns the number of new pairs found
{
  // this is used for "late" removal of spairs -- will need to be reworked
  //   in the new priority_queue approach
  nsaved_unneeded += remove_unneeded_pairs();
  int len = construct_pairs(remove_disjoints);
  return len;
}

void F4SPairSet::display_spair(spair *p)
// A debugging routine which displays an spair
{
  if (p->type == SPairType::SPair)
    {
      fprintf(stderr, "[%d %d deg %d lcm ", p->i, p->j, p->deg);
      M->show(p->lcm);
      fprintf(stderr, "\n");
    }
  else
    {
      fprintf(stderr, "unknown type\n");
    }
}

void F4SPairSet::display()
// A debugging routine which displays the spairs in the set
{
  /*
    fprintf(stderr, "spair set\n");
  for (spair *p = heap; p != nullptr; p = p->next)
    {
      fprintf(stderr, "   ");
      display_spair(p);
    }
  fprintf(stderr, "current set\n");
  for (spair *p = this_set; p != nullptr; p = p->next)
    {
      fprintf(stderr, "   ");
      display_spair(p);
    }
  */
}

////////////////////////////////
// Construction of new spairs //
////////////////////////////////

pre_spair *F4SPairSet::create_pre_spair(int j)
{
  // Steps:
  //  a. allocate the space for the pre_spair and the varpower monomial
  //  b. compute the quotient and the degree
  pre_spair *result = PS.allocate();
  result->quot = VP.reserve(max_varpower_size);
  result->j = j;
  result->type = SPairType::SPair;;
  M->quotient_as_vp(gb[j]->f.monoms,
                    gb[gb.size() - 1]->f.monoms,
                    result->quot,
                    result->deg1,
                    result->are_disjoint);
  int len = static_cast<int>(varpower_monomials::length(result->quot));
  VP.intern(len);
  return result;
}

void insert_pre_spair(std::vector<std::vector<pre_spair *>> & bins, pre_spair *p)
{
  int d = p->deg1;
  if (d >= bins.size()) bins.resize(d + 1);
  bins[d].push_back(p);
}

long PreSPairSorter::ncmps = 0;

int F4SPairSet::construct_pairs(bool remove_disjoints)
{
  if (gb.size() == 0) return 0;

  VP.reset();
  PS.reset();
  gbelem *me = gb[gb.size() - 1];
  int me_component = static_cast<int>(M->get_component(me->f.monoms));

  std::vector<std::vector<pre_spair *>> bins;

  mtbb::tick_count t0 {mtbb::tick_count::now()};
  
  // Loop through each element of gb, and create the pre_spair
  for (int i = 0; i < gb.size() - 1; i++)
    {
      if (gb[i]->minlevel == ELEM_NON_MIN_GB) continue;
      if (me_component != M->get_component(gb[i]->f.monoms)) continue;
      pre_spair *p = create_pre_spair(i);
      insert_pre_spair(bins, p);
    }

  mtbb::tick_count t1 {mtbb::tick_count::now()};
  mPrePairsSeconds += (t1-t0).seconds();  

  ////////////////////////////
  // Now minimalize the set //
  ////////////////////////////
  MonomialLookupTable *montab = new MonomialLookupTable(M->n_vars());

  PreSPairSorter C;
  int n_new_pairs = 0;
  for (int i = 0; i < bins.size(); i++)
    {
      if (bins[i].size() == 0) continue;
      // First sort the monomials of this degree

      std::stable_sort(bins[i].begin(), bins[i].end(), C);

      // Loop through each degree and potentially insert...
      auto first = bins[i].begin();
      auto next = first;
      auto end = bins[i].end();
      for (; first != end; first = next)
        {
          next = first + 1;
          pre_spair *chosen = *first;
          while (next != end)
            {
              pre_spair *p = *next;
              if (!varpower_monomials::is_equal(chosen->quot, p->quot)) break;
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
                  insert_spair(chosen, INTSIZE(gb) - 1);
                  n_new_pairs++;
                }
            }
        }
    }
  delete montab;
  mtbb::tick_count t2 {mtbb::tick_count::now()};
  mMinimizePairsSeconds += (t2-t1).seconds();

  return n_new_pairs;
}

#if 0
// testing mathic and mathicgb routines...
class TestPairQueueConfiguration
{
private:
  // What should be here?
  
public:
  TestPairQueueConfiguration(const gb_array& gb,
                     XXX
                     );
  using PairData = MonomialInfo::OrderedMonomial;
  void computePairData(
                       size_t col,
                       size_t row,
                       PairData & m, // allocated space?
                       ) const;

  using CompareResult = bool;
  bool compare(
               size_t colA,
               size_t rowA,
               MonomialInfo::ConstOrderedMonomial a,
               size_t colB,
               size_t rowB,
               MonomialInfo::ConstOrderedMonomial b) const
  {
    // What to change this test code to?
    const auto cmp = orderMonoid().compare(*a, *b);
    if (cmp == GT)
      return true;
    if (cmp == LT)
      return false;
      
    const bool aRetired = mBasis.retired(rowA) || mBasis.retired(colA);
    const bool bRetired = mBasis.retired(rowB) || mBasis.retired(colB);
    if (aRetired || bRetired)
      return !bRetired;
      
    if (mPreferSparseSPairs) {
      const auto termCountA =
        mBasis.basisElement(colA).termCount() +
        mBasis.basisElement(rowA).termCount();
      const auto termCountB =
        mBasis.basisElement(colB).termCount() +
        mBasis.basisElement(rowB).termCount();
      if (termCountA > termCountB)
        return true;
      if (termCountA < termCountB)
        return false;
    }
    return colA + rowA > colB + rowB;
  }

  bool cmpLessThan(bool v) const {return v;}
};

class TestSPairs
{
private:
  mathic::PairQueue<TestPairQueueConfiguration> mPairQueue;

public:
  TestSPairs(gb_poly& currentGroebnerBasis);

  ~TestSPairs() {} // anything here?

};
#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  indent-tabs-mode: nil
//  End:
