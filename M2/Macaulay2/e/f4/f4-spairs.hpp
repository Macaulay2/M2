// Copyright 2004-2021 Michael E. Stillman

#pragma once

#include <queue>                     // for std::priority_queue
#include "MemoryBlock.hpp"           // for MemoryBlock
#include "f4/f4-types.hpp"           // for spair (ptr only), gb_array, pre_...
#include "f4/memblock.hpp"           // for F4MemoryBlock
#include "f4/moninfo.hpp"            // for MonomialInfo (ptr only), packed_...
#include "f4/varpower-monomial.hpp"  // for varpower_word

class F4SPairSet
{
 private:
  spair *make_spair(SPairType type,
                    int deg,
                    int i,
                    int j);  // CAUTION: lcm is allocated correctly, but is
  // NOT initialized.  That is the caller's responsibility!!

  bool pair_not_needed(spair *p, gbelem *m);

  int remove_unneeded_pairs();
  // returns the number of pairs removed, based on the element gb[gb.size()-1]

  pre_spair *create_pre_spair(int i);

  int construct_pairs(bool remove_disjoints);

 public:
#if defined(WITH_TBB)
  F4SPairSet(const MonomialInfo *MI0, const gb_array &gb0,mtbb::task_arena& scheduler);
#else
  F4SPairSet(const MonomialInfo *MI0, const gb_array &gb0);  
#endif
  
  ~F4SPairSet();

  void insert_generator(int deg, packed_monomial lcm, int column);
  // The lcm is a pointer to the space in the polynomial itself

  void insert_spair(pre_spair *p, int me);

  void delete_spair(spair *p);

  int find_new_pairs(bool remove_disjoints);
  // returns the number of new pairs found, using the last element on this list

  std::pair<bool,int> setThisDegree();
  
  // int prepare_next_degree(int max, int &result_number);
  // Returns the (sugar) degree being done next, and collects all (or at
  // most 'max', if max>0) spairs in this lowest degree.
  // Returns the degree, sets result_number.
  // These spairs are not sorted in any way.  Should they be?

  std::pair<bool,spair> get_next_pair();
  // get the next pair in this degree (the one 'prepare_next_degree' set up')

  void display_spair(spair *p);
  // A debugging routine which displays an spair

  void display();
  // A debugging routine which displays the spairs in the set

  long n_unneeded_pairs() const { return nsaved_unneeded; }
  // Returns how many pairs were created, then later removed due to
  // spair criteria.

  size_t numberOfSPairs() const { return mSPairs.size(); }

  double secondsToMinimizePairs() const { return mMinimizePairsSeconds; }
  double secondsToCreatePrePairs() const { return mPrePairsSeconds; }
 private:
  F4MemoryBlock<pre_spair> PS;      // passed to constructor routine
  F4MemoryBlock<varpower_word> VP;  // used for constructing new pairs
  MemoryBlock mSPairLCMs;  // used for spair lcms  
  
  int max_varpower_size;

  const MonomialInfo *M;
  const gb_array &gb;

  std::vector<spair> mSPairs;
  SPairCompare mSPairCompare;
  std::priority_queue<size_t,std::vector<size_t>,SPairCompare> mSPairQueue;

  long mThisDegree;

  // stats
  long nsaved_unneeded;
  double mMinimizePairsSeconds;
  double mPrePairsSeconds;

#if defined (WITH_TBB)
  mtbb::task_arena& mScheduler;
#endif
  
};


// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  indent-tabs-mode: nil
//  End:
