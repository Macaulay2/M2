// Copyright 2004-2021 Michael E. Stillman

#pragma once

/**
 * @file f4/f4-spairs.hpp
 * @brief `F4SPairSet` --- priority-queue + pruning logic for F4 S-pairs.
 *
 * Declares `F4SPairSet`, the structure that manages the S-pair
 * worklist for the F4 inner loop. The pair records live in
 * `std::vector<spair> mSPairs`; an
 * `std::priority_queue<size_t, std::vector<size_t>, SPairCompare>
 * mSPairQueue` holds indices into that vector and `SPairCompare`
 * orders them lowest-degree-first, breaking ties by the spair's
 * `i` field (one of the two GB indices that form the pair ---
 * not the lcm). `make_spair(type, deg, i, j)` pushes a new
 * `spair` onto `mSPairs` and bump-allocates space for its lcm
 * monomial out of `MemoryBlock mSPairLCMs`; the lcm slot is
 * allocated correctly sized but **not initialised**, so the
 * caller must fill it. Construction of new pairs runs through
 * `pre_spair` (allocated from `F4MemoryBlock<pre_spair> PS`)
 * with a varpower scratch arena
 * `F4MemoryBlock<varpower_word> VP`.
 *
 * After a new basis element joins, `pair_not_needed(p, m)`
 * tests one pair against the chain criterion and
 * `remove_unneeded_pairs()` sweeps the queue for every pair
 * the new element subsumes (returning the count, accumulated
 * into `nsaved_unneeded`). Without these prunes the queue
 * would grow quadratically in basis size for any
 * Buchberger-style workload. The `spair` struct and
 * `SPairCompare` itself live in `f4-types.hpp`; the classical
 * `spair.hpp` counterpart powers `gbA` and friends rather than
 * F4. When compiled `WITH_TBB` the constructor takes the
 * shared `mtbb::task_arena&` so pair construction can run on
 * the same scheduler `F4GB` uses for Gauss.
 *
 * @see f4.hpp
 * @see f4-types.hpp
 * @see memblock.hpp
 */

#include <queue>                     // for std::priority_queue
#include "MemoryBlock.hpp"           // for MemoryBlock
#include "f4/f4-types.hpp"           // for spair (ptr only), gb_array, pre_...
#include "f4/memblock.hpp"           // for F4MemoryBlock
#include "f4/moninfo.hpp"            // for MonomialInfo (ptr only), packed_...
#include "f4/varpower-monomial.hpp"  // for varpower_word

/**
 * @brief S-pair scheduling queue used by `F4GB`: collects pairs, deduplicates
 * them, and hands out the next degree's worth on demand.
 *
 * @details Pre-S-pairs are built from each newly inserted GB element and
 * slotted into the working pool. `make_spair` promotes a
 * `pre_spair` into a full `spair` with its LCM and sugar degree.
 * A `std::priority_queue` ordered by `SPairCompare` keeps the
 * next degree at the top. `MemoryBlock`s back the monomial
 * storage so pair construction stays bump-pointer cheap, and the
 * held `mtbb::task_arena&` lets the queue share the scheduler
 * `F4GB` uses for Gauss elimination.
 */
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

  // discard all of the spairs in the current set degree
  void discardSPairsInCurrentDegree();
  
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
