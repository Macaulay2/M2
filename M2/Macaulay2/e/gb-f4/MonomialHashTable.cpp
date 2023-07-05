#include "MonomialHashTable.hpp"
#include <iostream>

namespace newf4 {

  void HashTableStats::dump() const
  {
    std::cout << "  number of calls   = " << n_calls_find << std::endl;
    std::cout << "  number of clashes = " << n_clashes << std::endl;
    std::cout << "  max run length    = " << max_run_length << std::endl;
    std::cout << "  monequal calls    = " << monequal_count << std::endl;
    std::cout << "  monequal false    = " << monequal_fails << std::endl;
  }

  // A private function used in grow().
  // Note that the monomial already exist in mMonomialPointers (and has storage still)
  // but we need to re-insert its value into mBuckets.
  void MonomialHashTable::reInsert(MonomialIndex i)
  {
    auto hashval = mHashValues[i];
    auto hash = hashval & mHashMask;
    while (mBuckets[hash] != 0)
      {
        hash++;
        mStats.n_clashes++;
        if (hash == mBuckets.size()) hash = 0;
      }
    mBuckets[hash] = i;
  }
  
  void MonomialHashTable::grow()
  {
    // Increase logsize, reset fields, and repopulate new hash table.
    //  if (M2_gbTrace >= 2) dump();

    std::cout << "Growing hash table..." << std::flush;
    dump();
    mLog2Size++;
    mThreshold *= 2;
    mHashMask = (1 << mLog2Size) - 1;
    std::vector<MonomialIndex> newBuckets(1<<mLog2Size, 0);
    std::swap(mBuckets, newBuckets);
    
    // Repopulates the mBuckets vector
    for (size_t i = 1; i < mMonomialPointers.size(); ++i)
      reInsert(i);

    std::cout << "...Done" << std::endl;
  }

  //////////////////////////////
  // Interface functions ///////
  //////////////////////////////
  
  MonomialHashTable::MonomialHashTable(int log2size)
  : mLog2Size(log2size),
    mHashMask((1<<log2size)-1),
    mThreshold((1<<log2size)  >> 4), // ouch
    mBuckets(1<<log2size, 0) // set to a vector of 2^log2size 0's.
  {
    mMonomialPointers.push_back(MonomialView(nullptr));
    mHashValues.push_back(0);
  }

  /// Frees all space from the table, including the the monomials
  /// themselves.  Tough shit if you are pointing to any of these
  /// monomials...
  MonomialHashTable::~MonomialHashTable()
  {
  }
  
  // Clear out the hash table, resetting all values to 0, and
  // all stats values back to 0.
  // BUT: the size is kept the same.
  void MonomialHashTable::reset()
  {
    std::fill(mBuckets.begin(), mBuckets.end(), 0);
    mMonomialSpace.deallocateAll();
    mStats = HashTableStats();
  }

  /// Essentially the previous case when monomial(n) = monomial 1.
  auto MonomialHashTable::find(const MonomialView& m, HashInt mhash) -> MonomialIndex
  {
    mStats.n_calls_find++;
    if (mMonomialPointers.size() >= mThreshold) grow();
    auto hash = mhash & mHashMask;
    long run = 0;
    while (mBuckets[hash] != 0)
      {
        MonomialIndex current = mBuckets[hash];
        if (mhash == mHashValues[current])
          {
            // likely a match.  But need to check equality first
            mStats.monequal_count++;
            if (m == mMonomialPointers[current])  // this == is a required MonomialView method!
              {
                // Already in the table
                if (run > mStats.max_run_length) mStats.max_run_length = run;
                return current;
              }
            mStats.monequal_fails++;
          }
        ++hash;
        ++run;
        mStats.n_clashes++;
        if (hash == mBuckets.size()) hash = 0;
      }
    if (run > mStats.max_run_length) mStats.max_run_length = run;
    mBuckets[hash] = mMonomialPointers.size(); // index of the new element.
    mMonomialPointers.emplace_back(m, mMonomialSpace);
    mHashValues.push_back(mhash);
    return mBuckets[hash];
  }

  auto MonomialHashTable::dump() const -> void
  {
    std::cout << "--hash table info--" << std::endl;
    std::cout << "  size of hashtable = " << mBuckets.size() << std::endl;
    std::cout << "  threshold         = " << mThreshold << std::endl;
    std::cout << "  number of monoms  = " << size() << std::endl;
    mStats.dump();
  }

  auto MonomialHashTable::dumpBuckets() const -> void
  {
    for (long i=0; i < mBuckets.size(); ++i)
      {
        if (i % 100 == 0) std::cout << std::endl;
        std::cout << mBuckets[i] << " ";
      }
    std::cout << std::endl;
  }

};

// Local Variables:
// indent-tabs-mode: nil
// End:
