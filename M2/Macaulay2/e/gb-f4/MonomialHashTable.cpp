#include "MonomialHashTable.hpp"
#include <iostream>

namespace newf4 {

  void HashTableStats::dump() const
  {
    std::cout << "  number of calls   = " << nfind_or_insert << std::endl;
    std::cout << "  number of clashes = " << nclashes << std::endl;
    std::cout << "  max run length    = " << max_run_length << std::endl;
    std::cout << "  monequal calls    = " << monequal_count << std::endl;
    std::cout << "  monequal false    = " << monequal_fails << std::endl;
  }
  
  void MonomialHashTable::grow()
  {
    // Increase logsize, reset fields, and repopulate new hash table.
    //  if (M2_gbTrace >= 2) dump();
    
    mLog2Size++;
    mThreshold *= 2;
    mHashMask = (1 << mLog2Size) - 1;
    std::vector<MonomialIndex> newBuckets(1<<mLog2Size, 0);
    std::swap(mBuckets, newBuckets);
    
    // Repopulates the mBuckets vector
    for (size_t i = 1; i < mMonomialPointers.size(); ++i)
      insert(mMonomialPointers[i], mHashValues[i]);
  }

  // insert: an *internal* function which blindly inserts already
  // created monomials in the backing store.  Private function for grow().
  void MonomialHashTable::insert(const Monomial& m, HashInt hashval)
  {
    if (mMonomialPointers.size() >= mThreshold) grow();
    auto hash = hashval & mHashMask;
    while (mBuckets[hash] != 0)
      {
        hash++;
        mStats.nclashes++;
        if (hash == mBuckets.size()) hash = 0;
      }
    mBuckets[hash] = mMonomialPointers.size(); // index of the new element.
    mMonomialPointers.push_back(m);
    mHashValues.push_back(hashval);
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
    mMonomialPointers.push_back(Monomial(nullptr));
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
  auto MonomialHashTable::find(const Monomial& m, HashInt mhash) -> MonomialIndex
  {
    if (mMonomialPointers.size() >= mThreshold) grow();
    auto hash = mhash & mHashMask;
    while (mBuckets[hash] != 0)
      {
        MonomialIndex current = mBuckets[hash];
        if (mhash == mHashValues[current])
          {
            // likely a match.  But need to check equality first
            if (m == mMonomialPointers[current])  // this == is a required Monomial method!
              {
                // Already in the table
                return current;
              }
          }
        hash++;
        mStats.nclashes++;
        if (hash == mBuckets.size()) hash = 0;
      }
    mBuckets[hash] = mMonomialPointers.size(); // index of the new element.
    mMonomialPointers.emplace_back(m, mMonomialSpace);
    mHashValues.push_back(mhash);
    return mBuckets[hash];
  }
  
};

// Local Variables:
// indent-tabs-mode: nil
// End:
