/* Copyright 2005-2023, The Macaulay2 group */

#pragma once

#include "../MemoryBlock.hpp"
#include "MonomialView.hpp"
#include <vector>

namespace newf4 {
  using MonomialIndex = uint32_t;  // 0 means undefined.  Valid values are > 0.
  //using MonomialInt = uint32_t;
  using ComponentIndex = int;
  using HashInt = uint64_t;

  // MonomialView: varpower type monomial (as in NC) (i.e. stored sparsely, length of a monomial is not constant.)
  // MonomialIndex: some int type.
  // HashTable.
  //  usual ops: creation, reset, findOrInsert (returns MonomialIndex)
  //  hashvalue(MonomialView).
  //  monomialAtIndex(MonomialIndex, HashTable) -> (range of MonomialView)
  //  iterator/range(MonomialIndex, HashTable)
  //  
  // hashtable for monomials in all polynomials in the GB basis.
  // keeps a vector of pointers to monomials.
  //   also keeps a 
  // MonomialIndex: int index of this monomial.
  
  // HashTable for monomials in ring.
  // 1. hash table (size 2^N)
  // 2. std::vector<MonomialIdx>, or std::vector<int32_t*> points into Memoryblock.
  // 3. MemoryBlock<int32_t>
  //
  // struct MonomiaIndex { int32_t* first; }, or struct MonomialIndex { int32_t idx; }
  // Operations:
  //  MonomialHashTable()
  //  reset()
  //  std::pair<value result, bool> findOrInsert(value m)
  //  monomialAtIndex(idx) // returns MonomialView, or range...
  // Requires:
  //   hashFunction(MonomialView).
  //   monomialSize(MonomialView).
  // Question: where to store hash value?

  // This doesn't work.  What changes are needed for that?
  // template<typename T>
  // concept MonoidHashable {
  //   auto hash(T a) -> uint64_t;
  //   auto eq(T a, T b) -> bool;
  //   auto show(T a) -> std::string;
  // };
    
  struct HashTableStats
  {
    unsigned long n_calls_find;
    unsigned long n_clashes;
    unsigned long max_run_length;
    unsigned long monequal_count;
    unsigned long monequal_fails;

    HashTableStats()
      : n_calls_find(0),
        n_clashes(0),
        max_run_length(0),
        monequal_count(0),
        monequal_fails(0)
    {
    }

    void dump() const;
  };

  // This class depends on properties of MonomialView's.
  // monom.size()
  // monom == monom2

  class MonomialHashTable
  {
  public:
    MonomialHashTable(int log2size = 16);

    /// Frees all space from the table, including the the monomials
    /// themselves.  Tough shit if you are pointing to any of these
    /// monomials...
    ~MonomialHashTable();

    // Clear out the hash table, resetting all values to 0, and
    // all stats values back to 0.
    // BUT: the size is kept the same.
    void reset();

    /// Essentially the previous case when monomial(n) = monomial 1.
    auto find(const MonomialView& m, HashInt mhash) -> MonomialIndex;

    /// Simple function which returns the monomial data pointed to at index m.
    /// If m is out of range: throws an error.
    auto monomialAt(MonomialIndex m) const -> MonomialView
    {
      return static_cast<const MonomialView>(
          MonomialView {mMonomialPointers[m]});
    }

    /// The actual number of monomials in the table
    auto size() const -> size_t { return mMonomialPointers.size() - 1; } // -1 because 0 index is unused.

    /// stats and debugging information.
    void dump() const;
    void dumpBuckets() const;    
  private:
    void reInsert(MonomialIndex i);
    void grow();
  private:
    // Backing storage
    MemoryBlock mMonomialSpace;
    std::vector<MonomialView> mMonomialPointers; // First element is ignored.
    std::vector<HashInt> mHashValues;

    // hash table itself.
    unsigned int mLog2Size; // size of table is 2^mLog2Size
    unsigned long mHashMask; // 2^mLog2Size - 1: mask with this to get the 
    unsigned long mThreshold; // when #elements in the table (size()) is >= this value, we grow the table.
    std::vector<MonomialIndex> mBuckets; // each bucket contains: either 0, or MonomialIndex >= 1.

    HashTableStats mStats;
  };



};
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
