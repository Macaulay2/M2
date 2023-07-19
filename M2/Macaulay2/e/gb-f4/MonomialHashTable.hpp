/* Copyright 2005-2023, The Macaulay2 group */

#pragma once

#include "MonomialTypes.hpp"
#include "../MemoryBlock.hpp"
#include "MonomialView.hpp"
#include <vector>

namespace newf4 {

  class MonomialHashFunction {
  private:
    std::vector<HashInt> mHashValues;
  public:
    MonomialHashFunction()
      : mHashValues({
        12550986463692465404ul, 3911555212215091238ul, 15090669942851505316ul,
        16174113364685515424ul, 18172793978567602378ul, 4970727551569665824ul,
        15244287395755336378ul, 3641586221293608170ul, 5697307520845005385ul,
        17982501052917221133ul, 4205210476184990958ul, 3995014217224167515ul,
        10391875845945764299ul, 17483720614571824287ul, 1115562083531405255ul,
        7842315096810324507ul, 673864007402015535ul, 15878473700446701422ul,
        15632675738063166334ul, 17700395182034373329ul
        })
    {
    }

    auto hashValue(const MonomialView& w) -> HashInt
    {
      HashInt val = 0;
      for (auto i = w.begin() + 1; i != w.end(); i += 2)
        // TODO: check that the variables are in range in mHashValues.
        //  if not, increase the size by adding in new HashInt's (uint64_t).
        val += mHashValues[*i] * (*(i+1));
      return val;
    }
  };


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
    MonomialHashTable(int log2size = 17); // use 16??

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
    auto find(const MonomialView& m) -> MonomialIndex
    {
      return find(m, mHashFunction.hashValue(m));
    }

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

    // Hash function
    MonomialHashFunction mHashFunction;
    
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
