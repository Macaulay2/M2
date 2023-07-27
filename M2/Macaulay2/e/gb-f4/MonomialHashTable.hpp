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
        15632675738063166334ul, 17700395182034373329ul,

        3647400739764648714ul, 15057857142894718214ul, 811470848562388018ul, 4213403134094065187ul,
        11972908277602038105ul, 753645190001903519ul, 8409368471645073943ul, 17434420111610391988ul,
        11290684105730860395ul, 6514428039353418497ul, 9766946806568431805ul, 12175828639099505778ul,
        16153880744947045125ul, 5266962483604231492ul, 7437584603302882318ul, 15236881761158912593ul,
        737715554071918441ul, 9712750857038529859ul, 7810988205991739696ul, 11544137928619240727ul,
        2282931637490911949ul, 7479558234611642393ul, 2992086816210816745ul, 14032643290431806479ul,
        17407261947046967157ul, 13976078027849361926ul, 3703435431725830669ul, 5190225415596727966ul,
        7326615512078281794ul, 13621935225106626626ul, 11243108442414122854ul, 11383277907705177830ul,
        1351966556830427717ul, 6210952720194152236ul, 5182286448890172674ul, 6242391044568798640ul,
        4341180720656093025ul, 7345600385861738727ul, 17930341431249554713ul, 12020897693351196045ul,
        6437449487804489292ul, 7513956725603664375ul, 1141356756031367723ul, 3599609783664644388ul
        })
    {
    }

    auto hashValue(const MonomialView& w) -> HashInt
    {
      HashInt val = 0;
      for (auto i = w.begin() + 1; i != w.end(); i += 2)
        // TODO: check that the variables are in range in mHashValues.
        //  if not, increase the size by adding in new HashInt's (uint64_t).
        val += mHashValues[(*i)%64] * (*(i+1));
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
    MonomialHashTable(int log2size = 19); // use 16??

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
