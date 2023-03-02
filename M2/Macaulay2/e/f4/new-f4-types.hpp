/* Copyright 2005-2023, The Macaulay2 group */

#pragma once
#include "m2tbb.hpp"                      // for tbb interface
#include "../MemoryBlock.hpp"
#include "../VectorArithmetic.hpp"      // for ElementArray
#include "f4-monlookup.hpp"       // for F4MonomialLookupTableT
#include "f4-spairs.hpp"
#include "moninfo.hpp"            // for MonomialInfo, monomial_word, pac...
#include "monhashtable.hpp"            // for MonomialInfo, monomial_word, pac...
#include "hilb-fcn.hpp"

#include <iostream>

namespace newf4 {
  using MonomialIndex = uint32_t;  // 0 means undefined.  Valid values are > 0.
  using ComponentIndex = int;
  using HashInt = uint64_t;
  struct Monomial {
    int32_t* mData;
  };
  
  
  // Monomial: varpower type monomial (as in NC) (i.e. stored sparsely, length of a monomial is not constant.)
  // MonomialIndex: some int type.
  // HashTable.
  //  usual ops: creation, reset, findOrInsert (returns MonomialIndex)
  //  hashvalue(Monomial).
  //  monomialAtIndex(MonomialIndex, HashTable) -> (range of Monomial)
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
  //  monomialAtIndex(idx) // returns Monomial, or range...
  // Requires:
  //   hashFunction(Monomial).
  //   monomialSize(Monomial).
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
    unsigned long nfind_or_insert;
    unsigned long nclashes;
    unsigned long max_run_length;
    unsigned long monequal_count;
    unsigned long monequal_fails;

    HashTableStats()
      : nfind_or_insert(0),
        nclashes(0),
        max_run_length(0),
        monequal_count(0),
        monequal_fails(0)
    {
    }

    void dump() const
    {
      std::cout << "  number of calls   = " << nfind_or_insert << std::endl;
      std::cout << "  number of clashes = " << nclashes << std::endl;
      std::cout << "  max run length    = " << max_run_length << std::endl;
      std::cout << "  monequal calls    = " << monequal_count << std::endl;
      std::cout << "  monequal false    = " << monequal_fails << std::endl;
    }
  };
  
  template <typename MonoidType> 
  class MonomialHashTable
  {
  public:
    // These types are meant to come from MonoidType eventually?
    //using Monomial = typename MonoidType::Monomial; // generally, a sparse monomial over the ring.
    //using MonomialIndex = typename MonoidType::MonomialIndex; // 0 means undefined.  Valid values are > 0.
    
    MonomialHashTable(const MonoidType& M, int log2size = 16);

    /// Frees all space from the table, including the the monomials
    /// themselves.  Tough shit if you are pointing to any of these
    /// monomials...
    ~MonomialHashTable();

    // Clear out the hash table, resetting all values to 0, and
    // all stats values back to 0.
    // BUT: the size is kept the same.
    void reset();

    /// Find the monomial m * (monomial at n)
    auto find(const Monomial&m, HashInt mhash, MonomialIndex n) -> MonomialIndex;

    /// Essentially the previous case when monomial(n) = monomial 1.
    auto find(const Monomial& m, HashInt mhash) -> MonomialIndex;

    /// Simple function which returns the monomial data pointed to at index m.
    /// If m is out of range: throws an error.
    auto monomialAt(MonomialIndex m) const -> Monomial {
      return static_cast<const Monomial>(Monomial{mMonomialPointers[m]});
    }

    /// The actual number of monomials in the table
    auto size() const -> size_t { return mMonomialPointers.size() - 1; } // -1 because 0 index is unused.

    /// stats and debugging information.

  private:
    void grow();
    void insert(const Monomial& m, HashInt hashval);
  private:
    // Backing storage
    const MonoidType& mMonoid;
    MemoryBlock mMemorySpaceForMonomials;
    std::vector<Monomial> mMonomialPointers; // First element is ignored.
    std::vector<HashInt> mHashValues;

    // hash table itself.
    unsigned int mLog2Size; // size of table is 2^mLog2Size
    unsigned long mHashMask; // 2^mLog2Size - 1: mask with this to get the 
    unsigned long mThreshold; // when #elements in the table (size()) is >= this value, we grow the table.
    std::vector<MonomialIndex> mBuckets; // each bucket contains: either 0, or MonomialIndex >= 1.

    HashTableStats mStats;
  };

  template <typename MonoidType>
  void MonomialHashTable<MonoidType>::grow()
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

  template <typename MonoidType>
  void MonomialHashTable<MonoidType>::insert(const Monomial& m, HashInt hashval)
  // Perhaps 2 versions:
  // one for monomials created here:
  //   if we know it must be inserted, we add to backing store:
  //   1. intern the element already in the backing store.
  //   2. add pointer to this at end of mMonomialPointers.
  //   3. return this resulting index.
  //   ?: also set the hash value for this.
    
  // TODO: is this correct?  insert a monomial?  There should perhaps be two routines:
  // one that takes a monomial, and "interns" it: places it into the memory block,
  // adds the pointer to the list of monomials, and returns the index?
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
    mMonomialPointers.push_back(m); //TODO: does this need to be copied in.
    mHashValues.push_back(hashval);
  }
  
  //////////////////////////////
  // Interface functions ///////
  //////////////////////////////
  
  template<typename MonoidType>
  MonomialHashTable<MonoidType>::MonomialHashTable(const MonoidType& M, int log2size)
  : mMonoid(M),
    mLog2Size(log2size),
    mHashMask((1<<log2size)-1),
    mThreshold((1<<log2size)  >> 4), // ouch
    mBuckets(1<<log2size, 0) // set to a vector of 2^log2size 0's.
  {
  }

  /// Frees all space from the table, including the the monomials
  /// themselves.  Tough shit if you are pointing to any of these
  /// monomials...
  template<typename MonoidType>
  MonomialHashTable<MonoidType>::~MonomialHashTable()
  {
    // TODO: decide how to remove pointers in mBuckets.
  }
  
  // Clear out the hash table, resetting all values to 0, and
  // all stats values back to 0.
  // BUT: the size is kept the same.
  template<typename MonoidType>
  void MonomialHashTable<MonoidType>::reset()
  {
    // TODO: remove the pointers in mBuckets somehow. MonomialBacking??
    std::fill(mBuckets.begin(), mBuckets.end(), 0);
    mStats = HashTableStats();
  }

  /// Find the monomial m * (monomial at n)
  template<typename MonoidType>
  auto MonomialHashTable<MonoidType>::find(const Monomial&m, HashInt mhash, MonomialIndex n) -> MonomialIndex
  {
  }

  /// Essentially the previous case when monomial(n) = monomial 1.
  template<typename MonoidType>
  auto MonomialHashTable<MonoidType>::find(const Monomial& m, HashInt mhash) -> MonomialIndex
  {
    if (mMonomialPointers.size() >= mThreshold) grow();
    auto hash = mhash & mHashMask;
    while (mBuckets[hash] != 0)
      {
        MonomialIndex current = mBuckets[hash];
        if (mhash == mHashValues[current])
          {
            // likely a match.  But need to check equality first
            if (mMonoid.is_equal(m, mMonomialPointers[current]))
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
    mMonomialPointers.push_back(m); //TODO: does this need to be copied in.
    mHashValues.push_back(mhash);
    return mMonomialPointers.size()-1;
  }


  //////////////////////////////////////////////////////////////////////////////////

#if 0
  
  // Maybe we want a type for backing memory for monomials?
  // Basically:
  //  mult monomials: copy result in. mult_monomials(MonomialIndex, Monomial, HashInteger) -> MonomialIndex
  //  maybe lcm too?
  //  monomialAt(MonomialIndex).
  //  hashValueAt(MonomialIndex),
  //  copyMonomials?
  //  initiailze monomials with 1, all variables?.
  
using MonomialLookupTable = F4MonomialLookupTableT<int32_t>;

  //// SPair construction.
  //// Add in a new generator:
  //// 1. For all minimal GB elements, if with same component, create a PreSPair. (with lcm).
  //// 2. Minimize this set?  Sort into bins, use MonomialLookupTable?
  //// 3. Create actual SPair elements.
  //// ?? Should these LCM monomials be MonomialIndex's?  (They will be needed in a MacaulayMatrix hash table... But perhaps not yet?)
  
enum class GBType {
  InRing, // ??
  GB, // minimal GB element (so far)
  Redundant
};

enum class SPairType {
  SPair,
  Ring,
  Skew,
  Generator,
  Dead
};
  
struct GBF4Polynomial
{
  // invariant: mCoefficients length === mMonomialIndices.size() === mComponents.size()
  ElementArray mCoefficients; // needs mVectorArithmetic to operate on these.
  std::vector<MonomialIndex> mMonomialIndices;
  std::vector<ComponentIndex> mComponents; // unused in the ring case?

  // or
  std::vector<std::pair<MonomialIndex, ComponentIndex>> mMonomials;
};

struct pre_spair
{
  SPairType mType;
  int deg1;  // simple degree of quot? Used to minimalize the set...
  int j;
  bool are_disjoint;
  MonomialIndex mQuotient; // or should it be LCM?
};

struct spair
{
  SPairType mType;
  int deg; /* sugar degree of this spair */
  int i;
  int j;
  MonomialIndex lcm;
};

struct gbelem
{
  GBType mType;
  int deg;
  int alpha;  // the homogenizing degree
  GBF4Polynomial f;
};

using gb_array = std::vector<gbelem>;

// Is this any different from GBF4Polynomial?  Except for the components are integers, not indices?
struct sparse_row
{
  int len;
  ElementArray coeffs;
  int *comps; // of length len, allocated in a memory block.
};

struct row_elem
{
  // Header information
  MonomialIndex mMonomial;
  int elem;

  ElementArray mCoefficients;
  std::vector<MonomialIndex> mMonomialIndices;
};

struct column_elem
{
  MonomialIndex mMonomial;
  int head;  // which row is being used as a pivot for this column.
             // -1 means none, -2 means not set yet
};

struct MacaulayMatrix
{
  std::vector<row_elem> mRows;
  std::vector<column_elem> mColumns;

  // maybe a hash table here too?
  // and vector of ints, or monomial block, etc?
};

struct GBF4Stats
{
  long n_lcmdups;
  long n_pairs_computed;
  long n_reduction_steps;
  int n_gens_left;
  int n_subring;

  // Change these to std c++ timers...  
  double clock_sort_columns;
  clock_t clock_gauss;
  clock_t clock_make_matrix;

};

struct MonomialHashStats
{
};

// class F4SPairSet
// {
// };

class F4GB
{
  // Basic required information
  const VectorArithmetic* mVectorArithmetic;
  const MonomialInfo& mMonoid;
  const FreeModule& mFreeModule;
  std::vector<int> mWeights;  // The length of this is the number of variables, each
                        // entry is positive.
  std::vector<int> mComponentDegrees;  // Degree of each free module element.
  // Also need Schreyer order info sometimes??

  // Options and information about the computation
  GBF4Stats mStats;

  // State variables?
  int complete_thru_this_degree;
  int this_degree;  // The current degree we are working on
  bool is_ideal;    // true if the rank of F is one.

  // Hilbert function information
  HilbertController *hilbert;  // null if not being used

  // Monomial order information.  Should this be in M?

  // The main players in the computation
  gb_array gens;
  gb_array gb;
  MonomialLookupTable *lookup;  // (monom,comp) --> index into gb
  F4SPairSet mSPairs;

  // The matrix and its construction: Should not be here?
  int next_col_to_process;
  MacaulayMatrix mMatrix;

  MonomialHashTable<MonomialInfo> mMonomialHashTable; // keeps its own hash stats, and perhaps the data storage for the monomials too?
  F4MemoryBlock<monomial_word> mMonomialSpace;
  monomial_word *next_monom;  // valid while creating the matrix ??
  
  F4Mem *Mem;  // Used to allocate and deallocate arrays used in the matrix
  MemoryBlock mComponentSpace; // stop-gap for use with VectorArithmetic and Mem.

 private:
  ////////////////////////////////////////////////////////////////////
  void delete_gb_array(gb_array &g);

  void test_spair_code();  // test routine: probably will be removed

  enum ComputationStatusCode computation_is_complete(StopConditions &stop_);

  void do_spairs();

  void make_matrix();

  void reset_matrix();
  void clear_matrix();
  int new_column(packed_monomial m);
  int find_or_append_column(packed_monomial m);
  int mult_monomials(packed_monomial m, packed_monomial n);
  void load_gen(int which);
  void load_row(packed_monomial monom, int which);
  void process_column(int c);
  void process_s_pair(spair *p);
  void reorder_columns();
  void reorder_rows();

  const ElementArray& get_coeffs_array(row_elem &r);
  // If r.coeffs is set, returns that, otherwise returns the coeffs array from
  // the generator or GB element.  The resulting value should not be modified.

  bool is_new_GB_row(int row) const;
  // returns true if the r-th row has its lead term not in the current GB
  // This can be used to determine which elements should be reduced in the first
  // place
  // and also to determine if an element (row) needs to be tail reduced

  void gauss_reduce(bool diagonalize);
  void tail_reduce();

  void row_to_dense_row(int r, int &first, int &last);
  void subtract1(int r, int &first, int &last);
  void reduce1(int r, int &first, int &last);
  void dense_row_to_row(int r, int &first, int &last);

  void new_GB_elements();

  void insert_gb_element(row_elem &r);

 public:
  F4GB(const VectorArithmetic* VA,
       F4Mem *Mem0,
       const MonomialInfo *MI,
       const FreeModule *F,  // used for debugging only...
       M2_bool collect_syz,
       int n_rows_to_keep,
       M2_arrayint gb_weights,
       int strategy,
       M2_bool use_max_degree,
       int max_degree);

  ~F4GB();

  void set_generators(gb_array &new_gens);
  // This grabs these elements, possibly by doing a swap

  void new_generators(int lo, int hi);

  const gb_array &get_generators() const { return gens; }
  gb_array &get_generators() { return gens; }
  const gb_array &get_gb() const { return gb; }
  gb_array &get_gb() { return gb; }
  void set_hilbert_function(const RingElement *hf);

  enum ComputationStatusCode start_computation(StopConditions &stop_);
  // ComputationStatusCode is defined in ../engine.h

  // Debugging routines
  void show_gb_array(const gb_array &g) const;
  void show_row_info() const;
  void show_column_info() const;
  void show_matrix();
  void show_new_rows_matrix();
};
#endif


};
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
