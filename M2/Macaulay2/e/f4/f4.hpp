// Copyright 2005 Michael E. Stillman

#ifndef __f4gb_h_
#define __f4gb_h_

// My implementation of Faugere's linear algebra GB routines.  Also includes
// free resolution code.

// Template parameters include:
//    coefficient ring arithmetic
//    packed_monomial
//    exponents
//    varpower_monomial
// Types to define
//    MonomialLookupTable
//      a. find_divisor(packed_monomial, comp) --> integer whose lead term
//      divides packed_monomial
//      b. insert(packed_monomial, comp, index)
//    packed_monomial
//      implemented as packed exponent vector, perhaps with weight vector(s).
//        monomial compare is done on a order by order basis (i.e. a different
//        function)
//        each packed_monomial also includes its hash code.
//        multiplication: checked, and unchecked.
//        quotient, lcm, gcd?
//    coefficient arithmetic
//      coeff types include: ZZ/p, GF(q), ZZ, QQ, QQ[x]/f(x), other tower
//      extensions?
//                           kk(a,...,z)
//      this is done at "low" level: we try to do as many ZZ ops as possible
//      before reducing mod p.
//      required ops: add, mult, negate, is_zero, divide(in ZZ/p,GF), what else?
//      also required: translation to/from M2 ring_elem.
//    spair
//      spairs and gens
//      make_spair, make_gen
//      where to store the packed_monomial's?
//    SPairSet
//      insert spair
//      prepare for next degree
//      next spair
//      remove redundant pairs
//    Minimalize a set of spairs
//      Used in finding new spairs.
//    Creation of the matrix
//      This is the heart of the matter.
//      Need:
//        hashtable for column monomials (packed_monomial,comp)'s.
//        sort the list of columns
//        column header
//        row header
//        several types of rows:
//         (a) one that is essentially a list of monomials
//         (b) one after arithmetic has been performed
//         how should these rows be implemented?
//       row reduction of the matrix (using ZZ/p to get QQ).
//       rows --> new gb elements
//    gb itself
//    generators
//
//    syzygies via this method
//    minimalization of these syzygies
//
//    keep in mind:
//      skew commutative multiplication
//      Schreyer orders
//      quotient rings
//      Hilbert function use
//

#include "engine-exports.h"         // for M2_arrayint, M2_bool
#include "f4-types.hpp"             // for gb_array, MonomialLookupTable
#include "f4/moninfo.hpp"           // for packed_monomial, MonomialInfo
#include "f4/f4-spairs.hpp"         // For F4SPairSet
#include "interface/computation.h"  // for ComputationStatusCode, StopCondit...
#include "m2tbb.hpp"                // for TBB
#include "memblock.hpp"             // for F4MemoryBlock
#include "monhashtable.hpp"         // for MonomialHashTable
#include "newdelete.hpp"            // for our_new_delete
#include "MemoryBlock.hpp"          // for MemoryBlock<T>
#include <ctime>                    // for clock, CLOCKS_PER_SEC, clock_t

class FreeModule;
class HilbertController;
class RingElement;
class VectorArithmetic;

/////////////////////////////////////////////////////////////////////////////

class F4GB : public our_new_delete
{
  // Basic required information
  const VectorArithmetic* mVectorArithmetic;
  const MonomialInfo *mMonomialInfo;
  const FreeModule *mFreeModule;
  M2_arrayint mWeights;  // The length of this is the number of variables, each
                        // entry is positive.
  M2_arrayint component_degrees;  // Degree of each free module element.
  // Also need Schreyer order info sometimes

  // Options and information about the computation
  long n_lcmdups;
  long n_pairs_computed;
  long n_reduction_steps;
  int n_gens_left;
  int n_subring;
  int complete_thru_this_degree;
  int this_degree;  // The current degree we are working on
  bool is_ideal;    // true if the rank of F is one.

  // Hilbert function information
  HilbertController *hilbert;  // null if not being used

  // Monomial order information.  Should this be in M?

  // The main players in the computation
  gb_array mGenerators;
  gb_array mGroebnerBasis;
  MonomialLookupTable mLookupTable;  // (monom,comp) --> index into mGroebnerBasis

  // The matrix and its construction
  int next_col_to_process;
  coefficient_matrix *mat;
  MonomialHashTable<MonomialInfo> mMonomialHashTable;
  F4MemoryBlock<monomial_word> mMonomialMemoryBlock;
  monomial_word *next_monom;  // valid while creating the matrix
  
  MemoryBlock mComponentSpace; // stop-gap for use with VectorArithmetic and Mem. (TODO: what does this comment mean?)

  // cumulative timing info
  double clock_sort_columns;
  clock_t clock_gauss;
  double mGaussTime;
  double mParallelGaussTime;
  double mSerialGaussTime;
  double mTailReduceTime;
  double mNewSPairTime;
  double mInsertGBTime;
  clock_t clock_make_matrix;

  struct MacaulayMatrixStats
  {
  public:
    // #rows/cols
    long mTopAndLeft = 0;
    long mBottom = 0;
    long mRight = 0;
    
    // #entries
    long mAEntries = 0; // but not the diagonals?
    long mBEntries = 0;
    long mCEntries = 0;
    long mDEntries = 0;
    
    // memory usage: column info, row info, all the entries, hash table?
    long mColumnInfo = 0;
    long mRowInfo = 0;
    long mEntries = 0;
 
    void display(buffer& o);
    void display();
  };

  MacaulayMatrixStats macaulayMatrixStats() const;  // For debugging info
  
#if defined (WITH_TBB)
  int mNumThreads;
  mtbb::task_arena mScheduler;

  mtbb::task_arena& getScheduler() { return mScheduler; }
#endif

  F4SPairSet mSPairSet;

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

  void loadSPairRow(spair *p);
  void loadReducerRow(spair *p);
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

  bool is_pivot_row(int index) const;
  
  void gauss_reduce(bool diagonalize);
  bool gauss_reduce_row(int index, ElementArray& gauss_row);
  void tail_reduce();

  void row_to_dense_row(int r, int &first, int &last);
  void subtract1(int r, int &first, int &last);
  void reduce1(int r, int &first, int &last);
  void dense_row_to_row(int r, int &first, int &last);

  void new_GB_elements();

  void insert_gb_element(row_elem &r);

  void poly_set_degrees(const GBF4Polynomial &f,
                        int &deg_result,
                        int &alpha) const; // private: used in new_generators
  
 public:
  F4GB(const VectorArithmetic* VA,
       const MonomialInfo *MI,
       const FreeModule *F,  // used for debugging only...
       M2_bool collect_syz,
       int n_rows_to_keep,
       M2_arrayint gb_weights,
       int strategy,
       M2_bool use_max_degree,
       int max_degree,
       int numThreads);

  ~F4GB();

  void set_generators(gb_array &new_gens);
  // This grabs these elements, possibly by doing a swap

  void new_generators(int lo, int hi);

  const gb_array &get_generators() const { return mGenerators; }
  gb_array &get_generators() { return mGenerators; }
  const gb_array &get_gb() const { return mGroebnerBasis; }
  gb_array &get_gb() { return mGroebnerBasis; }
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

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
