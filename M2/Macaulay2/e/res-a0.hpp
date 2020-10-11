// Copyright 1996.  Michael E. Stillman

#ifndef _res2_hh_
#define _res2_hh_

#include "style.hpp"
#include "intarray.hpp"
#include "matrix.hpp"
#include "monideal.hpp"
#include "poly.hpp"
#include "comp-res.hpp"

struct res2_pair;
class res2_comp;
class res2_poly;

#include "res-a0-poly.hpp"

// States of the resolution computation
enum {
  RES_SKELETON,  // Either at beginning of skeleton, or in the middle
                 // if next_pair is null, then at the beginning
  RES_MONORDER,  // Need to set the monomial order, which is always
                 // first use total degree, then compare_num of lead component.
  RES_MONIDEAL,  // At the beginning of, or in th emiddle of
                 // computing the resolution of the monomial ideal
  RES_REDUCTIONS,  // Done with the skeleton, doing computing reductions
                   // next_pair: next pair to (possibly) reduce
                   // if null: at the beginning of reductions.
  RES_DONE,        // If done, given length and degree limit
  RES_COMPLETE     // Completely done with resolution
};

const int FLAGS_SORT = 7;     // mask for comparison type
const int FLAGS_DESCEND = 8;  // first 12 bits are for the two comparsion types
const int FLAGS_REVERSE = 16;
const int FLAGS_DEGREE = 32;
const int FLAGS_LEVEL = (1 << 13);  // bit 13
const int FLAGS_AUTO = (3 << 14);   // bit 14,15
const int SHIFT_AUTO = 14;
const int FLAGS_GEO = (1 << 16);          // bit 16
const int FLAGS_DEGREELEVEL = (1 << 17);  // bit 17
const int FLAGS_LEVEL_STRIP =
    (1 << 18);  // bit 18  only valid in level by level.

const int SORT_SKELETON = 1;
const int SORT_MONORDER = 2;
const int SORT_REDUCTIONS = 3;

const int COMPARE_LEX = 0;
const int COMPARE_LEX_EXTENDED = 1;
const int COMPARE_LEX_EXTENDED2 = 2;
const int COMPARE_ORDER = 3;
const int COMPARE_MONORDER = 4;

const int COMPUTE_SKELETON = 0;
const int COMPUTE_MONORDER = 1;
const int COMPUTE_MONOMIAL_RES = 2;
const int COMPUTE_RES = 3;

struct auto_reduce_node : public our_new_delete
{
  auto_reduce_node *next;
  res2term *pivot;
  res2_pair *p;
};

struct res2_level : public our_new_delete
// Collection of pairs all at same syzygy level
{
  res2_pair *pairs;
  res2_pair *next_pair;  // either the next to compute skeleton wise, or the
  // next pair in the reduce chain, either during monideal,
  // or actual res computation
  int state;

  int npairs;
  int nleft;
  int nminimal;
  int nthrown;  // Number of pairs (that would be in this list)
                // that were thrown out, because of the hard_degree_limit.

  res2_level() : pairs(NULL), npairs(0), nleft(0), nminimal(0) {}
};

/**
    @ingroup res

    @brief One of the Resolution computations, based on Schreyer and Lascala.
*/
class res2_comp : public ResolutionComputation
{
  // Base ring and input
  const PolynomialRing *P;
  res2_poly *R;
  const Monoid *M;
  const Ring *K;
  const Matrix
      *generator_matrix;  // Input matrix of generators, needs to be a GB.

  stash *res2_pair_stash;
  stash *mi_stash;

  VECTOR(res2_level *) resn;  // The resolution itself

  // Degree and length limits, monomial size limit
  VECTOR(res2_pair *) base_components;

  int lodegree;  // Base degree
  int hidegree;  // Highest (slanted) degree appearing (offset from lodegree).
  int hard_degree_limit;  // Pairs of slanted degree > this+1 are thrown away
                          // Although the number of pairs thrown away is kept
                          // in resn.  This number may be increased or decreased
                          // throughout the life of the computation.
  bool have_degree_limit;  // If there is no hard degree limit set.
  // If not, all pairs are kept, otherwise, pairs in degree
  // too high are thrown away.

  int length_limit;  // Pairs of level > this+1 are not computed, and
                     // pairs in this level, if supposedly minimal, are
                     // set instead to SYZ2_MAYBE_MINIMAL.  This number
                     // may be either increased or decreased during the
                     // computation.
                     // Note: the resolution may be longer than this
                     // number.  Use resn.length() to get the actual length.

  int projdim;  // The max level of a pair whose type is SYZ2_MINIMAL

  int regularity;  // Set once known...

  int max_mon_degree;  // This is the largest degree than can be represented
                       // as a least common multiple.  Any higher degree found
                       // will cause the computation to exit with COMP_RESIZE

  int next_component;
  int next_pair_num;

  // Compare values for the three sorts that need to be done
  int skeleton_sort;
  int monorder_sort;
  int reduction_sort;

  // Algorithm choice:
  unsigned char
      do_by_level;  // if != 0, use this.  If == 2, use the strip components
                    // optimization.
  unsigned char do_by_degree;
  unsigned char
      use_respolyHeaps;  // 0=don't use, 1=use (and implies auto_reduce>=1)
  int auto_reduce;       // 0=none, 1=partial, 2=full

  // Statistics
  int nleft;
  int npairs;
  int nminimal;
  int total_reduce_count;

  // More statistics, just for interest
  int n_ones;
  int n_unique;
  int n_others;

  // byte sizes for allocating temp exp vectors and monomials on the stack
  size_t exp_size;
  size_t monom_size;

  // Variables for compare_res2_pairs
  int compare_type;
  int compare_use_degree;
  int compare_use_descending;
  int compare_use_reverse;

  int find_ring_divisor(const int *exp, ring_elem &result) const;
  int find_divisor(const MonomialIdeal *mi, const int *exp, res2_pair *&result);
  res2_pair *reduce(res2term *&f,
                    res2term *&fsyz,
                    res2term *&pivot,
                    res2_pair *p);
  res2_pair *reduce2(res2term *&f,
                     res2term *&fsyz,
                     res2term *&pivot,
                     res2_pair *p);
  res2_pair *reduce3(res2term *&f,
                     res2term *&fsyz,
                     res2term *&pivot,
                     res2_pair *p);
  res2_pair *reduce4(res2term *&f,
                     res2term *&fsyz,
                     res2term *&pivot,
                     res2_pair *p);
  res2_pair *reduce_by_level(res2term *&f, res2term *&fsyz);
  res2_pair *reduce_heap_by_level(res2term *&f, res2term *&fsyz);
  res2term *s_pair(res2term *fsyz) const;

  void do_auto_reductions(res2_pair *p, auto_reduce_node *au);

  void handle_pair(res2_pair *p);
  void handle_pair_by_level(res2_pair *p);
  void handle_pair_by_degree(res2_pair *p);

  int sort_value(res2_pair *p, const int *sort_order) const;
  int compare_res2_pairs(res2_pair *f, res2_pair *g) const;
  res2_pair *merge_res2_pairs(res2_pair *f, res2_pair *g) const;
  void sort_res2_pairs(res2_pair *&p) const;
  void sort_skeleton(res2_pair *&p);
  void sort_monorder(res2_pair *&p);
  void sort_reduction(res2_pair *&p);

  void multi_degree(const res2_pair *q, int *result) const;

  res2_pair *new_base_res2_pair(int i);  // level 0
  res2_pair *new_res2_pair(int i);       // level 1
  res2_pair *new_res2_pair(res2_pair *first,
                           res2_pair *second,
                           const int *basemon);
  void insert_pair(res2_pair *p);

  //////////////////////////////////////////////
  //  Initiating the computation ///////////////
  //////////////////////////////////////////////
 private:
  void remove_res2_pair(res2_pair *p);
  void remove_res2_level(res2_level *lev);

  void initialize(const Matrix *mat,
                  int LengthLimit,
                  bool UseDegreeLimit,
                  int SlantedDegreeLimit,
                  int SortStrategy);

 public:
  res2_comp(const Matrix *m,
            int LengthLimit,
            bool UseDegreeLimit,
            int SlantedDegreeLimit,
            int SortStrategy);

  virtual ~res2_comp();

  void resize(const Ring *new_ring);

  //////////////////////////////////////////////
  //  Performing the calculation ///////////////
  //////////////////////////////////////////////

  void increase_level(int newmax);
  enum ComputationStatusCode skeleton(int level);
  void new_pairs(res2_pair *p);
  enum ComputationStatusCode do_pairs(int level, int degree);
  enum ComputationStatusCode do_all_pairs(int level, int degree);
  enum ComputationStatusCode do_pairs_by_level(int level);
  enum ComputationStatusCode do_pairs_by_degree(int level, int degree);

  int calc(const int *DegreeLimit,
           int LengthLimit,
           int SyzygyLimit,
           int PairLimit,
           int SyzLimitValue,
           int SyzLimitLevel,
           int SyzLimitDegree);

  bool stop_conditions_ok();

  void start_computation();

  int complete_thru_degree() const;

  //////////////////////////////////////////////
  //  Result matrices of the resolution ////////
  //////////////////////////////////////////////
 private:
  void reduce_minimal(int x,
                      res2term *&f,
                      VECTOR(res2_pair *)& elems,
                      VECTOR(res2term *)& stripped) const;

 public:
  FreeModule *free_of(int i) const;
  FreeModule *minimal_free_of(int i) const;
  Matrix *make(int i) const;
  Matrix *make_minimal(int i) const;

  const Matrix /* or null */ *get_matrix(int level)
  {
    return make_minimal(level);
  }

  const FreeModule /* or null */ *get_free(int level)
  {
    return minimal_free_of(level);
  }

  //////////////////////////////////////////////
  //  Betti routines and numbers associated ////
  //  with the resolution                   ////
  //////////////////////////////////////////////
  // Betti output is a flattened array of  /////
  // length                                /////
  // (high_degree() - low_degree() + 1)    /////
  //    * (max_level() + 1)                /////
  // The first row of the betti display is /////
  // given first, then the second, etc     /////
  //////////////////////////////////////////////

  M2_arrayint betti_skeleton() const;
  M2_arrayint betti_remaining() const;
  M2_arrayint betti_minimal() const;
  M2_arrayint betti_nmonoms() const;

  M2_arrayint get_betti(int type) const;

  //////////////////////////////////////////////
  //  Debugging ////////////////////////////////
  //////////////////////////////////////////////

  void display_order(buffer &o, int sortval) const;
  void text_out(const res2_pair *p) const;
  void text_out(buffer &o, const res2_pair *p) const;
  void stats() const;

  void text_out(buffer &o) const;

  //////////////////////////////////////////////
  //  Infrastructure ///////////////////////////
  //////////////////////////////////////////////

 public:
  res2_comp *cast_to_res2_comp() { return this; }
  const Ring *get_ring() const { return P; }
  const Monoid *getMonoid() const { return M; }
  const Ring *getCoefficientRing() const { return K; }
  const Monoid *degree_monoid() const { return P->degree_monoid(); }
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
