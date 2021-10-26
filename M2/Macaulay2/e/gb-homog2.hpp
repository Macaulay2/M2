// Copyright 1996  Michael E. Stillman
#ifndef _gb_hh_
#define _gb_hh_

#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"

#include "comp-gb.hpp"

#include "spair.hpp"
#include "gbweight.hpp"

class hilb_comp;

// These are the possible states of a GB computation
const int GB_COMP_NEWDEGREE = 1;
const int GB_COMP_NEED_RESIZE = 2;
const int GB_COMP_S_PAIRS = 3;
const int GB_COMP_GENS = 4;
const int GB_COMP_AUTO_REDUCE = 5;
const int GB_COMP_NEWPAIRS = 6;
const int GB_COMP_DONE = 7;

/**
    @ingroup gb

    @brief A Groebner basis computation class for homogeneous input modules.
*/
class GB_comp : public GBComputation
{
 private:
  // Ring information
  const PolynomialRing *originalR;
  GBRing *_GR;
  const GBWeight *weightInfo_;
  const Monoid *_M;
  const Ring *_K;

  const FreeModule *_F;
  const FreeModule *_Fsyz;
  int _this_degree;

  // state information
  int _state;        // GB_COMP_*
  int _ar_i, _ar_j;  // State info used for autoreduction
  int _np_i;         // State info used for new pairs

  s_pair_heap *_spairs;
  s_pair_heap *_gens;

  VECTOR(gb_elem *) _gb;
  VECTOR(monideal_pair *) _monideals;  // baggage for each is 'gb_elem *'

  // Syzygies collected
  VECTOR(gbvector *) _syz;

  // statistics information, much is kept with the s_set
  int _n_gb;
  int _n_gens_left;
  int _n_pairs_computed;
  int _n_subring;
  int _n_syz;  // Same as _syz.length()

  int _n_saved_gcd;
  int _n_reductions;

  // Syzygy type
  bool _collect_syz;
  int _n_rows_per_syz;
  bool _is_ideal;
  int _strategy;  // USE_SORT, STRATEGY_LONGPOLYNOMIALS, or both

  // Hilbert function information
  bool _use_hilb;
  bool _hilb_new_elems;   // True if any new elements since HF was last computed
  int _hilb_n_in_degree;  // The number of new elements that we expect to find
                          // in this degree.
  int _n_saved_hilb;
  const RingElement
      *_hf_orig;  // The Hilbert function that we are given at the beginning
  RingElement
      *_hf_diff;  // The difference between hf_orig and the computed hilb fcn
 private:
  void initialize0(const Matrix *m, int csyz, int nsyz, M2_arrayint gb_weights);
  void initialize(const Matrix *m,
                  int csyz,
                  int nsyz,
                  M2_arrayint gb_weights,
                  int strategy);

  void initialize_forced(const Matrix *m,
                         const Matrix *gb,
                         const Matrix *mchange);

  // S-pair control
  s_pair *new_var_pair(gb_elem *p, const int *lcm);
  s_pair *new_ring_pair(gb_elem *p, const int *lcm);
  s_pair *new_s_pair(gb_elem *p, gb_elem *q, const int *lcm);
  s_pair *new_gen(int i, gbvector *f, ring_elem denom);
  void remove_pair(s_pair *&p);

  void find_pairs(gb_elem *p);
  void compute_s_pair(s_pair *p);
  void gb_reduce(gbvector *&f, gbvector *&fsyz);
  void gb_geo_reduce(gbvector *&f, gbvector *&fsyz);
  void gb_insert(gbvector *f, gbvector *fsyz, int ismin);

  int gb_sort_partition(int lo, int hi);
  void gb_sort(int lo, int hi);

  // Hilbert function use
  void flush_pairs(int deg);
  RingElement /* or null */ *compute_hilbert_function() const;

  int next_degree();
  ComputationStatusCode computation_is_complete() const;

  bool new_pairs_step();
  int s_pair_step();
  int gen_step();
  bool auto_reduce_step();

  GB_comp() {}
  virtual ~GB_comp();

  virtual void remove_gb() {}
  // Adding generators
  void add_gens(int lo, int hi, const Matrix *m);

  void debug_out(s_pair *q) const;

  virtual bool stop_conditions_ok() { return true; }
 public:
  //////////////////////////
  // Computation routines //
  //////////////////////////
  static GB_comp *create(const Matrix *m,
                         M2_bool collect_syz,
                         int n_rows_to_keep,
                         M2_arrayint gb_weights,
                         int strategy,
                         M2_bool use_max_degree,
                         int max_degree);

  static GB_comp *create_forced(const Matrix *m,
                                const Matrix *gb,
                                const Matrix *mchange);

  virtual int kind() { return 1; }
  void start_computation();

  virtual const PolynomialRing *get_ring() const { return originalR; }
  virtual Computation /* or null */ *set_hilbert_function(const RingElement *h);

  virtual const Matrix /* or null */ *get_gb();

  virtual const Matrix /* or null */ *get_mingens();

  virtual const Matrix /* or null */ *get_change();

  virtual const Matrix /* or null */ *get_syzygies();

  virtual const Matrix /* or null */ *get_initial(int nparts);

  virtual const Matrix /* or null */ *get_parallel_lead_terms(M2_arrayint w);

  virtual const Matrix /* or null */ *matrix_remainder(const Matrix *m);

  virtual M2_bool matrix_lift(const Matrix *m,
                              const Matrix /* or null */ **result_remainder,
                              const Matrix /* or null */ **result_quotient);

  virtual int contains(const Matrix *m);

  virtual void text_out(buffer &o) const;
  /* This displays statistical information, and depends on the
     M2_gbTrace value */

  virtual int complete_thru_degree() const;
  // The computation is complete up through this degree.
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
