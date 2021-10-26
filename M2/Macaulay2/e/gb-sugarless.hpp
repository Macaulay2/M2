// Copyright 1996  Michael E. Stillman
#ifndef _gbinhom_hh_
#define _gbinhom_hh_

#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp-gb.hpp"
#include "reducedgb.hpp"

#include "spair.hpp"
#include "matrix-con.hpp"
class GBWeight;

/**
    @ingroup gb

    @brief A Groebner basis computation class for inhomogeneous input modules.

    Works for homogeneous modules too.  Poor performance in many cases, but
   dramatically good
    performance in others.
*/
class GBinhom_comp : public GBComputation
{
 private:
  // Ring information
  GBRing *GR;
  GBWeight *weightInfo_;
  const PolynomialRing *originalR;
  const Monoid *M;  // flattened monomials
  const Ring *K;    // flattened coefficients

  const FreeModule *F;
  const FreeModule *Fsyz;

  s_pair_heap *spairs;

  gb_elem *gb;  // The minimal GB so far encountered
                // This is a list with a dummy head.

  gb_elem *gbLarge;

  ReducedGB *minimal_gb;
  bool minimal_gb_valid;

  VECTOR(MonomialIdeal *) monideals;  // baggage for each is 'gb_elem *'
                                     // This is the 'large' GB

  // Syzygies collected
  MatrixConstructor syz;

  // statistics information
  int n_gb;
  int n_syz;
  int n_subring;
  int n_pairs;
  int n_computed;
  int last_gb_num;
  int n_saved_gcd;
  int n_saved_lcm;

  // Syzygy type
  int collect_syz;  // 0 or 1
  int n_comps_per_syz;
  char is_ideal;
  int strategy;

  int need_resize;

 private:
  void set_up0(const Matrix *m, int csyz, int nsyz, M2_arrayint gb_weights);
  void set_up(const Matrix *m,
              int csyz,
              int nsyz,
              M2_arrayint gb_weights,
              int strategy);

  // S-pair control
  s_pair *new_var_pair(gb_elem *p, const int *lcm);
  s_pair *new_ring_pair(gb_elem *p, const int *lcm);
  s_pair *new_s_pair(gb_elem *p, gb_elem *q, const int *lcm);
  s_pair *new_gen(int i, gbvector *f, ring_elem denom);
  void remove_pair(s_pair *&p);
  int mark_pair(gb_elem *p, gb_elem *q) const;

  int search(const int *exp, int comp, gb_elem *&result);
  int compare(const gb_elem *p, const gb_elem *q) const;
  void find_pairs(gb_elem *p);
  void compute_s_pair(s_pair *p);
  int gb_reduce(gbvector *&f, gbvector *&fsyz);
  int gb_geo_reduce(gbvector *&f, gbvector *&fsyz);
  void gb_insert(gbvector *f, gbvector *fsyz, int minlevel);
  void inter_reduce(gb_elem *&gens);

  ComputationStatusCode computation_complete() const;

  int s_pair_step(s_pair *p);

  void resize(int nbits);

  void minimalize_gb();

 public:
  // An honest GB computation
  GBinhom_comp(const Matrix *m,
               int collect_syz,
               int n_syz,
               M2_arrayint gb_weights,
               int strategy);
  ~GBinhom_comp();

  virtual void remove_gb() {}
  // Performing the computation
  int calc(const int *deg, const intarray &stop_conditions);

  // Adding generators
  void add_gens(int lo, int hi, const Matrix *m);

  // reduction
  Matrix *reduce(const Matrix *m, Matrix *&lift);

  // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
  Matrix *min_gens_matrix();
  Matrix *initial_matrix(int n);
  Matrix *gb_matrix();
  Matrix *change_matrix();
  Matrix *syz_matrix();
  void debug_out(s_pair *q) const;
  void debug_pairs_out(gb_elem *p) const;
  void debug_pairs() const;
  void debug_out(buffer &o, s_pair *q) const;
  void debug_pairs_out(buffer &o, gb_elem *p) const;
  void debug_pairs(buffer &o) const;

  void stats() const;

  //////////////////////////
  // Computation routines //
  //////////////////////////
  virtual bool stop_conditions_ok() { return true; }
  static GBinhom_comp *create(const Matrix *m,
                              M2_bool collect_syz,
                              int n_rows_to_keep,
                              M2_arrayint gb_weights,
                              int strategy,
                              M2_bool use_max_degree,
                              int max_degree);

  virtual int kind() { return 232; }  // FIX THIS!!
  void start_computation();

  virtual const PolynomialRing *get_ring() const { return originalR; }
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
