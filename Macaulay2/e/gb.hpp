// Copyright 1996  Michael E. Stillman
#ifndef _gb_hh_
#define _gb_hh_

#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp.hpp"
#include "gb_comp.hpp"

#include "spair.hpp"

class hilb_comp;

class GB_comp : public Computation
{
private:
  // Ring information
  const PolynomialRing *_originalR;
  GBRing *_GR;
  const Monoid *_M;
  const Ring *_K;

  const FreeModule *_F;
  const FreeModule *_Fsyz;
  int _this_degree;

  // state information
  int _state;  // GB_COMP_*
  int _ar_i, _ar_j;		// State info used for autoreduction
  int _np_i;			// State info used for new pairs

  s_pair_heap *_spairs;
  s_pair_heap *_gens;

  array<gb_elem *> _gb;
  array<monideal_pair *> _monideals; // baggage for each is 'gb_elem *'

  // Syzygies collected
  array<gbvector *> _syz;

  // statistics information, much is kept with the s_set
  int _n_gb;
  int _n_gens_left;
  int _n_pairs_computed;
  int _n_subring;
  int _n_syz;			// Same as _syz.length()

  int _n_saved_gcd;
  int _n_reductions;

  // Syzygy type
  bool _collect_syz;
  int _n_rows_per_syz;
  bool _is_ideal;
  int _strategy;			// USE_SORT, USE_GEOBUCKET, or both

  // Hilbert function information
  bool _use_hilb;
  bool _hilb_new_elems;	// True if any new elements since HF was last computed
  int _hilb_n_in_degree; // The number of new elements that we expect to find
			 // in this degree.
  int _n_saved_hilb;
  const RingElement *_hf_orig;	// The Hilbert function that we are given at the beginning
  RingElement *_hf_diff;		// The difference between hf_orig and the computed hilb fcn
private:
  void initialize0(const Matrix *m, int csyz, int nsyz);
  void initialize(const Matrix *m, int csyz, int nsyz, int strategy);
  void initialize_forced(const Matrix *m, 
			 const Matrix *gb, 
			 const Matrix *mchange);

  // S-pair control
  s_pair *new_var_pair(gb_elem *p, const int *lcm);
  s_pair *new_ring_pair(gb_elem *p, const int *lcm);
  s_pair *new_s_pair(gb_elem *p, gb_elem *q, const int *lcm);
  s_pair *new_gen(int i, gbvector *f, ring_elem denom);
  void remove_pair(s_pair *& p);

  void find_pairs(gb_elem *p);
  void compute_s_pair(s_pair *p);
  void gb_reduce(gbvector * &f, gbvector * &fsyz);
  void gb_geo_reduce(gbvector * &f, gbvector * &fsyz);
  void gb_insert(gbvector *f, gbvector *fsyz, int ismin);

  int gb_sort_partition(int lo, int hi);
  void gb_sort(int lo, int hi);

  // Hilbert function use
  void flush_pairs(int deg);
  RingElementOrNull *compute_hilbert_function() const;
  
  int next_degree();
  int computation_is_complete() const;

  bool new_pairs_step();
  int s_pair_step();
  int gen_step();
  bool auto_reduce_step();

  GB_comp() {}
  virtual ~GB_comp();
  // Performing the computation
  int calc();

  // Adding generators
  void add_gens(int lo, int hi, const Matrix *m);

  void debug_out(s_pair *q) const;

public:

  //////////////////////////
  // Computation routines //
  //////////////////////////
  static GB_comp * create(const Matrix *m, 
			  M2_bool collect_syz, 
			  int n_rows_to_keep,
			  int strategy, 
			  M2_bool use_max_degree,
			  int max_degree);

  static GB_comp * create_forced(const Matrix *m, 
				 const Matrix *gb, 
				 const Matrix *mchange);

  virtual int kind() { return 1; }

  virtual const PolynomialRing *get_ring() { return _originalR; }

  virtual ComputationOrNull *set_hilbert_function(const RingElement *h);

  virtual const MatrixOrNull *get_matrix(int level, M2_bool minimize); 

  virtual const MatrixOrNull *get_change(int level);

  virtual const MatrixOrNull *get_leadterms(int nparts, int level);
  
  virtual const FreeModuleOrNull *get_free(int level, M2_bool minimal); 

  virtual const MatrixOrNull *matrix_remainder(int level,
					       const Matrix *m);

  virtual void matrix_lift(int level,
			   const Matrix *m,
			   MatrixOrNull **result_remainder,
			   MatrixOrNull **result_quotient
			   );

  virtual int contains(int level,
		       const Matrix *m);

  virtual int status(int * complete_up_through_this_degree,
		     int * complete_up_through_this_level);
  /* -1: error condition, and the error message is set.
     0: not made, and in fact it won't ever be done...
     1: not started,
     2: started, 
     3: stopped because of a stopping condition
     4: finished the computation completely
  */

  virtual int status_level(int level, 
			   M2_bool minimize,
			   int * complete_up_through_this_degree);
  /* Same return values */

  virtual const M2_arrayint betti(int type);
  /* 0: minimal betti numbers,
     1:
     2:
     3:
  */
  
  virtual void text_out(buffer &o); 
  /* This displays statistical information, and depends on the
     gbTrace value */

};  
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
