// Copyright 1998  Michael E. Stillman
#ifndef _gbZZ_hh_
#define _gbZZ_hh_

#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp.hpp"
#include "gb_comp.hpp"
#include "gbring.hpp"

#include "newspair.hpp"
#include "termideal.hpp"

const bool GB_NOT_MINIMAL = false;
const bool GB_MAYBE_MINIMAL = true;

struct GB_elem
{
  gbvector *f;
  gbvector *fsyz;
  int *lead_exp;
  int sugar_degree;
  
  GB_elem()
    : f(NULL), fsyz(NULL), lead_exp(NULL), 
      sugar_degree(0) {}
  GB_elem(gbvector *f0, gbvector *fsyz0, int sugar_degree0) 
    : f(f0), fsyz(fsyz0), lead_exp(NULL), 
      sugar_degree(sugar_degree0) {}
};


class GBZZ_comp : public gb_comp
{
private:
  // Ring information
  const Ring *K;
  const Monoid *M;
  const PolynomialRing *A;	// The quotient ring (if a quotient, that is)
  const PolynomialRing *R;	// The polynomial ring (possibly A = R).
  const FreeModule *F;		// generators live here
  const FreeModule *Fsyz;	// original generators correspond to basis elements

  // These two will become one thing...
  FreeModule *Gsyz;	// GB elements correspond to basis elements
				// These are NOT necessarily minimal GB elements.
				// spairs are initially constructed using Gsyz.
  // Base ring information.  This should be in the PolynomialRing class...
  const FreeModule *Rsyz;	// NOT a Schreyer order.
				// This is NOT a Schreyer order...

  GBRing *GR;

  int this_degree;
  int prev_degree;		// Used only in determining if we are continuing
				// with a degree, or starting a new one.
  // state information
  int state;  // GB_COMP_*
  int ar_i, ar_j;		// State info used for autoreduction
  int ar_first_in_deg;		// First element in the current degree.
  int np_i;			// State info used for new pairs

  s_pair_set *spairs;

  array<GB_elem *> gb;
  array<TermIdeal *> termideals;

  intarray gblocs;		// permutation of 0..gb.length()-1
				// Used for auto reduction, creating gb matrix.
  intarray gbpairlocs;		// permutation of 0..gb.length()-1
				// Used for s-pair construction.

  // Minimal generators being collected
  Matrix *mingens;

  // Syzygies collected
  Matrix *syz;

  // statistics information, much is kept with the s_set
  int n_gb;
  int n_mingens;
  int n_gens_left;
  int n_subring;

  int n_pairs;
  int n_computed;
  int n_saved_gcd;

  // Syzygy type
  bool collect_syz;
  int n_comps_per_syz;
  bool is_ideal;
  int strategy;

  ring_elem one;		// K->one.
private:
  void set_up0(const Matrix *m, int csyz, int nsyz);
  void set_up(const Matrix *m, int csyz, int nsyz, int strategy);
  void force(const Matrix *m, const Matrix *gb, const Matrix *mchange, 
	  const Matrix *syz);

  // S-pair control
  bool new_generator(int i, const vec m, 
		     gbvector *&f, gbvector *&fsyz);

  void apply_gb_elements(gbvector *&f, gbvector *&fsyz, gbvector *gsyz) const;
  void compute_s_pair(gbvector *gsyz, gbvector *rsyz,
		      gbvector *&f, gbvector *&fsyz) const;
  bool gb_reduce(gbvector *&f, gbvector *&fsyz) const; 
  // gb_reduce: returns 'false' iff the final lead term of 'f' is not in the
  // initial monomial ideal.  If 'f' reduces to zero, 'true' is returned.

  // void gb_geo_reduce(gbvector *&f, gbvector *&fsyz) const;

  void find_pairs(int me);

  void insert_gb_element(gbvector *f, gbvector *fsyz);
  void replace_or_insert_gb_element(gbvector *f, gbvector *fsyz);
  bool insert_syzygy(gbvector *fsyz);
  void handle_element(gbvector *f, gbvector *fsyz);

  int gb_sort_partition(int lo, int hi);
  void gb_sort(int lo, int hi);
  int autoreduce_sort_partition(int lo, int hi);
  void autoreduce_sort(int lo, int hi);

  int next_degree();
  int computation_complete(const int *stop_degree,
			   int stop_gb, int stop_syz, 
			   int stop_codim,
			   int stop_pairs, 
			   int stop_min_gens,
			   int subring);

  bool new_pairs_step();
  bool s_pair_step();
  bool gen_step();
  bool auto_reduce_step();

  void resize(int nbits);

public:
  // Forcing a GB
  GBZZ_comp(const Matrix *m, const Matrix *gb, const Matrix *mchange, 
	  const Matrix *syz);

  // An honest GB computation
  GBZZ_comp(const Matrix *m, int collect_syz, int n_syz, int strategy);

  ~GBZZ_comp();
  // Performing the computation
  int calc(const int *deg, const intarray &stop_conditions);

  // Adding generators
  void add_gens(int lo, int hi, const Matrix &m);

  // reduction
  Matrix *reduce(const Matrix *m, Matrix *&lift);
  Vector *reduce(const Vector *v, Vector *&lift);

  virtual int contains(const Matrix *m);
  virtual bool is_equal(const gb_comp *q);
  
  // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
  Matrix *min_gens_matrix();
  Matrix *initial_matrix(int n);
  Matrix *gb_matrix();
  Matrix *change_matrix();
  Matrix *syz_matrix();
  void stats() const;
};  
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
