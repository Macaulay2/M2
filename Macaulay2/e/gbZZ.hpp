// Copyright 1998  Michael E. Stillman
#ifndef _gbZZ_hh_
#define _gbZZ_hh_

#include "object.hpp"
#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp.hpp"
#include "gb_comp.hpp"

#include "newspair.hpp"
#include "termideal.hpp"

const bool GB_NOT_MINIMAL = false;
const bool GB_MAYBE_MINIMAL = true;

struct GB_elem
{
  vec f;
  vec fsyz;
  int *lead_exp;
  int sugar_degree;
  bool is_min;			// eventually: TY_MINIMAL, TY_SMALL_GB, TY_LARGE_GB, TY_REMOVED
  
  GB_elem()
    : f(NULL), fsyz(NULL), lead_exp(NULL), 
      sugar_degree(0), is_min(0) {}
  GB_elem(vec f, vec fsyz, int sugar_degree, int is_min) 
    : f(f), fsyz(fsyz), lead_exp(NULL), 
      sugar_degree(sugar_degree), is_min(is_min) {}

  // infrastructure
  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
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
  FreeModule *Gsyz;	// GB elements correspond to basis elements
				// These are NOT necessarily minimal GB elements.
				// spairs are initially constructed using Gsyz.
				// This is NOT a Schreyer order...

  // Base ring information.  This should be in the PolynomialRing class...
  const FreeModule *Rsyz;	// NOT a Schreyer order.

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

  // Syzygies collected
  Matrix syz;

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
  void set_up0(const Matrix &m, int csyz, int nsyz);
  void set_up(const Matrix &m, int csyz, int nsyz, int strategy);
  void force(const Matrix &m, const Matrix &gb, const Matrix &mchange, 
	  const Matrix &syz);

  // S-pair control
  bool new_generator(int i, const vec m, 
		     vec &f, vec &fsyz);

  void apply_gb_elements(vec &f, vec &fsyz, vec gsyz) const;
  void compute_s_pair(vec gsyz, vec rsyz,
		      vec &f, vec &fsyz) const;
  bool gb_reduce(vec &f, vec &fsyz) const; 
  // gb_reduce: returns 'false' iff the final lead term of 'f' is not in the
  // initial monomial ideal.  If 'f' reduces to zero, 'true' is returned.

  // void gb_geo_reduce(vec &f, vec &fsyz) const;

  void find_pairs(int me);

  void insert_gb_element(vec f, vec fsyz, bool is_minimal);
  bool insert_syzygy(vec fsyz);
  void handle_element(vec f, vec fsyz, bool maybe_minimal);

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
  GBZZ_comp(const Matrix &m, const Matrix &gb, const Matrix &mchange, 
	  const Matrix &syz);

  // An honest GB computation
  GBZZ_comp(const Matrix &m, int collect_syz, int n_syz, int strategy);

  ~GBZZ_comp();
  // Performing the computation
  int calc(const int *deg, const intarray &stop_conditions);

  // Adding generators
  void add_gens(int lo, int hi, const Matrix &m);

  // reduction
  Matrix reduce(const Matrix &m, Matrix &lift);
  Vector reduce(const Vector &v, Vector &lift);

  virtual int contains(const Matrix &m);
  virtual bool is_equal(const gb_comp *q);
  
  // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
  Matrix min_gens_matrix();
  Matrix initial_matrix(int n);
  Matrix gb_matrix();
  Matrix change_matrix();
  Matrix syz_matrix();
  void stats() const;

  // infrastructure
  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  void bin_out(buffer &) const {}
  void text_out(buffer &o) const { o << "groebner computation"; }

  class_identifier class_id() const { return CLASS_GB_comp; }

  int length_of() const { return n_gb; }
};  
#endif
