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

class GBZZ_comp : public gb_comp
{
private:
  // Ring information
  const Monoid *M;
  const PolynomialRing *R;
  const FreeModule *F;		// generators live here
  const FreeModule *Fsyz;	// original generators correspond to basis elements
  const FreeModule *Gsyz;	// GB elements correspond to basis elements
				// These are NOT necessarily minimal GB elements.
				// spairs are initially constructed using Gsyz.
				// 
  int this_degree;

  // state information
  int state;  // GB_COMP_*
  int ar_i, ar_j;		// State info used for autoreduction
  int np_i;			// State info used for new pairs

  s_pair_set *spairs;

  array<GB_elem *> gb;
  array<monideal_pair *> monideals; // baggage for each is 'gb_elem *'

  // Syzygies collected
  Matrix syz;
  Matrix gbmatrix;

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
  int strategy;			// 0 = don't sort gb, 1 = sort gb

  // Local data
  int *REDUCE_EXP;		// exponent vector, used in reduction routines as local var.
  int *REDUCE_DIV;		// element of M, used in reduction routines as local var.
private:
  void set_up0(const Matrix &m, int csyz, int nsyz);
  void set_up(const Matrix &m, int csyz, int nsyz, int strategy);
  void force(const Matrix &m, const Matrix &gb, const Matrix &mchange, 
	  const Matrix &syz);

  // S-pair control
  S_pair *new_ring_pair(GB_elem *p, const int *lcm);
  S_pair *new_s_pair(GB_elem *p, GB_elem *q, const int *lcm);
  gen_pair *new_gen(int i, const vec f);
  void remove_pair(S_pair *& p);
  void remove_gen(gen_pair *& p);

  void find_pairs(GB_elem *p);
  void compute_s_pair(S_pair *p, vec &f, vec &fsyz);
  void gb_reduce(vec &f, vec &fsyz);
  void gb_geo_reduce(vec &f, vec &fsyz);
  void gb_insert(vec f, vec fsyz, int ismin);

  int gb_sort_partition(int lo, int hi);
  void gb_sort(int lo, int hi);

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
  void debug_out(S_pair *q) const;
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
