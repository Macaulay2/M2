// Copyright 1996  Michael E. Stillman
#ifndef _gb_hh_
#define _gb_hh_

#include "object.hpp"
#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp.hpp"
#include "gb_comp.hpp"

#include "spair.hpp"

class GB_comp : public gb_comp
{
private:
  // Ring information
  const Monoid *M;
  const PolynomialRing *R;
  const FreeModule *F;
  const FreeModule *Fsyz;

  int this_degree;

  // state information
  int state;  // GB_COMP_*
  int ar_i, ar_j;		// State info used for autoreduction
  int np_i;			// State info used for new pairs

  s_pair_heap *spairs;
  s_pair_heap *gens;

  array<gb_elem *> gb;
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
  int collect_syz;	// 0 or 1
  int n_comps_per_syz;
  bool is_ideal;
  int strategy;			// 0 = don't sort gb, 1 = sort gb

  // Hilbert function information
  bool use_hilb;
  char hilb_step;		// Are there any new GB elements in this degree?
  int n_in_degree;		// The number of new elements that we expect to find
				// in this degree.
  int n_saved_hilb;
  hilb_comp *hf_comp;
  RingElement hf_orig;		// The Hilbert function that we are given at the beginning
  RingElement hf_diff;		// The difference between hf_orig and the computed hilb fcn
private:
  void set_up0(const Matrix &m, int csyz, int nsyz);
  void set_up(const Matrix &m, int csyz, int nsyz, int strategy);
  void force(const Matrix &m, const Matrix &gb, const Matrix &mchange, 
	  const Matrix &syz);

  // S-pair control
  s_pair *new_var_pair(gb_elem *p, const int *lcm);
  s_pair *new_ring_pair(gb_elem *p, const int *lcm);
  s_pair *new_s_pair(gb_elem *p, gb_elem *q, const int *lcm);
  s_pair *new_gen(int i, const vec f);
  void remove_pair(s_pair *& p);

  void find_pairs(gb_elem *p);
  void compute_s_pair(s_pair *p);
  void gb_reduce(vec &f, vec &fsyz);
  void gb_geo_reduce(vec &f, vec &fsyz);
  void gb_insert(vec f, vec fsyz, int ismin);

  int gb_sort_partition(int lo, int hi);
  void gb_sort(int lo, int hi);

  // Hilbert function use
  int coeff_of(const RingElement &h, int deg) const;
  void flush_pairs(int deg);
  
  int next_degree();
  int computation_complete(const int *stop_degree,
			   int stop_gb, int stop_syz, 
			   int stop_codim,
			   int stop_pairs, 
			   int stop_min_gens,
			   int subring);

  bool new_pairs_step();
  int s_pair_step();
  int gen_step();
  bool auto_reduce_step();

  void resize(int nbits);

public:
  // Forcing a GB
  GB_comp(const Matrix &m, const Matrix &gb, const Matrix &mchange, 
	  const Matrix &syz);

  // An honest GB computation
  GB_comp(const Matrix &m, int collect_syz, int n_syz, int strategy);
  GB_comp(const Matrix &m, int collect_syz, int n_syz, 
	  RingElement hf, int strategy);

  ~GB_comp();
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
  void debug_out(s_pair *q) const;
  void stats() const;

  // infrastructure
  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  void bin_out(ostream &) const {}
  const char * type_name         () const { return "groebner computation"; }
  void text_out(ostream &o) const { o << "groebner computation"; }

  int length_of() const { return n_gb; }
};  
#endif
