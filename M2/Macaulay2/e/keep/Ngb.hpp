// Copyright 1996  Michael E. Stillman
#ifndef _Ngb_hh_
#define _Ngb_hh_

#include "object.hh"
#include "relem.hh"
#include "matrix.hh"
#include "polyring.hh"
#include "comp.hh"
#include "gb_comp.hh"

struct MonomialIdeal_pair
{
  MonomialIdeal mi;
  MonomialIdeal mi_search;
  
  MonomialIdeal_pair(const Ring *R) : mi(R), mi_search(R) {}

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

struct Ns_pair
{
  Ns_pair *next;
  int syz_type;
  int compare_num;
  int *lcm;			// A packed monomial (should it be an expvector?)
  Ns_pair *first;
  Ns_pair *second;
  vec f;			// A vector in NGB_comp::gens.rows()
  vec fsyz;			// A vector in NGB_comp::syz.rows()
  int is_min;

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

struct Ns_degree
    // Collection of Ns_pairs's all of the same degree
{
  Ns_degree *next;
  int *this_deg;
  Ns_pair *first;
  int num_pairs;
  int num_left;

  Ns_degree() : next(NULL), first(NULL), num_pairs(0), num_left(0) {}
  ~Ns_degree();

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

class NGB_comp : public gb_comp
{
private:
  // state information
  int state;  // GB_COMP_*

  Ns_degree *this_degree;

  int ar_i, ar_j;		// State info used for autoreduction
  int np_i;			// State info used for new pairs
  Ns_pair *this_pair;		// State info used for spair/gen steps

  int lowest_degree;
  array<Ns_degree *> s_pairs;
  array<Ns_pair *> base_components;

  Matrix gens;			// This is the input
  Matrix syz;
  array<Ns_pair *> gb;

  array<MonomialIdeal_pair *> monideals;

  // statistics information, much is kept with the s_set
  int next_compare_num;	// Used to set this field for each pair.
  int n_gb;
  int n_mingens;
  int n_gens_left;

  int n_pairs;
  int n_computed;
  int n_saved_gcd;
  int n_saved_hilb;

  // finish conditions
  const int *stop_degree; // A monomial in deg_monoid()
  int stop_codim;
  int stop_gb;
  int stop_syz;
  int stop_pairs;
  int stop_min_gens;
  int resize_nbits;

  // Syzygy type
  int collect_syz;	// 0 or 1
  int n_comps_per_syz;
  char is_ideal;

  // Hilbert function information
  char use_hilb;
  int n_in_degree;
  ring_elem hf;
  ring_elem hf_diff;

  // Local variables for some inner loop routines
  int *find_pairs_m;
  int *find_pairs_exp;
  int *find_pairs_lcm;
  intarray a_m, a_exp, a_lcm;

  // Ring information
  const monoid *M;
  const PolynomialRing *R;
  int n_degrees;
private:
  // Simple internal routines
  Ns_pair *elem(int i) const { return gb[i]; }

  int *multi_degree(const Ns_pair *q) const;
  const int *current_degree() const; // Points into 'this_degree'

  void set_up(const Matrix &m, int nsyz, int stype);

  Ns_degree *get_degree_set(int *&deg);

  // S-pair control
  Ns_pair *new_var_pair(Ns_pair *p, const int *lcm);
  Ns_pair *new_ring_pair(Ns_pair *p, const int *lcm);
  Ns_pair *new_s_pair(Ns_pair *p, Ns_pair *q, const int *lcm);
  Ns_pair *new_base(int i);
  Ns_pair *new_gen(int i);

  void find_pairs(Ns_pair *p);
  void insert_s_pair(Ns_pair *p);

  int compare_pairs(Ns_pair *f, Ns_pair *g) const;
  Ns_pair *merge_pairs(Ns_pair *f, Ns_pair *g) const;
  void sort_pairs(Ns_pair *&p) const;

  void new_pairs(Ns_pair *p);

  int degree_set_ok(const Ns_degree *p, const int *deg) const;
  Ns_degree *next_degree(const int *deg);
  int new_pairs_step();

  void compute_s_pair(Ns_pair *p);
  void gb_reduce(vec &f, vec &fsyz);
  void gb_insert(Ns_pair *p, int ismin);
  void syz_insert(vec fsyz);
  void sort_new_elems();

  // internal routines used in computation
  int next_level();
  void resize(int nbits);
  int s_pair_step();
  int auto_reduce_step();
  int computation_complete();
  void step();
public:
  // Forcing a GB
  NGB_comp(const Matrix &m, const Matrix &gb, const Matrix &mchange);
  NGB_comp(const Matrix &m, const Matrix &gb, const Matrix &mchange, 
	  const Matrix &syz);

  // An honest GB computation
  NGB_comp(const Matrix &m, int collect_syz, int n_syz);
  NGB_comp(const Matrix &m, int collect_syz, int n_syz, 
	  const ring_elem hf);

  ~NGB_comp();

  // setting options
  int set_verbose_level(int d);
  void set_spair_order(const int *wts);
  void set_div_order(const int *wts);

  // performing the computation
  int calc(const int *deg, const intarray &stop_conditions);
      // stop_conditions = (ngb, nsyz, npairs, cod, do_min, nbits)

  // reduction
  Matrix reduce(const Matrix &m, Matrix &lift);
  Vector reduce(const Vector &v, Vector &lift);

  // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
  Matrix min_gens_matrix();
  Matrix initial_matrix(int n);
  Matrix gb_matrix();
  Matrix change_matrix();
  Matrix syz_matrix();
  void debug_out(Ns_pair *q) const;
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

  const Ring  * Ring_of() const      { return R; }
  const Ring  * Ncoeffs() const       { return R->Ncoeffs(); }
  const monoid * degree_monoid() const { return R->degree_monoid(); }
};
#endif
