// Copyright 1999  Michael E. Stillman
#ifndef _EGB1_hh_
#define _EGB1_hh_

#include "Espairs.hpp"
#include "geovec.hpp"

const int SP_SYZ = 1;
const int SP_RING = 2;
const int SP_SKEW = 3;
const int SP_GEN = 4;

// This is in EGB.hpp
class EStopConditions
{
public:
  bool degree;
  int degree_limit;
  int gb_limit;
  int syz_limit;
  int pair_limit;
  int mingens_limit;
  int subring_limit;
  bool codim;
  int codim_limit;  // Set if 'codim' is true.
        // Stop if the codimension of the initial submodule becomes
        // >= codim_limit.
};

// The following type contains all of the interface elements needed to
// the rest of the system, including polynomial/monoid/coefficient arithmetic
#if 0
class EGBinterface
{
public:
  typedef void *ringelement;  // A coefficient in the base field/ring.
  typedef void *monomial;
  typedef void *term;
  typedef void *polynomial;
  typedef void *vector;
  typedef void *freemodule;
  typedef void *vector_collector;
  typedef void *vector_heap;

  // Ring informational
  bool is_weyl_algebra();
  bool is_quotient_ring();
  bool is_skew_commutative();
  bool ring_is_graded();
  bool coefficients_have_denominators();

  // Operations on ringelement's
  ringelement one;
  ringelement minus_one;
  ringelement copy_coefficient(const ringelement a) const;
  void remove_coefficient(ringelement &a) const;
  ringelement negate_coefficient(const ringelement a) const;
  ringelement syz_coefficient(const ringelement a, const ringelement b, 
			      ringelement &a1,
			      ringelement &b1) const;
  bool is_zero_coefficient(ringelement a) const;
  bool is_equal_coefficient(ringelement a, ringelement b) const;

  // Operations on monomials
  int nvars;
  const int *heuristic;  // Array 0..nvars-1 of positive integers giving a
			 // "standard" degree for monomials.
  const int *standard;   // Array 0..nvars-1 of all 1's.
  int degree_monomial(const int *wts, const monomial m) const;
  monomial one_monomial;
  
  monomial lcm_monomial(const monomial a, const monomial b) const;
  monomial gcd_monomial(const monomial a, const monomial b) const;
  monomial syz_monomial(const monomial a, const monomial b,
			monomial &a1, monomial &b1) const; // returns the lcm

  bool is_one_monomial(const monomial a) const;
  bool is_equal_monomial(const monomial a, const monomial b) const;
  int compare_monomial(const monomial a, const monomial b) const;
    // returns EQ, LT, or GT
  monomial copy_monomial(const monomial a) const;
  void remove_monomial(monomial &a) const;

  monomial monomial_from_exponents(const int *exponents) const;
  monomial monomial_from_pairs(int npairs, const int *pairs) const;
  void copy_exponents_of_monomial(const monomial m, int *exponents) const;

  // Special operations for skew commuting variables
  
  // Operations on exponent vectors (of length 'nvars')...

  // Operations on terms
  term make_term(ringelement c, monomial m, int x) const;
  void remove_term(term &t) const;

  // Operations on vectors
  void remove_vector(const freemodule F, vector &a) const;
  vector copy_vector(const freemodule F, const vector a) const;
  vector e_sub_i(const freemodule F, int i) const;
  bool is_zero_vector(const vector a) const;
  bool is_equal_vector(const vector a, const vector b) const;

  const ringelement lead_coefficient(const vector a) const;
  const monomial lead_monomial(const vector a) const;
  int lead_component(const vector a) const;
  int size(const vector a) const;
  const term lead_term(const vector a) const;

  void negate_to(vector &a) const;
  vector multiply_by_term(const ringelement a, const monomial m, 
			  const vector v) const;
  vector multiply_by_term(const polynomial f, 
			  const ringelement a, const monomial m, int x) const;
  void normal_form(const freemodule F, vector &a);

  ringelement remove_content(vector &a) const;
  void divide_to(vector &a, const ringelement c) const;

  // vector collectors: these collect terms IN ORDER ONLY
  vector_collector start_collection(const freemodule F) const;
  void append_term_to_vector(vector_collector a, term &t) const;
  vector to_vector(vector_collector &a) const;

  // vector heap operations
  vector_heap start_heap(const freemodule F) const;
  void add_to_heap(vector_heap &h, vector &v) const;
  void add_multiple_to(vector_heap &h, const ringelement a, const monomial m,
		       const vector v) const;
  const term get_lead_term(vector_heap &h) const;
  term remove_lead_term(vector_heap &h) const;
  vector heap_to_vector(vector_heap &h) const;
  
  // free module operations
  int degree(const freemodule F, int i) const;
  int rank(const freemodule F) const;
  
  // matrix creation operations
};
#endif
struct egb_elem
{
  int degree;
  EVector f;
  EVector fsyz;
  const exponent_vector *lcm;  // Lead monomial
  // need denominator of fsyz, when computation is over a frac field.
  bool is_min_GB;
  bool is_min;  // Maybe also the largest 'n' for which 
		// this element is minimal for the subring.
  int npairs;	// Number of pairs remaining for this element.
  egb_elem() {}

  // infrastructure
  friend void i_Ecommands();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

struct egb_component {
  egb_elem *gb;  // Are these sorted?
  EMonomialLookupTable *mi;
};

class EMonomialLookupTable
{
  // An abstract class, which has two descendents at least:
  // Linked list, with masks, and the tree technique.

public:

  void minimalize(es_pair * & pairs, es_pair *&rejects);
  virtual bool search(const monomial *m, egb_elem *&result);
  virtual bool search(const monomial *m, array<egb_elem *> &result);
  virtual void insert(egb_elem *p, array<egb_elem *> &nonminimals);

  virtual egb_elem *first();  // Are these sorted??
};

class EMonomialLinearLookupTable : public EMonomialLookupTable
{
  struct node
  {
    node *next;
    egb_elem *elem;
    int mask;
    int degree;  // This is the actual degree of the exponent vector
    const exponent_vector *exponents;
  };

  int nvars;
  node *table;

  int monomial_mask(const exponent_vector *exp) const;
  int exp_divides(const exponent_vector *e, const exponent_vector *f) const;

  int compare(node *p, node *q) const;

public:
  EMonomialLinearLookupTable();

  virtual bool search(const monomial *m, egb_elem *&result);
  virtual bool search(const monomial *m, array<egb_elem *> &result);
  virtual void insert(egb_elem *p, const exponent_vector *exponents, 
		      array<egb_elem *> &nonminimals);
  virtual egb_elem *first();  // Are these sorted??

  bool find_divisor(const exponent_vector *exp, egb_elem * & result) const;
  bool find_all_divisors(const int *exp,array<egb_elem *> &result) const;
};
class ESPairLookupTable
{
public:
  ESPairLookupTable(es_pair *pairs);
  bool find_divisor(const exponent_vector *exp, es_pair *&result) const;
  void append(es_pair *&pairs);
  es_pair *getList();
};
class EGB1 : public EGroebnerComputation
{
private:
  // Ring information
  const EMonoid *M;
  const EPolynomialRing *R;
  const EFreeModule *F;
  const EFreeModule *Fsyz;

  ESPairSet *spairs;
  es_pair *this_set;		// All the elements of this degree
  int this_degree;

  array< egb_elem * > gbLarge;
  array< egb_component > gb;

  // Syzygies collected
  array< EVector > syz;

  // statistics information
  int n_gb;
  int n_subring;
  int n_pairs;
  int n_computed;
  int n_saved_gcd;
  int n_saved_lcm;

  // Syzygy type
  bool collect_syz;
  int n_comps_per_syz;

  // Flags
  bool is_ideal;
  bool find_trimmed;
  bool find_subring_trimmed;

  // Options which are useful to have handy
  bool ring_is_skew;
  bool ring_is_quotient;
  bool coeffs_QQ;
  bool is_homogeneous;

  // Strategy information for the various choices
  int strategy;
  int *heuristicWeightVector;

  // Stash for exponent vectors (this will probably go away with new engine code)
  stash *exponent_stash;
private:
  // These are low level routines
  exponent_vector *new_exponent_vector() const;
  void remove_exponent_vector(exponent_vector *&a) const;
  void increment(egb_elem *p) const;
  void decrement(egb_elem *p) const;
  int lead_component(egb_elem *p) const;
  int lead_component(es_pair *s) const;

  void sort_pairs(es_pair * &p) const;
  void choose_unique_pairs(es_pair * &p) const;
  
private:
  bool set_up(const Matrix &m, int csyz, int nsyz, int strategy);

  bool search(const monomial *m, int comp, egb_elem *&result);
    // Find a GB element which divides m*comp, if one exists.
    // Problem: which one should one choose?

  void minimalize_pairs(es_pair *p) const;
    // Takes a list of s-pairs, and chooses a set which generate.

  void update_pairs(egb_elem *p);
    // Update the set of s-pairs

  void compute_s_pair(es_pair *p, EVectorHeap &f, EVectorHeap &fsyz);
    // Make an s-pair.
  void cancel_lead_term(EVectorHeap &fh, EVectorHeap &fsyz, vec lead, egb_elem *g) const;
  int gb_reduce(EVectorHeap &fh, EVectorHeap &fsyzh, EVector &f, EVector &fsz);
  void gb_reduce(EVector &f, EVector &fsyz);
  // Return values: 0 = f reduces to zero
  // 1 = lead term of f is a new monomial
  // 2 = lead term of f is not new, but get new coefficient (over ZZ only...)

  void make_monic(EVector &f, EVector &fsyz) const;
  void auto_reduce_by(egb_elem *new_elem);
  void insert_and_minimalize(egb_elem *new_elem);

  void gb_insert(EVector &f, EVector &fsyz, bool maybe_minimal, bool maybe_subring_minimal);
  // Insert the element 'f' as a new element in the GB.

  int is_computation_complete(const EStopConditions &stop) const;

  void s_pair_step(es_pair *p);

  int gen_step(es_pair *p);

  // The following routines use polynomial and monoid arithmetic
  egb_elem *make_gb_elem(int degree,
			 EVector &f, 
			 EVector &fsyz,
			 bool maybe_minimal, 
			 bool maybe_subring_minimal) const;

  es_pair *make_ring_s_pair(egb_elem *p, int j) const;
  es_pair *make_skew_s_pair(egb_elem *p, int v) const;
  es_pair *make_s_pair(egb_elem *a, egb_elem *b) const;
  es_pair *make_gen_pair(int i, const EVector &f);

  void remove_pair(es_pair *& p) const;

  // Pair updating
  bool pair_not_needed(es_pair *p, egb_elem *m) const;

  // Syzygy control
  void collect_syzygy(EVector &fsyz);

  int compare(const egb_elem *p, const egb_elem *q) const;
  void inter_reduce(egb_elem *&gens);

public:
  // An honest GB computation
  EGB1(const EMatrix *m, int collect_syz, int n_syz, int strategy);
  ~EGB1();

  virtual int new_calc(const EStopConditions &stop);
  //////////////////////////////////////
  // Old (gb_comp) interface routines //
  //////////////////////////////////////
  virtual int calc(const int *deg, const intarray &stop_conditions);
  
  virtual void stats() const;
  virtual Matrix min_gens_matrix();
  virtual Matrix gb_matrix();
  virtual Matrix syz_matrix();
  virtual Matrix change_matrix();
  virtual Matrix initial_matrix(int n=-1);

  virtual Matrix reduce(const Matrix &m, Matrix &result_lift);
  virtual Vector reduce(const Vector &v, Vector &result_lift);

  virtual int contains(const Matrix &m);
  virtual bool is_equal(const gb_comp *q);

  virtual bool moreGenerators(int lo, int hi, const Matrix &m);
#if 0
  // NEW VERSIONS OF THESE ROUTINES
  // Adding generators
  
  // The return value of 'calc' is a comp_return_value.
  virtual int calc(const EStopConditions &stop);

  // Free modules used with reduction
  virtual const EFreeModule *getTarget() const;
  virtual const EFreeModule *getSource() const;
  virtual EVector reduceVector(const EVector &v) const;
  virtual EVector reduceVector(const EVector &v, EVector &result_lift) const;
  
  virtual EMatrix *getGenerators() const;
  virtual EMatrix *getGB() const;
  virtual EMatrix *getChangeOfBasis() const;
  virtual EMatrix *getSyzygies() const;
  virtual EMatrix *getLeadTerms(int n=-1) const;
  virtual EMatrix *getSubring(int n=-1) const;
  virtual EMatrix *getSubringGB(int n=-1) const;
#endif

  // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
  void debug_out(es_pair *q) const;
  void debug_pairs_out(egb_elem *p) const;
  void debug_pairs() const;
  void debug_out(buffer &o, es_pair *q) const;
  void debug_pairs_out(buffer &o, egb_elem *p) const;
  void debug_pairs(buffer &o) const;

  // infrastructure
  friend void i_Ecommands();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  int length_of() const { return n_gb; }
};  
#endif
