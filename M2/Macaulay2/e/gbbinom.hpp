// Copyright 1997  Michael E. Stillman

#ifndef _gbbinom_hh_
#define _gbbinom_hh_

#include "comp.hpp"
#include "gb_comp.hpp"
#include "matrix.hpp"

/////////////////////
// Data structures //
/////////////////////

class binomialGB_comp;
typedef int *monomial;
  
struct binomial {
  monomial lead;
  monomial tail;

  binomial() : lead(NULL), tail(NULL) {}
  binomial(monomial lead, monomial tail) : lead(lead), tail(tail) {}
};

struct binomial_gb_elem {
  binomial_gb_elem *next;
  binomial_gb_elem *smaller;		// NULL if this is a minimal GB element, otherwise
				// points to a GB element whose lead term divides 
				// this lead term.
  binomial f;

  binomial_gb_elem(binomial ff) : next(NULL), smaller(NULL), f(ff) {}
};

struct binomial_s_pair {
  binomial_gb_elem *f1;
  binomial_gb_elem *f2;		// f2 is NULL for generators...
  monomial lcm;
  binomial_s_pair() {}
  binomial_s_pair(binomial_gb_elem *f1, binomial_gb_elem *f2, monomial lcm)
    : f1(f1), f2(f2), lcm(lcm) {}
};

/////////////////////////////
// Monomials and binomials //
/////////////////////////////

class binomial_ring
// First implementation: exponent vectors
{
  friend class binomialGB_comp;
  const Ring *R;
  const FreeModule *F;		// rank 1 free R-module

  // exponent vector: [e0, ..., e(nvars-1), -deg, -wt] (the last slot is only there, if there
  // is a weight function.
  int nvars;
  bool have_weights;
  int nslots;			// nvars+1 or nvars+2
  int *degrees;			// (Negative of) Degree vector.
  int *weights;			// (Negative of) Weight function, if any
  bool revlex;			// true means break ties by degrevlex, false by deglex

  stash *monstash;

  monomial new_monomial() const;
  void set_weights(monomial m) const;
  
public:
  binomial_ring(const Ring *RR);
  binomial_ring(const Ring *RR, 
		int *wts,
		bool revlex);
  ~binomial_ring();

  // monomial operations
  void remove_monomial(monomial &m) const;
  monomial make_monomial(int *exp) const; // Make a monomial from an exponent vector

  int weight(monomial m) const;
  int degree(monomial m) const;
  unsigned int mask(monomial m) const;
  bool divides(monomial m, monomial n) const;

  monomial quotient(monomial m, monomial n) const;
  monomial divide(monomial m, monomial n) const;
  monomial mult(monomial m, monomial n) const;
  monomial lcm(monomial m, monomial n) const;
  monomial spair(monomial lcm, monomial m, monomial n) const; // returns lcm - m + n.
  void spair_to(monomial a, monomial b, monomial c) const;
  // spair_to: a = a - b + c

  int compare(monomial m, monomial n) const; // returns LT, EQ, or GT.
  int graded_compare(monomial m, monomial n) const; // returns LT, EQ, or GT.

  void translate_monomial(const binomial_ring *old_ring, monomial &m) const;
  vec monomial_to_vector(monomial m) const;

  // binomial operations
  void remove_binomial(binomial &f) const;
  binomial make_binomial() const; // allocates the monomials

  const monomial lead_monomial(binomial f) const { return f.lead; }
  void translate_binomial(const binomial_ring *old_ring, binomial &f) const;

  bool gcd_is_one(monomial m, monomial n) const;
  bool gcd_is_one(binomial f) const;
  bool remove_content(binomial &f) const;

  vec binomial_to_vector(binomial f) const;
  vec binomial_to_vector(binomial f, int n) const;
  bool vector_to_binomial(vec f, binomial &result) const;
  void intvector_to_binomial(vec f, binomial &result) const;

  bool normalize(binomial &f) const;

  bool one_reduction_step(binomial &f, binomial g) const;
  bool calc_s_pair(binomial_s_pair &s, binomial &result) const;

  void monomial_out(buffer &o, const monomial m) const;
  void elem_text_out(buffer &o, const binomial &f) const;
};

class binomial_s_pair_set
{
  struct s_pair_lcm_list;
  struct s_pair_elem;

  struct s_pair_degree_list {
    s_pair_degree_list *next;
    int deg;
    s_pair_lcm_list *pairs;
    int n_elems;
  };

  struct s_pair_lcm_list {
    s_pair_lcm_list *next;
    monomial lcm;
    s_pair_elem *pairs;		// List of pairs with this lcm.
  };

  struct s_pair_elem {
    s_pair_elem *next;
    binomial_gb_elem *f1;
    binomial_gb_elem *f2;
    s_pair_elem(binomial_gb_elem *f1, binomial_gb_elem *f2) : next(NULL), f1(f1), f2(f2) {}
  };

  const binomial_ring *R;
  monomial _prev_lcm;		// This class is responsible for deleting lcm's.
  s_pair_degree_list *_pairs;
  int _n_elems;
  
  // Stats for number of pairs:
  int _max_degree;
  intarray _npairs; // npairs[2*d] = total # of pairs.  npairs[2*d+1] = number left

  void remove_lcm_list(s_pair_lcm_list *p);
  void remove_pair_list(s_pair_degree_list *p);
  void insert_pair(s_pair_degree_list *q, binomial_s_pair &s);
public:
  binomial_s_pair_set(const binomial_ring *R);
  ~binomial_s_pair_set();

  void enlarge(const binomial_ring *R);

  void insert(binomial_gb_elem *p);  // Insert a generator
  void insert(binomial_s_pair p);   // Insert an s-pair.

  bool next(const int *d, binomial_s_pair &result);
  // returns false if no more pairs in degrees <= *d. (NULL represents
  // infinity).  

  int lowest_degree() const;
  int n_elems(int d) const;
  int n_elems() const;
  void stats() const;
};

class binomialGB
{
  struct gbmin_elem {
    gbmin_elem *next;
    binomial_gb_elem *elem;
    int mask;
    gbmin_elem() {}  // For list header
    gbmin_elem(binomial_gb_elem *f, int mask) : elem(f), mask(mask) {}
  };

  struct monomial_list {
    monomial_list *next;
    monomial m;
    int mask;
    gbmin_elem *tag;  // This is a list
    monomial_list(monomial m, int mask, gbmin_elem *val) : m(m), mask(mask), tag(val) {}
  };

  const binomial_ring *R;
  gbmin_elem *first;
  int _max_degree;  // The highest degree of a GB generator found so far.

  bool use_bigcell;
  bool is_homogeneous_prime;
public:
  binomialGB(const binomial_ring *R, bool bigcell, bool homogprime);
  ~binomialGB();
  void enlarge(const binomial_ring *newR);

  void minimalize_and_insert(binomial_gb_elem *f);
  monomial_list *find_divisor(monomial_list *I, monomial m) const;
  monomial_list *ideal_quotient(monomial m) const;
  void remove_monomial_list(monomial_list *mm) const;
  
  binomial_gb_elem *find_divisor(monomial m) const;
  void make_new_pairs(binomial_s_pair_set *Pairs, binomial_gb_elem *f) const;
  void reduce_monomial(monomial m) const;
  bool reduce(binomial &f) const; // returns false if reduction is to zero.

  class iterator {
    friend class binomialGB;
    gbmin_elem *elem;
    iterator(gbmin_elem *p) : elem(p) {}
    gbmin_elem *this_elem() { return elem; }
  public:
    iterator &operator++() { elem = elem->next; return *this; }
    iterator &operator++(int) { elem = elem->next; return *this; }
    binomial_gb_elem *operator*() { return elem->elem; }
    bool operator==(const iterator &p) { return p.elem == elem; }
    bool operator!=(const iterator &p) { return p.elem != elem; }
  };

  iterator begin() const { return iterator(first); }
  iterator end() const { return iterator(NULL); }

  int n_masks() const;
  void debug_display() const;
};

#define GB_FLAG_IS_HOMOGENEOUS 1
#define GB_FLAG_IS_NONDEGENERATE 2
#define GB_FLAG_BIGCELL 4

class binomialGB_comp : public gb_comp
{
  binomial_ring *R;
  binomial_s_pair_set *Pairs;		// Pairs and Generators
  binomialGB *Gmin;
  array <binomial_gb_elem *> Gens;       // All of the generators
  array <binomial_gb_elem *> G;		// All of the binomial_gb_elem's in one place.

  ///////////////////////
  // Flags and options //
  ///////////////////////

  // flags used during the computation:
  bool is_homogeneous;		// Generators will all be homogeneous,
				// and elements of degree d will be added
				// only BEFORE computing a GB in degree d+1.
				// An error is given if a generator is added in
				// after that point.
  bool is_nondegenerate;	// Assumed: the ideal has no linear factors:
				// thus, if x.f is found to be in the ideal,
				// f must already be in the ideal.
  bool use_bigcell;		// The generators given do not necessarily generate
				// the entire ideal.  If an element of the form
				// x.f (x a variable) is found, then we may add f
				// to the ideal.
  bool flag_auto_reduce;	// Form full auto-reduction of GB elements.  This is the
				// default.
  bool flag_use_monideal;	// If true, divisibility is done using monomial ideals
				// otherwise, divisibility is done using a linked list, and 
				// monomial masks.  Currently, using monideals is not yet
				// implemented, so this is 'false'.

  array<binomial_gb_elem *> mingens;	
           // Only valid for homogeneous case.  These point to GB elements
           // so don't free them by accident!

  array<binomial_gb_elem *> mingens_subring;  // Same comment applies here!

  void process_pair(binomial_s_pair p);
  int gb_done(const intarray &stop_condtions) const;
public:
  // creation
  binomialGB_comp(const Ring *R, int *wts, bool revlex, 
		  unsigned int options);
  ~binomialGB_comp();

  void enlarge(const Ring *R, int *wts);
  void add_generators(const Matrix &m);
  int calc(const int *deg, const intarray &stop_conditions);

  Matrix subring();
  Matrix subringGB();

  // reduction: these elements do not need to be binomials?
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

public:
  // infrastructure
  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  binomialGB_comp * cast_to_binomialGB_comp() { return this; }
  const binomialGB_comp * cast_to_binomialGB_comp() const { return this; }

  void bin_out(buffer &) const {}
  const char * type_name         () const { return "binomialGB_comp"; }
  void text_out(buffer &o) const { o << "binomialGB_comp"; }

  int length_of() const { return 0; }
  
};
#endif

