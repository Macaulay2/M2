// Copyright 1997  Michael E. Stillman

#ifndef _gbbinom_hh_
#define _gbbinom_hh_

#include "comp.hpp"
#include "gb_comp.hpp"
#include "matrix.hpp"

/////////////////////
// Data structures //
/////////////////////

typedef int *monomial;
  
struct binomial {
  monomial a;
  monomial b;
};

struct gb_elem {
  gb_elem *next;
  gb_elem *smaller;		// NULL if this is a minimal GB element, otherwise
				// points to a GB element whose lead term divides 
				// this lead term.
  binomial f;
};

struct s_pair {
  s_pair *next;
  gb_elem *f1;
  gb_elem *f2;		// f2 is NULL for generators...
};

/////////////////////////////
// Monomials and binomials //
/////////////////////////////

class binomial_ring
// First implementation: exponent vectors
{
  const Ring *R;
  const FreeModule *F;		// rank 1 free R-module

  // exponent vector: [e0, ..., e(nvars-1), -deg, -wt] (the last slot is only there, if there
  // is a weight function.
  int nvars;
  bool have_wtfcn;
  int nslots;			// nvars+1 or nvars+2
  int *degrees;			// (Negative of) Degree vector.
  int *weights;			// (Negative of) Weight function, if any
  bool revlex;			// true means break ties by degrevlex, false by deglex

  stash *monstash;

  monomial new_monomial() const;
  void set_weights(monomial m) const;
  
public:
  binomial_ring(Ring *RR, int nvars, int nwts, int *wts);
  ~binomial_ring();

  // monomial operations
  void remove_monomial(monomial &m) const;
  monomial make_monomial(int *exp) const; // Make a monomial from an exponent vector

  monomial quotient(monomial m, monomial n) const;
  monomial divide(monomial m, monomial n) const;
  monomial mult(monomial m, monomial n) const;
  monomial lcm(monomial m, monomial n) const;
  monomial spair(monomial lcm, monomial m, monomial n) const; // returns lcm - m + n.

  bool gcd_is_one(monomial m, monomial n) const;
  bool remove_content(monomial &m, monomial &n) const;

  int compare(monomial m, monomial n) const; // returns LT, EQ, or GT.

  void translate_monomial(const binomial_ring *old_ring, monomial &m) const;
  vec monomial_to_vector(monomial m) const;

  bool in_subring(monomial m) const;
  int mask(monomial m) const;

  // binomial operations
  void translate_binomial(const binomial_ring *old_ring, binomial &f) const;

  vec binomial_to_vector(binomial f) const;
  binomial vector_to_binomial(vec f) const;
  binomial intvector_to_binomial(vec f) const;

  void one_reduction_step(binomial &f, binomial g) const;
  binomial calc_s_pair(binomial f, binomial g) const;
};

class s_pair_set
{
  struct s_pair_list {
    s_pair_list *next;
    monomial lcm;
    s_pair *pairs;		// List of pairs with this lcm.
    int n_elems;
  };
  
  array<s_pair_list *> spairs;
  
public:
  s_pair_set();
  ~s_pair_set();

  void insert(s_pair *p, monomial *lcm);

  s_pair *next();  // returns NULL if none left
  s_pair *next(int d); // returns next pair in degrees <= d, if any.

  int lowest_degree() const;
  int n_elems(int d) const;
  int n_elems() const;
};

class binomialGB
{
  binomial_ring *R;
  
  struct gbmin_elem {
    gbmin_elem *next;
    gb_elem *elem;
    int mask;
  };

public:
  void minimalize(gb_elem *f);  // remove elements which have lead term divisible by in(f).
  void insert(gb_elem *f);	// optionally auto-reduces the other elements as well.
  
  gb_elem *find_divisor(monomial *m);
  void make_new_pairs(s_pair_set &Pairs, gb_elem *f);
  void reduce(binomial *&f) const;

  class iterator {
    gbmin_elem *elem;
    iterator(gbmin_elem *p) : elem(p) {}
    friend class binomialGB;
  public:
    iterator &operator++() { elem = elem->next; return *this; }
    gb_elem *operator*() { return elem->elem; }
    bool operator==(const iterator &p) { return p.elem == elem; }
  };

  iterator begin() { return iterator(first); }
  iterator end() { return iterator(NULL); }
};

class binomialGB_comp : public gb_comp
{
  binomial_ring *R;

  s_pair_list Pairs;		// Pairs and Generators
  binomialGB Gmin;
  array <gb_elem *> G;		// All of the gb_elem's in one place.

  ///////////////////////
  // Flags and options //
  ///////////////////////

  // flags used during the computation:
  bool is_homogeneous;
  bool is_homogeneous_prime;
  bool flag_auto_reduce;
  bool flag_divide_by_variables;
  bool flag_use_monideal;	// If true, divisibility is done using monomial ideals
				// otherwise, divisibility is done using a linked list, and 
				// monomial masks.

  array<binomial *> mingens;	// Only valid for homogeneous case.  These point to G elements
				// so don't free them by accident!

  array<binomial *> mingens_subring;  // Same comment applies here!

public:
  // creation
  binomialGB_comp(const Ring *R);
  ~binomialGB_comp();

  void enlarge(const Ring *R);
  void add_generators(const Matrix &m);
  int calc(const int *deg, const intarray &stop_conditions);

  Matrix subring(int n);
  Matrix subringGB(int n);

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

  void bin_out(ostream &) const {}
  const char * type_name         () const { return "binomialGB_comp"; }
  void text_out(ostream &o) const { o << "binomialGB_comp"; }

  int length_of() const { return 0; }
  
};

#endif
