// Copyright 1997  Michael E. Stillman

#ifndef _gbbinom_hh_
#define _gbbinom_hh_

#include "comp-gb.hpp"
#include "matrix.hpp"

/////////////////////
// Data structures //
/////////////////////

class binomialGB_comp;
typedef int *monomial0;

struct binomial : public our_new_delete
{
  monomial0 lead;
  monomial0 tail;

  binomial() : lead(NULL), tail(NULL) {}
  binomial(monomial0 lead0, monomial0 tail0) : lead(lead0), tail(tail0) {}
};

struct binomial_gb_elem : public our_new_delete
{
  binomial_gb_elem *next;
  binomial_gb_elem *smaller;  // NULL if this is a minimal GB element, otherwise
                              // points to a GB element whose lead term divides
                              // this lead term.
  binomial f;

  binomial_gb_elem(binomial ff) : next(NULL), smaller(NULL), f(ff) {}
};

struct binomial_s_pair : public our_new_delete
{
  binomial_gb_elem *f1;
  binomial_gb_elem *f2;  // f2 is NULL for generators...
  monomial0 lcm;
  binomial_s_pair() {}
  binomial_s_pair(binomial_gb_elem *ff1, binomial_gb_elem *ff2, monomial0 lcm0)
      : f1(ff1), f2(ff2), lcm(lcm0)
  {
  }
};

/////////////////////////////
// Monomials and binomials //
/////////////////////////////

class binomial_ring : public our_new_delete
// First implementation: exponent vectors
{
  friend class binomialGB_comp;
  const PolynomialRing *R;
  const FreeModule *F;  // rank 1 free R-module

  // exponent vector: [e0, ..., e(nvars-1), -deg, -wt] (the last slot is only
  // there, if there
  // is a weight function.
  int nvars;
  bool have_weights;
  int nslots;    // nvars+1 or nvars+2
  int *degrees;  // (Negative of) Degree vector.
  int *weights;  // (Negative of) Weight function, if any
  bool revlex;   // true means break ties by degrevlex, false by deglex

  stash *monstash;

  monomial0 new_monomial() const;
  void set_weights(monomial0 m) const;

 public:
  binomial_ring(const PolynomialRing *RR);
  binomial_ring(const PolynomialRing *RR, int *wts, bool revlex0);
  ~binomial_ring();

  // monomial operations
  void remove_monomial(monomial0 &m) const;
  monomial0 make_monomial(
      int *exp) const;  // Make a monomial from an exponent vector
  monomial0 copy_monomial(monomial0 m) const;

  int weight(monomial0 m) const;
  int degree(monomial0 m) const;
  unsigned int mask(monomial0 m) const;
  bool divides(monomial0 m, monomial0 n) const;

  monomial0 quotient(monomial0 m, monomial0 n) const;
  monomial0 divide(monomial0 m, monomial0 n) const;
  monomial0 mult(monomial0 m, monomial0 n) const;
  monomial0 monomial_lcm(monomial0 m, monomial0 n) const;
  monomial0 spair(monomial0 lcm,
                  monomial0 m,
                  monomial0 n) const;  // returns lcm - m + n.
  void spair_to(monomial0 a, monomial0 b, monomial0 c) const;
  // spair_to: a = a - b + c

  int compare(monomial0 m, monomial0 n) const;         // returns LT, EQ, or GT.
  int graded_compare(monomial0 m, monomial0 n) const;  // returns LT, EQ, or GT.

  void translate_monomial(const binomial_ring *old_ring, monomial0 &m) const;
  vec monomial_to_vector(monomial0 m) const;

  // binomial operations
  void remove_binomial(binomial &f) const;
  binomial make_binomial() const;  // allocates the monomials
  binomial copy_binomial(const binomial &f) const;

  monomial0 lead_monomial(binomial f) const { return f.lead; }
  void translate_binomial(const binomial_ring *old_ring, binomial &f) const;

  bool gcd_is_one(monomial0 m, monomial0 n) const;
  bool gcd_is_one(binomial f) const;
  bool remove_content(binomial &f) const;

  vec binomial_to_vector(binomial f) const;
  vec binomial_to_vector(binomial f, int n) const;
  bool vector_to_binomial(vec f, binomial &result) const;
  void intvector_to_binomial(vec f, binomial &result) const;

  bool normalize(binomial &f) const;

  bool one_reduction_step(binomial &f, binomial g) const;
  bool calc_s_pair(binomial_s_pair &s, binomial &result) const;

  void monomial_out(buffer &o, const monomial0 m) const;
  void elem_text_out(buffer &o, const binomial &f) const;
};

class binomial_s_pair_set : public our_new_delete
{
  struct s_pair_lcm_list;
  struct s_pair_elem;

  struct s_pair_degree_list : public our_new_delete
  {
    s_pair_degree_list *next;
    int deg;
    s_pair_lcm_list *pairs;
    int n_elems;
  };

  struct s_pair_lcm_list : public our_new_delete
  {
    s_pair_lcm_list *next;
    monomial0 lcm;
    s_pair_elem *pairs;  // List of pairs with this lcm.
  };

  struct s_pair_elem : public our_new_delete
  {
    s_pair_elem *next;
    binomial_gb_elem *f1;
    binomial_gb_elem *f2;
    s_pair_elem(binomial_gb_elem *ff1, binomial_gb_elem *ff2)
        : next(NULL), f1(ff1), f2(ff2)
    {
    }
  };

  const binomial_ring *R;
  monomial0 _prev_lcm;  // This class is responsible for deleting lcm's.
  s_pair_degree_list *_pairs;
  int _n_elems;

  // Stats for number of pairs:
  int _max_degree;
  intarray
      _npairs;  // npairs[2*d] = total # of pairs.  npairs[2*d+1] = number left

  void remove_lcm_list(s_pair_lcm_list *p);
  void remove_pair_list(s_pair_degree_list *p);
  void insert_pair(s_pair_degree_list *q, binomial_s_pair &s);

 public:
  binomial_s_pair_set(const binomial_ring *R);
  ~binomial_s_pair_set();

  void enlarge(const binomial_ring *R);

  void insert(binomial_gb_elem *p);  // Insert a generator
  void insert(binomial_s_pair p);    // Insert an s-pair.

  bool next(const int *d, binomial_s_pair &result, int &result_deg);
  // returns false if no more pairs in degrees <= *d. (NULL represents
  // infinity).

  int lowest_degree() const;
  int n_elems(int d) const;
  int n_elems() const;
  void stats() const;
};

class binomialGB : public our_new_delete
{
  struct gbmin_elem : public our_new_delete
  {
    gbmin_elem *next;
    binomial_gb_elem *elem;
    int mask;
    gbmin_elem() {}  // For list header
    gbmin_elem(binomial_gb_elem *f, int mask0) : elem(f), mask(mask0) {}
  };

  struct monomial_list : public our_new_delete
  {
    monomial_list *next;
    monomial0 m;
    int mask;
    gbmin_elem *tag;  // This is a list
    monomial_list(monomial0 m0, int mask0, gbmin_elem *val)
        : m(m0), mask(mask0), tag(val)
    {
    }
  };

  const binomial_ring *R;
  gbmin_elem *first;
  int _max_degree;  // The highest degree of a GB generator found so far.

  // bool use_bigcell;
  bool is_homogeneous_prime;

 public:
  binomialGB(const binomial_ring *R, bool bigcell, bool homogprime);
  ~binomialGB();
  void enlarge(const binomial_ring *newR);

  void minimalize_and_insert(binomial_gb_elem *f);
  monomial_list *find_divisor(monomial_list *I, monomial0 m) const;
  monomial_list *ideal_quotient(monomial0 m) const;
  void remove_monomial_list(monomial_list *mm) const;

  binomial_gb_elem *find_divisor(monomial0 m) const;
  void make_new_pairs(binomial_s_pair_set *Pairs, binomial_gb_elem *f) const;
  void reduce_monomial(monomial0 m) const;
  bool reduce(binomial &f) const;  // returns false if reduction is to zero.

  class iterator
  {
    friend class binomialGB;
    gbmin_elem *elem;
    iterator(gbmin_elem *p) : elem(p) {}
    gbmin_elem *this_elem() { return elem; }
   public:
    iterator &operator++()
    {
      elem = elem->next;
      return *this;
    }
    iterator &operator++(int)
    {
      elem = elem->next;
      return *this;
    }
    binomial_gb_elem *operator*() { return elem->elem; }
    bool operator==(const iterator &p) const { return p.elem == elem; }
    bool operator!=(const iterator &p) const { return p.elem != elem; }
  };

  iterator begin() const { return iterator(first); }
  iterator end() const { return iterator(NULL); }
  int n_masks() const;
  void debug_display() const;
};

#define GB_FLAG_IS_HOMOGENEOUS 1
#define GB_FLAG_IS_NONDEGENERATE 2
#define GB_FLAG_BIGCELL 4

/**
    @ingroup gb

    @brief Non-functional.
*/
class binomialGB_comp : public GBComputation
{
  binomial_ring *R;
  binomial_s_pair_set *Pairs;  // Pairs and Generators
  binomialGB *Gmin;
  VECTOR(binomial_gb_elem *) Gens;  // All of the generators
  VECTOR(binomial_gb_elem *) G;  // All of the binomial_gb_elem's in one place.

  int top_degree;

  ///////////////////////
  // Flags and options //
  ///////////////////////

  // flags used during the computation:
  bool is_homogeneous;    // Generators will all be homogeneous,
                          // and elements of degree d will be added
                          // only BEFORE computing a GB in degree d+1.
                          // An error is given if a generator is added in
                          // after that point.
  bool is_nondegenerate;  // Assumed: the ideal has no linear factors:
                          // thus, if x.f is found to be in the ideal,
                          // f must already be in the ideal.
  bool use_bigcell;       // The generators given do not necessarily generate
                          // the entire ideal.  If an element of the form
                          // x.f (x a variable) is found, then we may add f
                          // to the ideal.
  bool flag_auto_reduce;  // Form full auto-reduction of GB elements.  This is
                          // the
                          // default.
  bool
      flag_use_monideal;  // If true, divisibility is done using monomial ideals
  // otherwise, divisibility is done using a linked list, and
  // monomial masks.  Currently, using monideals is not yet
  // implemented, so this is 'false'.

  VECTOR(binomial_gb_elem *) mingens;
  // Only valid for homogeneous case.  These point to GB elements
  // so don't free them by accident!

  VECTOR(binomial_gb_elem *) mingens_subring;  // Same comment applies here!

  void process_pair(binomial_s_pair p);
  ComputationStatusCode gb_done() const;

 public:
  // creation
  binomialGB_comp(const PolynomialRing *R,
                  int *wts,
                  bool revlex,
                  unsigned int options);
  virtual ~binomialGB_comp();

  virtual bool stop_conditions_ok();

  void enlarge(const PolynomialRing *R, int *wts);
  void add_generators(const Matrix *m);

  Matrix *subring();
  Matrix *subringGB();

  // reduction: these elements do not need to be binomials?
  Matrix *reduce(const Matrix *m, Matrix *&lift);

  void stats() const;

 public:
  //////////////////////////
  // Computation routines //
  //////////////////////////

  static binomialGB_comp *create(const Matrix *m,
                                 M2_bool collect_syz,
                                 int n_rows_to_keep,
                                 M2_arrayint gb_weights,
                                 int strategy,
                                 M2_bool use_max_degree,
                                 int max_degree);

  void remove_gb();

  void start_computation();

  virtual const Ring *get_ring() const
  {
    return 0;
  } /* doesn't have a ring !!  */
  virtual const Matrix /* or null */ *get_gb();

  virtual const Matrix /* or null */ *get_mingens();

  virtual const Matrix /* or null */ *get_initial(int nparts);

  virtual void text_out(buffer &o) const { /* to do */}
  /* This displays statistical information, and depends on the
     M2_gbTrace value */

  virtual int complete_thru_degree() const { return top_degree; }
  // The computation is complete up through this degree.

  ////////////////////////////////////////////////////////
  // The following are here because they need to be.
  // But they are not implemented... and there is no plan to implement them
  ////////////////////////////////////////////////////////

  virtual const Matrix /* or null */ *get_change();
  // not planned to be implemented

  virtual const Matrix /* or null */ *get_syzygies();
  // not planned to be implemented

  virtual const Matrix /* or null */ *matrix_remainder(const Matrix *m);
  // likely not planned to be implemented

  virtual M2_bool matrix_lift(const Matrix *m,
                              const Matrix /* or null */ **result_remainder,
                              const Matrix /* or null */ **result_quotient);
  // not planned to be implemented

  virtual int contains(const Matrix *m);
  // not planned to be implemented
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
