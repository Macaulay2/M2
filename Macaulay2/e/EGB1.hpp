// Copyright 1999  Michael E. Stillman
#ifndef _EGB1_hh_
#define _EGB1_hh_

#include "Espairs.hpp"
#include "Einterface.hpp"
#include "Elookup.hpp"

const int SP_SYZ = 1;
const int SP_RING = 2;
const int SP_SKEW = 3;
const int SP_GEN = 4;

enum comp_return_value
{
  COMP_ERROR = -2,
  COMP_INTERRUPTED = -1,
  COMP_DONE = 0,
  COMP_DONE_DEGREE_LIMIT = 1,
  COMP_DONE_LENGTH_LIMIT = 2,
  COMP_DONE_PAIR_LIMIT   = 4,
  COMP_DONE_GB_LIMIT     = 5,
  COMP_DONE_SYZ_LIMIT    = 6,
  COMP_DONE_CODIM        = 7,
  COMP_DONE_MIN_GENS     = 8,
  COMP_DONE_SUBRING_LIMIT= 10,
  COMP_DONE_STEPS        = 9,  // Possible Hilbert function return value
  COMP_COMPUTING = 100
};
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

struct ering_elem
{
  int degree;
  const exponent_vector *lcm;
  polynomial f;
};

struct egb_elem
{
  int degree;
  EVector f;
  EVector fsyz;
  const exponent_vector *lcm;  // Lead monomial
  // need denominator of fsyz, when computation is over a frac field.
  int me;       // Index starting at 1.
		// Once this element is no longer minimal,
                // this value is set to -me.
  bool is_minimal;  // Maybe also the largest 'n' for which 
		// this element is minimal for the subring.
  int npairs;	// Number of pairs remaining for this element.
  egb_elem() {}

  // infrastructure
  friend void i_Ecommands();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

typedef EMonomialLookupTable<egb_elem> EGBLookupTable;
typedef EMonomialLookupTable<ering_elem> ERingTable;

class ESPairLookupTable
{
  // This structure is only used to find a minimal set
  // of spairs: one inserts elements one degree at a time, in increasing
  // degree order.
  // For each element of a new degree: one must check ALL elements
  // of lower degree, I guess.  Then choosing a uniqe pairs
  // of each lcm must be done by the user, after which
  // the user appends these elements.
  struct node
  {
    node *next;
    es_pair *elem;
    unsigned int mask;
  };

  int nvars;
  node *table;  // This has a list head.
  node *last;
public:
  ESPairLookupTable(int nvars, es_pair *pairs);
  ~ESPairLookupTable();
  bool find_divisor(const exponent_vector *exp, es_pair *&result) const;
  void append(es_pair *&p);
  void append_list(es_pair *&pairs);
  es_pair *value();  // Returns a list of es_pair's.
};
class EGB1 : public EGroebnerComputation
{
  class iterator;
  friend class iterator;
private:
  // Ring information
  const EInterface I;
  const EFreeModule *F;
  const EFreeModule *Fsyz;
  ERingTable *ring_table;  // 0 if ring is not a quotient

  ESPairSet *spairs;
  es_pair *this_set;		// All the elements of this degree
  int this_degree;

  array< egb_elem * > gbLarge;
  array< EGBLookupTable * > gb;

  // Syzygies collected
  array< vec > syz;

  // statistics information
  int next_gb_num;
  int n_gb;
  int n_subring;
  int n_pairs;
  int n_computed;
  int n_saved_gcd;
  int n_saved_gcd_choice;
  int n_saved_lcm;

  // Syzygy type
  bool collect_syz;
  int n_comps_per_syz;

  // Flags
  bool is_ideal;
  bool find_trimmed;
  bool find_subring_trimmed;
  bool use_hilb_function;
  bool sugar_degree;  // This means that the degree stored in a egb_elem
				// and an es_pair is not the actual degree.
				// It also means that GB elements are inserted
				// in increasing sugar degrees.

  // Hilbert function use routines
  int n_in_degree;		// Only in use if use_hilb_function is true.

  // Options which are useful to have handy
  bool ring_is_skew;
  bool ring_is_quotient;
  bool coeffs_QQ;
  bool is_homogeneous;

  // Strategy information for the various choices
  int strategy;
  int *heuristicWeightVector;

  int nvars;
  // Stash for exponent vectors (this will probably go away with new engine code)
  stash *exponent_stash;
  int *mSkewVars;
  int *mReduceExp;  // to be used ONLY inside gb_reduce.
  int *mCancelExp; // to be used ONLY inside cancel_lead_term
private:
  int F_degree(int i) const;

  exponent_vector *make_skew_lcm(const exponent_vector *exp,
				 int v,
				 int deg_of_exp,
				 int & result_degree) const;
  exponent_vector *make_lcm(const exponent_vector *exp1,
			    const exponent_vector *exp2,
			    int deg_of_exp1,
			    int deg_of_exp2,
			    int & result_degree) const;
  
  // These are low level routines
  exponent_vector *new_exponent_vector() const;
  void remove_exponent_vector(exponent_vector *&a) const;
  void increment(egb_elem *p) const;
  void decrement(egb_elem *p) const;
  int lead_component(egb_elem *p) const { return I.lead_component(p->f); }
  int lead_component(es_pair *s) const;

  bool is_gcd_one_pair(es_pair *p) const;
  void remove_pairs(es_pair *&pair_list) const;

  int compare_pairs(es_pair *f, es_pair *g) const;

  es_pair *merge_pairs(es_pair *f, es_pair *g) const;
  void sort_pairs(es_pair * &p) const;

  void choose_nice_pair(es_pair *&p); 
  void choose_unique_pairs(es_pair * &p);
  
private:
  ERingTable *create_ring_table() const;
  bool set_up(const Matrix &m, int csyz, int nsyz, int strategy);

  bool search(const monomial *m, int comp, egb_elem *&result);
    // Find a GB element which divides m*comp, if one exists.
    // Problem: which one should one choose?

  void minimalize_pairs(es_pair *&p) ;
    // Takes a list of s-pairs, and chooses a set which generate.

  void update_pairs(egb_elem *p);
    // Update the set of s-pairs

  void compute_s_pair(es_pair *p, EVectorHeap &f, EVectorHeap &fsyz);
    // Make an s-pair.
  int gb_reduce(EVectorHeap &fh, EVectorHeap &fsyzh, EVector &f, EVector &fsz) const;
  void gb_reduce(EVector &f, EVector &fsyz) const;

  void auto_reduce_by(egb_elem *new_elem);

  void gb_insert(int degree, EVector &f, EVector &fsyz, bool minimal);
  // Insert the element 'f' as a new element in the GB.

  int is_computation_complete(const EStopConditions &stop) const;

  void s_pair_step(es_pair *p);

  // The following routines use polynomial and monoid arithmetic
  egb_elem *make_gb_elem(int degree,
			 EVector &f, 
			 EVector &fsyz,
			 bool minimal);

  es_pair *make_ring_s_pair(egb_elem *p, ering_elem *r) const;
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
  EGB1(const EMatrix &m, int collect_syz, int n_syz, int strategy);
  ~EGB1();

  virtual int new_calc(const EStopConditions &stop);

  class iterator {
    const EGB1 *comp;
    int i;
    EGBLookupTable::iterator p;
    void next() {
      for (--i; i >= 0; --i)
	{
	  p = comp->gb[i]->first();
	  if (p.valid())
	    return;
	}
    }
  public:
    iterator(const EGB1 *comp) : comp(comp), i(comp->F->rank()) { next(); }

    bool valid() { return (i >= 0 && p.valid()); }
    void operator++() { 
      ++p;
      if (p.valid()) return;
      else next();
    }    
    egb_elem *operator*() { return *p; }
  };

  iterator first() const { return iterator(this); }

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

  virtual void moreGenerators(int lo, int hi, const Matrix &m);
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
  void spair_debug_out(es_pair *q) const;
  void gb_debug_out(egb_elem *p) const;
  void debug_pairs(char *heading, es_pair *qlist) const;
  void spair_debug_out(buffer &o, es_pair *q) const;
  void gb_debug_out(buffer &o, egb_elem *p) const;
  void debug_pairs(buffer &o, char *heading, es_pair *qlist) const;

  // infrastructure
  friend void i_Ecommands();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  int length_of() const { return n_gb; }
};  
#endif
