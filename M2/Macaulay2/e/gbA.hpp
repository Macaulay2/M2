/* Copyright 2003, Michael E. Stillman */

#ifndef _gbA_h_
#define _gbA_h_

#include "comp_gb.hpp"

#include "gbring.hpp"
#include "montable.hpp"
#include "montableZZ.hpp"
#include "reducedgb.hpp"

#include "linalgGB/memblock.hpp"
#include "linalgGB/MonomialTable.hpp"

class GBWeight;

class gbA : public GBComputation {
public:
  enum gbelem_type { 
    ELEM_IN_RING,  // These are ring elements
    ELEM_POSSIBLE_MINGEN,   // These are min GB elements which might also be min gens
                            // In the graded case, they ARE minimal generators
    ELEM_MIN_GB,    // These are elements which are minimal GB elements
    ELEM_NON_MIN_GB // These are elements which are not minimal GB elements
  };

  struct gbelem : public our_new_delete {
    POLY g;
    int deg;
    int alpha; // the homogenizing degree
    exponents lead; // -1..nvars-1, the -1 part is the component
    gbelem_type minlevel;
  };

private:
  /* Types of minimality */
  enum spair_type {
    SPAIR_SPAIR,
    SPAIR_GCD_ZZ,
    SPAIR_RING,
    SPAIR_SKEW,
    SPAIR_GEN,
    SPAIR_ELEM
  };

public:
  // This is only public to allow spair_sorter to use it!!
  struct spair : public our_new_delete {
    spair *next;
    spair_type type; /* SPAIR_SPAIR, SPAIR_GCD_ZZ, 
			SPAIR_GEN, SPAIR_ELEM, SPAIR_RING, SPAIR_SKEW */
    int deg;
    exponents lcm; /* Contains homogenizing variable, component */
    union {
      POLY f; /* SPAIR_GEN, SPAIR_ELEM */
      struct pair {
	int i,j; /* i refers to a GB element. j refers to GB element (SPAIR_SPAIR)
		    or a ring element (SPAIR_RING) or a variable number (SPAIR_SKEW)
		    or an SPAIR_GCD_ZZ */
      } pair;
    } x;
    gbvector *&f() { return x.f.f; }
    gbvector *&fsyz() { return x.f.fsyz; }
  };

private:
  typedef std::vector<spair *, gc_allocator<spair *> > spairs;


  struct SPairSet {
    int nelems;  /* Includes the number in this_set */
    int n_in_degree; /* The number in 'this_set */
    spair *heap;
    int n_computed; /* The number removed via next() */

    // Each of the following is initialized on each call to spair_set_prepare_next_degree
    spair *spair_list;
    spair spair_deferred_list; // list header
    spair *spair_last_deferred; // points to last elem of previous list, possibly the header

    spair *gen_list; 
    spair gen_deferred_list;  // list header
    spair *gen_last_deferred; // points to last elem of previous list, possibly the header

    SPairSet();
  };
  
private:
  // Data
  const PolynomialRing *originalR;
  GBRing *R;
  const GBWeight *weightInfo_;
  M2_arrayint gb_weights;

  // Ring information

  const FreeModule *_F;
  const FreeModule *_Fsyz;
  int _nvars;
  Ring::CoefficientType _coeff_type;
  int _n_fraction_vars;

  std::vector<gbelem *, gc_allocator<gbelem *> > gb; // Contains any quotient ring elements

  ReducedGB *minimal_gb;
  //vector<POLY, gc_allocator<POLY> > minimal_gb; // Contains NO quotient ring elements
  bool minimal_gb_valid;

  const MonomialTable *ringtable;    // At most one of these two will be non-NULL.
  const MonomialTableZZ *ringtableZZ;

  MonomialTable *lookup;
  MonomialTableZZ *lookupZZ; // Only one of these two will be non-NULL.

  MemoryBlock monomial_memory; // Where we keep monomial space for 'lookups'
  MonomialLookupTable **lookups; // one for each component
  
  exponents EXP_; // Used in 'remainder'

  SPairSet S;
  std::vector<gbvector *, gc_allocator<gbvector *> > _syz;

  int _strategy;
  int _this_degree;
  int _n_rows_per_syz;
  bool _collect_syz;
  bool _is_ideal;
  int _first_gb_element; /* First index past the skew variable squares, quotient elements,
			   in the array in G */
  int _first_in_degree; /* for the current 'sugar' degree, this is the first GB element
			   inserted for this degree */
  int _complete_thru_this_degree; /* Used in reporting status to the user */

  /* stats */
  int _stats_ngcd1;
  int _stats_nreductions;
  int _stats_ntail;
  int _stats_ngb;
  int _stats_npairs;

  /* for ending conditions */
  int _n_syz;
  int _n_pairs_computed;
  int _n_gens_left;
  int _n_subring;

  // Hilbert function information
  bool _use_hilb;
  bool _hilb_new_elems;	// True if any new elements since HF was last computed
  int _hilb_n_in_degree; // The number of new elements that we expect to find
			 // in this degree.
  int _n_saved_hilb;
  const RingElement *_hf_orig;	// The Hilbert function that we are given at the beginning
  RingElement *_hf_diff;		// The difference between hf_orig and the computed hilb fcn
  Matrix *_hilb_matrix; // non-null if Hilbert function is being used or if the codim us
    // being used as a stop condition.

  // Reduction count: used to defer spairs which are likely to reduce to 0
  long max_reduction_count;
  void spair_set_defer(spair *&p);

  // Stashing previous divisors;
  long divisor_previous;
  long divisor_previous_comp;
private:
  bool over_ZZ() const { return _coeff_type == Ring::COEFF_ZZ; }
  
  /* initialization */
  void initialize(const Matrix *m, int csyz, int nsyz, M2_arrayint gb_weights, int strat);
  spair *new_gen(int i, gbvector *f, ring_elem denom);

  int gbelem_COMPONENT(gbelem *g) { return g->g.f->comp; }
  int spair_COMPONENT(spair *s) { 
    // Only valid if this is an SPAIR_ELEM, SPAIR_RING, SPAIR_SKEW.
    // Probably better is to put it into spair structure.
    return gb[s->x.pair.i]->g.f->comp;
  }

  gbelem *gbelem_ring_make(gbvector *f);

  gbelem *gbelem_make(gbvector *f,  // grabs f
		      gbvector *fsyz, // grabs fsyz
		      gbelem_type minlevel,
		      int deg);

  /* spair creation */
  /* negative indices index quotient ring elements */
  spair *spair_node();
  spair *spair_make(int i, int j);
  spair *spair_make_gcd_ZZ(int i, int j);
  spair *spair_make_gen(POLY f);
  spair *spair_make_skew(int i, int v);
  spair *spair_make_ring(int i, int j);
  void spair_text_out(buffer &o, spair *p);
  void spair_delete(spair *&p);

  /* spair handling */
  bool pair_not_needed(spair *p, gbelem *m);
  void remove_unneeded_pairs(int id);
  bool is_gcd_one_pair(spair *p);
  spairs::iterator choose_pair(spairs::iterator first, spairs::iterator next);
  void minimalize_pairs(spairs &new_set);
  void minimalize_pairs_ZZ(spairs &new_set);
  void minimalize_pairs_non_ZZ(spairs &new_set);
  void update_pairs(int id);

  /* spair set handling */
  void spair_set_insert(spair *p);
    /* Insert a LIST of s pairs into S */
  spair *spair_set_next(); 
    /* Removes the next element of the current degree, returning NULL if none left */
  int spair_set_determine_next_degree(int &nextdegree);
  int spair_set_prepare_next_degree(int &nextdegree);
    /* Finds the next degree to consider, returning the number of spairs in that degree */

  void spairs_sort(int len, spair *& list);
  void spairs_reverse(spair *&ps);

  /* Sorts the list of spairs 'list' (which has length 'len') */

  /* Hilbert function handling */
  void flush_pairs(); // Used to flush the rest of the pairs in the current degree.
  RingElementOrNull *compute_hilbert_function(); // Compute the HF of _hilb_matrix.
  Matrix *make_lead_term_matrix(); // The submodule of all lead terms

  /* reduction */
  void auto_reduce_by(int id);
  void compute_s_pair(spair *p);
  bool reduce(spair *p);
  void collect_syzygy(gbvector *fsyz);

  void insert(POLY f, gbelem_type minlevel);
  void handle_elem(POLY f, gbelem_type minlevel);
  bool s_pair_step();
  enum ComputationStatusCode computation_is_complete();


  virtual bool stop_conditions_ok() { return true; }

private:
  int insert(gbvector *f, gbvector *fsyz, gbelem_type minlevel, int deg);
    // returns integer index of this inserted element

  /* Making the minimal GB */

  void poly_auto_reduce(std::vector<POLY, gc_allocator<POLY> > &mat);
  void poly_auto_reduce_ZZ(std::vector<POLY, gc_allocator<POLY> > &mat);

  void remainder_by_ZZ(const FreeModule *F,
		       const FreeModule *Fsyz,
		       POLY &f,
		       const std::vector<POLY, gc_allocator<POLY> > &polys,
		       MonomialTableZZ *T);

  void minimalize_gb();

  int find_good_divisor(exponents e,
			int x,
			int degf, 
			int &result_alpha);

  int find_good_monomial_divisor_ZZ(
				    mpz_ptr c,
				    exponents e,
				    int x,
				    int degf, 
				    int &result_alpha);

  int find_good_term_divisor_ZZ(
				mpz_ptr c,
				exponents e,
				int x,
				int degf, 
				int &result_alpha);

  void remainder(POLY &f, int degf, bool use_denom, ring_elem &denom);
  // denom must start out as an element of the base R->getCoefficients().
  // denom is multiplied by the coefficient which is multiplied to f in order to
  // reduce f.
  // i.e. the result g satisfies: g = c*f mod GB, where new(denom) = c * old(denom).

  void remainder_ZZ(POLY &f, int degf, bool use_denom, ring_elem &denom);
  void remainder_non_ZZ(POLY &f, int degf, bool use_denom, ring_elem &denom);
public:
  //////////////////////////
  // Computation routines //
  //////////////////////////

  static gbA * create(const Matrix *m, 
		      M2_bool collect_syz, 
		      int n_rows_to_keep,
		      M2_arrayint gb_weights,
		      int strategy, 
		      M2_bool use_max_degree,
		      int max_degree);

  static gbA * create_forced(const Matrix *m, 
				 const Matrix *gb, 
				 const Matrix *mchange);

  virtual ~gbA();

  virtual int kind() { return 231; } // FIX THIS!!

  void start_computation();

  virtual const PolynomialRing *get_ring() { return originalR; }

  virtual ComputationOrNull *set_hilbert_function(const RingElement *h);

  virtual const MatrixOrNull *get_gb();

  virtual const MatrixOrNull *get_mingens();

  virtual const MatrixOrNull *get_change();

  virtual const MatrixOrNull *get_syzygies();

  virtual const MatrixOrNull *get_initial(int nparts);

  virtual const MatrixOrNull *matrix_remainder(const Matrix *m);

  virtual void matrix_lift(const Matrix *m,
			   MatrixOrNull **result_remainder,
			   MatrixOrNull **result_quotient
			   );

  virtual int contains(const Matrix *m);

  virtual void text_out(buffer &o); 
  /* This displays statistical information, and depends on the
     gbTrace value */

  virtual int complete_thru_degree() const;
  // The computation is complete up through this degree.

  /* Debug display routines */
  void debug_spair(spair *p);
  void debug_spairs(spair *spairlist);
  void debug_spair_array(spairs &spairlist);
  void showgb();
};  

#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
