/* Copyright 2003, Michael E. Stillman */

#ifndef _gbA_h_
#define _gbA_h_

#include "comp_gb.hpp"

#include "gbring.hpp"
#include "montable.hpp"
#include "montableZZ.hpp"
#include "minimalgb.hpp"

class gbA : public GBComputation {
public:
  enum gbelem_type { 
    ELEM_IN_STONE,  // These are ring elements
    ELEM_TRIMMED,   // These are min GB elements which might also be min gens
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
  typedef vector<spair *,gc_alloc> spairs;


  struct SPairSet {
    int nelems;  /* Includes the number in this_set */
    int n_in_degree; /* The number in 'this_set */
    spair *heap;
    spair *this_set;
    int n_computed; /* The number removed via next() */

    SPairSet();
  };
  
private:
  // Data
  const PolynomialRing *originalR;
  GBRing *R;

  // Ring information

  const FreeModule *_F;
  const FreeModule *_Fsyz;
  int _nvars;
  Ring::CoefficientType _coeff_type;
  int _n_fraction_vars;

  vector<gbelem *,gc_alloc> gb; // Contains any quotient ring elements

  MinimalGB *minimal_gb;
  //vector<POLY,gc_alloc> minimal_gb; // Contains NO quotient ring elements
  bool minimal_gb_valid;

  MonomialTable *lookup;
  MonomialTableZZ *lookupZZ; // Only one of these two will be non-NULL.

  exponents _EXP; // Used in 'remainder'

  SPairSet S;
  vector <gbvector *,gc_alloc> _syz;

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
private:
  bool over_ZZ() const { return _coeff_type == Ring::COEFF_ZZ; }
  
  /* initialization */
  void initialize(const Matrix *m, int csyz, int nsyz, int strat);
  spair *new_gen(int i, gbvector *f, ring_elem denom);

  void lead_exponents(gbvector *f, exponents e);
  void lead_exponents_deg(gbvector *f, exponents e, int deg);

  int gbelem_COMPONENT(gbelem *g) { return g->g.f->comp; }
  int spair_COMPONENT(spair *s) { 
    // Only valid if this is an SPAIR_ELEM, SPAIR_RING, SPAIR_SKEW.
    // Probably better is to put it into spair structure.
    return gb[s->x.pair.i]->g.f->comp;
  }

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

  /* reduction */
  void auto_reduce_by(int id);
  void compute_s_pair(spair *p);
  bool reduce(spair *p);
  void collect_syzygy(gbvector *fsyz);

  void insert(POLY f, int minlevel);
  void handle_elem(POLY f, int minlevel);
  bool s_pair_step();
  enum ComputationStatusCode computation_is_complete();


  virtual bool stop_conditions_ok() { return true; }

private:
  int insert(gbvector *f, gbvector *fsyz, gbelem_type minlevel, int deg);
    // returns integer index of this inserted element

  /* Making the minimal GB */

  void poly_auto_reduce(vector<POLY,gc_alloc> &mat);
  void poly_auto_reduce_ZZ(vector<POLY,gc_alloc> &mat);

  void remainder_by_ZZ(const FreeModule *F,
		       const FreeModule *Fsyz,
		       POLY &f,
		       const vector<POLY,gc_alloc> &polys,
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
  // denom must start out as an element of the base R->get_flattened_coefficients().
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

  virtual int gb_complete_thru_degree();
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
