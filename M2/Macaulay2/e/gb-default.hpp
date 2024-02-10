/* Copyright 2003, Michael E. Stillman */

#ifndef _gbA_h_
#define _gbA_h_

#include "comp-gb.hpp"

#include "gbring.hpp"
#include "montable.hpp"
#include "montableZZ.hpp"
#include "reducedgb.hpp"

class GBWeight;

const int ELEM_IN_RING = 1;
const int ELEM_MINGEN = 2;
const int ELEM_MINGB = 4;

/**
    @ingroup gb

    @brief The default Groebner basis computation class.
*/
class gbA : public GBComputation
{
 public:
  typedef int gbelem_type;
  // ELEM_IN_RING: a bit set.  If this is set, then the next two bits need to
  //   off.
  // ELEM_MINGEN: a bit set: 0 means it is provably not needed to
  //   generated the ideal or submodule.  1 means it might be a minimal
  //   generator.
  //   NOTE: often the min gens obtained this way are not so good.  Exceptions
  //   are
  //    for graded submodules.  Then the meaning of 1 is: it IS a min gen.
  // ELEM_MINGB: a bit set: 0 means is not part of the min gb (so far).  1 means
  // it is.
  // SO: possible values are 0, 1, 2, 4, 6.
  // Test using code like this:
  //   if (g->minlevel & ELEM_MINGEN) { .. do this if it is a possible min gen
  //   .. } else { .. if not .. };

  struct gbelem
  {
    POLY g;
    int deg;
    int gap;  // the homogenizing degree, called "alpha", or the "ecart".  It is
              // deg g - deg lead term of g.
    int size;  // number of monomials, when the element is first inserted. After
    // auto reduction, the number can change.  It is not yet clear if we should
    // change this value...
    exponents_t lead;  // -1..nvars-1, the -1 part is the component
    gbelem_type minlevel;
  };

  /* Types of minimality */
  enum SPAIR {
    SPAIR_SPAIR,
    SPAIR_GCD_ZZ,
    SPAIR_RING,
    SPAIR_SKEW,
    SPAIR_GEN,
    SPAIR_ELEM
  };

 public:
  // This is only public to allow spair_sorter to use it!!
  struct spair
  {
    spair *next;
    SPAIR type; /* SPAIR_SPAIR, SPAIR_GCD_ZZ,
                        SPAIR_GEN, SPAIR_ELEM, SPAIR_RING, SPAIR_SKEW */
    int deg;
    exponents_t lcm; /* Contains homogenizing variable, component */
    union
    {
      POLY f; /* SPAIR_GEN, SPAIR_ELEM */
      struct pair
      {
        int i, j; /* i refers to a GB element. j refers to GB element
                     (SPAIR_SPAIR)
                     or a ring element (SPAIR_RING) or a variable number
                     (SPAIR_SKEW)
                     or an SPAIR_GCD_ZZ */
      } pair;
    } x;
    gbvector *lead_of_spoly;  // experimental
    gbvector *&f() { return x.f.f; }
    gbvector *&fsyz() { return x.f.fsyz; }
  };

 private:
  typedef VECTOR(spair *) spairs;

  struct SPairSet : public our_new_delete
  {
    int nelems;      /* Includes the number in this_set */
    int n_in_degree; /* The number in 'this_set */
    spair *heap;
    int n_computed; /* The number removed via next() */

    // Each of the following is initialized on each call to
    // spair_set_prepare_next_degree
    spair *spair_list;
    spair spair_deferred_list;   // list header
    spair *spair_last_deferred;  // points to last elem of previous list,
                                 // possibly the header

    spair *gen_list;
    spair gen_deferred_list;   // list header
    spair *gen_last_deferred;  // points to last elem of previous list, possibly
                               // the header

    SPairSet();
  };

 private:
  // Stashes
  stash *spair_stash;
  stash *gbelem_stash;

  size_t exp_size;  // in bytes
  stash *lcm_stash;

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
  int n_fraction_vars;
  bool is_local_gb;

  VECTOR(gbelem *) gb;  // Contains any quotient ring elements
  VECTOR(int)
  forwardingZZ;  // forwarding[i] is the replaced gb element (-1 = none)
  // note that forwarding[forwarding[i]] might not be -1 either.
  // so to use this, loop through and get final value.
  int get_resolved_gb_index(int i) const;

  ReducedGB *minimal_gb;
  bool minimal_gb_valid;

  MonomialTable *ringtable;  // At most one of these two will be non-NULL.
  const MonomialTableZZ *ringtableZZ;

  MonomialTable *lookup;
  MonomialTableZZ *lookupZZ;  // Only one of these two will be non-NULL.

  SPairSet *S;

  VECTOR(gbvector *) _syz;

  int _strategy;
  int n_rows_per_syz;
  bool _collect_syz;
  bool _is_ideal;
  int first_gb_element; /* First index past the skew variable squares, quotient
                           elements,
                           in the array in G */
  int first_in_degree;  /* for the current 'sugar' degree, this is the first GB
                           element
                            inserted for this degree */
  int complete_thru_this_degree; /* Used in reporting status to the user */

  /* (new) state machine */
  enum gbA_state {
    STATE_NEWDEGREE,
    STATE_NEWPAIRS,
    STATE_HILB,
    STATE_SPAIRS,
    STATE_GENS,
    STATE_AUTOREDUCE,
    STATE_DONE
  } state;
  int this_degree;
  int npairs;  // in this_degree
  int np_i;
  int ar_i;
  int ar_j;
  int n_gb;

  /* stats */
  int stats_ngcd1;
  int stats_nreductions;
  int stats_ntail;
  int stats_ngb;
  int stats_npairs;
  int stats_nretired;

  /* for ending conditions */
  int n_syz;
  int n_pairs_computed;
  int n_gens_left;
  int n_subring;

  // Hilbert function information
  bool use_hilb;
  bool hilb_new_elems;   // True if any new elements since HF was last computed
  int hilb_n_in_degree;  // The number of new elements that we expect to find
                         // in this degree.
  int n_saved_hilb;
  const RingElement
      *hf_orig;  // The Hilbert function that we are given at the beginning
  RingElement
      *hf_diff;  // The difference between hf_orig and the computed hilb fcn

  // Reduction count: used to defer spairs which are likely to reduce to 0
  long max_reduction_count;
  void spair_set_defer(spair *&p);

  // Stashing previous divisors;
  int divisor_previous;
  int divisor_previous_comp;

 private:
  exponents_t exponents_make();

  bool over_ZZ() const { return _coeff_type == Ring::COEFF_ZZ; }
  /* initialization */
  void initialize(const Matrix *m,
                  int csyz,
                  int nsyz,
                  M2_arrayint gb_weights,
                  int strat,
                  int max_reduction_count0);
  spair *new_gen(int i, gbvector *f, ring_elem denom);

  int gbelem_COMPONENT(gbelem *g) { return g->g.f->comp; }
  int spair_COMPONENT(spair *s)
  {
    // Only valid if this is an SPAIR_ELEM, SPAIR_RING, SPAIR_SKEW.
    // Probably better is to put it into spair structure.
    return gb[s->x.pair.i]->g.f->comp;
  }

  /* new state machine routines */
  void insert_gb(POLY f, gbelem_type minlevel);
  void replace_gb_element_ZZ(MonomialTableZZ::mon_term *t);

  bool process_spair(spair *p);
  void do_computation();

  gbelem *gbelem_ring_make(gbvector *f);

  gbelem *gbelem_make(gbvector *f,     // grabs f
                      gbvector *fsyz,  // grabs fsyz
                      gbelem_type minlevel,
                      int deg);

  gbelem *gbelem_copy(gbelem *g);  // (deep) copy of g

  void gbelem_text_out(buffer &o, int i, int nterms = 3) const;

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
  bool spair_is_retired(spair *p) const;

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
  void remove_spair_list(spair *&set);
  void remove_SPairSet();
  void spair_set_insert(spair *p);
  /* Insert a LIST of s pairs into S */
  spair *spair_set_next();
  /* Removes the next element of the current degree, returning NULL if none left
   */
  int spair_set_determine_next_degree(int &nextdegree);
  int spair_set_prepare_next_degree(int &nextdegree);
  /* Finds the next degree to consider, returning the number of spairs in that
   * degree */
  void spair_set_show_mem_usage();

  void spairs_sort(int len, spair *&list);
  void spairs_reverse(spair *&ps);

  void spair_set_lead_spoly(spair *p);

  /* Sorts the list of spairs 'list' (which has length 'len') */

  /* Hilbert function handling */
  void
  flush_pairs();  // Used to flush the rest of the pairs in the current degree.
  RingElement /* or null */ *
  compute_hilbert_function();       // Compute the HF of _hilb_matrix.
  Matrix *make_lead_term_matrix();  // The submodule of all lead terms

  /* reduction */
  void auto_reduce_by(int id);
  void compute_s_pair(spair *p);
  bool reduceit(spair *p);
  void collect_syzygy(gbvector *fsyz);

  enum ComputationStatusCode computation_is_complete();

  virtual bool stop_conditions_ok() { return true; }
 private:
  /* Making the minimal GB */

  bool reduce_ZZ(spair *p);
  bool reduce_kk(spair *p);

  void poly_auto_reduce(VECTOR(POLY) & mat);
  void poly_auto_reduce_ZZ(VECTOR(POLY) & mat);

  void remainder_by_ZZ(const FreeModule *F,
                       const FreeModule *Fsyz,
                       POLY &f,
                       const VECTOR(POLY) & polys,
                       MonomialTableZZ *T);

  void minimalize_gb();

  int find_good_divisor(exponents_t e, int x, int degf, int &result_gap);

  int find_good_monomial_divisor_ZZ(mpz_srcptr c,
                                    exponents_t e,
                                    int x,
                                    int degf,
                                    int &result_gap);

  int find_good_term_divisor_ZZ(mpz_srcptr c,
                                exponents_t e,
                                int x,
                                int degf,
                                int &result_gap);

  void remainder(POLY &f, int degf, bool use_denom, ring_elem &denom);
  // denom must start out as an element of the base R->getCoefficients().
  // denom is multiplied by the coefficient which is multiplied to f in order to
  // reduce f.
  // i.e. the result g satisfies: g = c*f mod GB, where new(denom) = c *
  // old(denom).

  void remainder_ZZ(POLY &f, int degf, bool use_denom, ring_elem &denom);
  void tail_remainder_ZZ(POLY &f, int degf);
  // Used when replacing GB element with one with smaller coeff
  void remainder_non_ZZ(POLY &f, int degf, bool use_denom, ring_elem &denom);

 public:
  //////////////////////////
  // Computation routines //
  //////////////////////////

  static gbA *create(const Matrix *m,
                     M2_bool collect_syz,
                     int n_rows_to_keep,
                     M2_arrayint gb_weights,
                     int strategy,
                     M2_bool use_max_degree,
                     int max_degree,
                     int max_reduction_count);

  static gbA *create_forced(const Matrix *m,
                            const Matrix *gb,
                            const Matrix *mchange);

  void remove_gb();
  virtual ~gbA();

  virtual int kind() { return 231; }  // FIX THIS!!
  void start_computation();

  virtual const PolynomialRing *get_ring() const { return originalR; }
  virtual Computation /* or null */ *set_hilbert_function(const RingElement *h);

  virtual const Matrix /* or null */ *get_gb();

  virtual const Matrix /* or null */ *get_mingens();

  virtual const Matrix /* or null */ *get_change();

  virtual const Matrix /* or null */ *get_syzygies();

  virtual const Matrix /* or null */ *get_initial(int nparts);

  virtual const Matrix /* or null */ *get_parallel_lead_terms(M2_arrayint w);

  virtual const Matrix /* or null */ *matrix_remainder(const Matrix *m);

  virtual M2_bool matrix_lift(const Matrix *m,
                              const Matrix /* or null */ **result_remainder,
                              const Matrix /* or null */ **result_quotient);

  virtual int contains(const Matrix *m);

  virtual void text_out(buffer &o) const;
  /* This displays statistical information, and depends on the
     M2_gbTrace value */

  virtual int complete_thru_degree() const;
  // The computation is complete up through this degree.

  /* Debug display routines */
  void debug_spair(spair *p);
  void debug_spairs(spair *spairlist);
  void debug_spair_array(spairs &spairlist);
  void show() const;

  void show_mem_usage();
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
