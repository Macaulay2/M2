/* Copyright 2003, Michael E. Stillman */

#ifndef _gbA_h_
#define _gbA_h_

#include "comp_gb.hpp"

#include "gbring.hpp"
#include "montable.hpp"
#include "gbasis.hpp"

class gbA : public GBComputation {
private:
  /* Types of minimality */
  enum spair_type {
    SPAIR_SPAIR,
    SPAIR_RING,
    SPAIR_SKEW,
    SPAIR_GEN,
    SPAIR_ELEM
  };

public:
  // This is only public to allow spair_sorter to use it!!
  struct spair {
    spair *next;
    spair_type type; /* SPAIR_SPAIR, SPAIR_GEN, SPAIR_ELEM, SPAIR_RING, SPAIR_SKEW */
    int deg;
    exponents lcm; /* Contains homogenizing variable, component */
    union {
      POLY f; /* SPAIR_GEN, SPAIR_ELEM */
      struct pair {
	int i,j; /* i refers to a GB element. j refers to GB element (SPAIR_SPAIR)
		    or a ring element (SPAIR_RING) or a variable number (SPAIR_SKEW) */
      } pair;
    } x;
    gbvector *&f() { return x.f.f; }
    gbvector *&fsyz() { return x.f.fsyz; }
  };

private:
  typedef vector<spair *> spairs;


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
  GBasis *G;

  SPairSet S;
  vector <gbvector *> _syz;

  int _strategy;
  int _this_degree;
  int _n_rows_per_syz;
  bool _collect_syz;
  bool _is_ideal;
  int _first_gb_element; /* First index past the skew variable squares, quotient elements,
			   in the array in G */

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
  
  /* initialization */
  void initialize(const Matrix *m, int csyz, int nsyz, int strat);
  spair *new_gen(int i, gbvector *f, ring_elem denom);

  void lead_exponents(gbvector *f, exponents e);
  void lead_exponents_deg(gbvector *f, exponents e, int deg);

  int gbelem_COMPONENT(GBasis::gbelem *g) { return g->g.f->comp; }
  int spair_COMPONENT(spair *s) { 
    // Only valid if this is an SPAIR_ELEM, SPAIR_RING, SPAIR_SKEW.
    // Probably better is to put it into spair structure.
    return G->gb[s->x.pair.i]->g.f->comp;
  }

  /* spair creation */
  /* negative indices index quotient ring elements */
  spair *spair_node();
  spair *spair_make(int i, int j);
  spair *spair_make_gen(POLY f);
  spair *spair_make_skew(int i, int v);
  spair *spair_make_ring(int i, int j);
  void spair_text_out(buffer &o, spair *p);
  void spair_delete(spair *&p);

  /* spair handling */
  bool pair_not_needed(spair *p, GBasis::gbelem *m);
  void remove_unneeded_pairs(int id);
  bool is_gcd_one_pair(spair *p);
  spairs::iterator choose_pair(spairs::iterator first, spairs::iterator next);
  void minimalize_pairs(spairs &new_set);
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
  bool s_pair_step();
  int computation_is_complete();

  /* Making the minimal GB */
  void poly_auto_reduce(vector<POLY> &mat);

  virtual bool stop_conditions_ok();

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

  virtual enum ComputationStatusCode gb_status(int *degree); 
  // The computation is complete up through this degree.

  /* Debug display routines */
  void debug_spair(spair *p);
  void debug_spairs(spair *spairlist);
  void debug_spair_array(spairs &spairlist);

};  

#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
