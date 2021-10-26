// Copyright 1997-2006  Michael E. Stillman
#ifndef _gb2_hh_
#define _gb2_hh_

#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp-res.hpp"
#include "hilb.hpp"
#include "spair.hpp"

#define STATE_DONE 0
#define STATE_NEW_DEGREE 1
#define STATE_HILB 2
#define STATE_GB 3
#define STATE_GENS 4

////////////////////////////
// Computation node types //
////////////////////////////

class gb_node : public our_new_delete
{
 public:
  virtual ~gb_node() {}
  virtual void set_output(gb_node *p) = 0;

  // The following two routines return one of
  // COMP_DONE, COMP_DONE_*, COMP_COMPUTING, COMP_INTERRUPTED

  virtual enum ComputationStatusCode calc_gb(int degree) = 0;
  virtual enum ComputationStatusCode calc_gens(int degree) = 0;

  virtual bool
  is_done() = 0;  // Returns true if computation is completely done,
                  // if no other generators are received.

  virtual bool receive_generator(gbvector *f, int n, const ring_elem denom) = 0;
  // returns true if f is minimal.  'denom' is only used in the 'origsyz>0'
  // case.

  // Reduction of a vector f (in correct free module), and its rep 'fsyz'.
  virtual void reduce(gbvector *&f, gbvector *&fsyz) = 0;

  // Hilbert functions of these nodes...
  // Both of these return 0 if computed, non-zero if interrupted.
  virtual RingElement /* or null */ *hilbertNumerator() = 0;

  virtual int n_gb_elems() const = 0;
  virtual const FreeModule *output_free_module() const = 0;
  virtual Matrix *min_gens_matrix() = 0;
  virtual Matrix *get_matrix() = 0;
  virtual Matrix *initial_matrix(int n) = 0;
  virtual Matrix *gb_matrix() = 0;
  virtual Matrix *change_matrix() = 0;
  virtual void text_out(buffer &o) const = 0;
  virtual void stats() const = 0;
};

////////////////////////////
// Specific node types /////
////////////////////////////

class gb_emitter : public gb_node
{
  const PolynomialRing *originalR;
  GBRing *GR;
  const Matrix *gens;
  gb_node *g;
  int this_degree;
  int n_left;
  int n_i;
  int n_gens;
  int *these;

  void flush();
  int start_degree(int deg);

 public:
  gb_emitter(const Matrix *m);
  ~gb_emitter();
  virtual void set_output(gb_node *gg) { g = gg; }
  virtual enum ComputationStatusCode calc_gb(int degree);
  virtual enum ComputationStatusCode calc_gens(int degree);

  virtual bool is_done();

  virtual bool receive_generator(gbvector *, int, const ring_elem)
  {
    return false;
  }

  // Reduction of a vector f (in correct free module), and its rep 'fsyz'.
  virtual void reduce(gbvector *&, gbvector *&) {}
  // The following routine should NEVER be called
  virtual RingElement /* or null */ *hilbertNumerator();

  virtual int n_gb_elems() const { return 0; }
  virtual const FreeModule *output_free_module() const { return gens->rows(); }
  virtual Matrix *get_matrix() { return const_cast<Matrix *>(gens); }
  virtual Matrix *min_gens_matrix() { return 0; }
  virtual Matrix *initial_matrix(int) { return 0; }
  virtual Matrix *gb_matrix() { return 0; }
  virtual Matrix *change_matrix() { return 0; }
  virtual void text_out(buffer &o) const;
  virtual void stats() const;
};

class gb2_comp : public gb_node
{
 private:
  // Ring information
  const PolynomialRing *originalR;
  GBRing *GR;
  const Monoid *M;  // flattened monomials (same as originalR->getMonoid())
  const Ring
      *K;  // flattened coefficients (same as originalR->getCoefficients())
  stash *mi_stash;  // owned by the creator of this node

  FreeModule *F;
  FreeModule *Fsyz;  // This is a Schreyer module

  int level;  // what level is this?
  int state;  // STATE_NEW_DEGREE, STATE_GB, STATE_GENS, STATE_HILB
  int this_degree;
  int n_gb_first;  // First GB element in the current degree.
                   // (or previous degree, if state = STATE_NEW_DEGREE)

  s_pair_heap *spairs;
  s_pair *these_pairs;
  intarray total_pairs;

  VECTOR(gb_elem *) gb;
  VECTOR(monideal_pair *) monideals;  // baggage for each is 'gb_elem *'

  // Syzygies collected
  gb_node *syz;
  gb_node *gens;

  // statistics information, much is kept with the s_set
  int n_gb;
  int n_mingens;
  int n_subring;
  int n_syz;

  int n_pairs;           // Total number of pairs
  int n_pairs_computed;  // Number of pairs total computed (sum of next 6
                         // integers)
  int n_pairs_syz;       // #pairs which produced non-zero syzygies
  int n_pairs_usyz;      // #pairs which produced zero syzygies, after reduction
  int n_pairs_gb;        // #pairs which produced gb elements
  int n_pairs_zero;      // #pairs which reduced to 0 (syz reduced to 0 too)
  int n_pairs_hilb;      // #pairs which were not done, due to HF info
  int n_pairs_gcd;       // #pairs which were not done, due to gcd=1 pairs

  // Syzygy type
  int orig_syz;  // >=0 means how many components to keep.
                 // < 0 means compute syz's on minimal gens.

  int strategy_flags;  // STRATEGY_LONGPOLYNOMIALS, USE_SORT are the current
                       // flags used

  // Hilbert function information
  char use_hilb;
  RingElement *hf;    // The Hilbert function, as so far computed
  int hf_numgens_gb;  // The HF has been computed for this many GB elements.
                      // (Used to determine whether to recompute HF).
  int hf_numgens_F;   // The HF was computed using this size of F.
  int n_gb_syz;

 private:
  void setup(FreeModule *Fsyz,
             stash *mi_stash,
             gb_node *gens,
             int lodegree,
             int origsyz,
             int level,
             int strategy);

  // S-pair control
  s_pair *new_ring_pair(gb_elem *p, const int *lcm);
  s_pair *new_s_pair(gb_elem *p, gb_elem *q, const int *lcm);
  void remove_pair(s_pair *&p);

  void find_pairs(gb_elem *p);
  void compute_s_pair(s_pair *p);
  void gb_reduce(gbvector *&f, gbvector *&fsyz);
  void gb_geo_reduce(gbvector *&f, gbvector *&fsyz);
  void gb_insert(gbvector *f, gbvector *fsyz, int ismin);

  int gb_sort_partition(int lo, int hi);
  void gb_sort(int lo, int hi);

  void flush_pairs();
  Matrix *make_lead_term_matrix();  // for computing hilbert functions

  void schreyer_append(gbvector *f);
  bool s_pair_step();
  int get_pairs();

 public:
  gb2_comp(FreeModule *Fsyz,
           stash *mi_stash,
           gb_node *gens,
           int lodegree,
           int orig_syz,
           int level,
           int strategy);

  ~gb2_comp();
  virtual void set_output(gb_node *p);

  // calc returns COMP_DONE_*, or COMP_DONE, or COMP_INTERRUPTED.
  bool receive_generator(gbvector *f, int n, const ring_elem denom);
  void end_degree();
  bool is_done();

  enum ComputationStatusCode calc_gb(int deg);
  enum ComputationStatusCode calc_gens(int deg);

  virtual void reduce(gbvector *&f, gbvector *&fsyz);

  virtual RingElement /* or null */ *hilbertNumerator();

  // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
  int n_gb_elems() const { return n_gb; }
  const FreeModule *output_free_module() const { return Fsyz; }
  Matrix *min_gens_matrix();
  Matrix *get_matrix();
  Matrix *initial_matrix(int n);
  Matrix *gb_matrix();
  Matrix *change_matrix();

  void debug_out(s_pair *q) const;
  void debug_out(buffer &o, s_pair *q) const;
  virtual void text_out(buffer &o) const;
  void stats() const;
};

/**
    @ingroup res

    @brief One of the Resolution computations, based on computing step by step.

    Induced (Schreyer) orders are used to speed up computation.
*/
class gbres_comp : public ResolutionComputation
{
 private:
  const PolynomialRing *originalR;
  stash *mi_stash;  // for all of the nodes of the computation
  GBRing *GR;
  int n_nodes;
  gb_node **nodes;

  int lo_degree;
  int last_completed_degree;
  int strategy_flags;

 private:
  void setup(const Matrix *m, int length, int origsyz, int strategy);

 protected:
  bool stop_conditions_ok();

 public:
  gbres_comp(const Matrix *m, int length, int orig_syz, int strategy);
  gbres_comp(const Matrix *m,
             int length,
             int orig_syz,
             const RingElement *hf,
             int strategy);

  virtual ~gbres_comp();

  // Performing the computation
  void start_computation();

  bool is_done();

  // reduction
  Matrix *reduce(const Matrix *m, Matrix *&lift);

  // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
  M2_arrayint betti_minimal() const;

  const FreeModule *free_module(int level) const;
  const Matrix *min_gens_matrix(int level);
  const Matrix *initial_matrix(int n, int level);
  const Matrix *gb_matrix(int level);
  const Matrix *change_matrix(int level);

  int complete_thru_degree() const;
  // The computation is complete up through this slanted degree.

  const Matrix /* or null */ *get_matrix(int level);

  const FreeModule /* or null */ *get_free(int level)
  {
    return free_module(level);
  }

  M2_arrayint get_betti(int type) const;
  // type is documented under rawResolutionBetti, in engine.h

  //////////////////////////////////////
  // Statistics and spair information //
  //////////////////////////////////////

  void text_out(buffer &o) const;
  // This displays statistical information, and depends on the
  // M2_gbTrace value.

  void stats() const;
  // Same as text_out, but writes its information directly, so as not
  // to encounter huge allocation of strings.
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
