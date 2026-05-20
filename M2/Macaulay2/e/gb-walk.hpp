/* Copyright 2007, Michael E. Stillman */

#ifndef _gb_walk_
#define _gb_walk_

#include "comp-gb.hpp"
#include "engine-includes.hpp"
#include "interface/monomial-ordering.h"  // for MonomialOrdering
#include "polyring.hpp"

class Computation;
class FreeModule;
class GBRing;
class MarkedGB;
class Matrix;
class RingElement;
class buffer;

struct POLY;
struct gbvector;

/**
    @ingroup gb

    @brief The generic Groebner walk algorithm.  Not yet working?
*/

class GBWalker : public GBComputation
{
  const GBRing *R;
  const FreeModule *F;  // The free module containing the elements of the GB
  MarkedGB *G;          // This ends up being the answer...

  VECTOR(M2_arrayint) w_history;

  long **monorder1;  // array 0..nvars-1 each of array 0..nvars-1
  long **monorder2;  // same

  void initialize();

  GBComputation *make_gb(const Matrix *M) const;

  bool compute_next_w();

  // local data in the computation (kept because of possible interrupts)
  enum {
    STATE_compute_w,
    STATE_do_gb,
    STATE_reduce,
    STATE_autoreduce,
    STATE_done
  } state;
  M2_arrayint ww;
  const Matrix *inwwG;
  GBComputation *gb_inwwG;
  int next_to_reduce;
  const Matrix *H;
  VECTOR(gbvector *) leadterms;
  VECTOR(POLY) polys;
  MarkedGB *G1;  // becomes G eventually
 protected:
  virtual bool stop_conditions_ok();

  GBWalker(const Matrix *gb_under_order1, const MonomialOrdering *order1);

  GBWalker(MarkedGB *G0, long **order1, long **order2);

 public:
  static GBWalker *create(MarkedGB *G0, long **order1, long **order2);

  static GBWalker *create(const Matrix *gb_under_order1,
                          const MonomialOrdering *order1);

  virtual ~GBWalker();

  // GBComputation and Computation inherited routines //
  virtual void remove_gb();

  virtual void start_computation();

  virtual const PolynomialRing *get_ring() const;

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
  virtual void show() const;
  void show_mem_usage();
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
