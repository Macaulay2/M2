/* Copyright 2005, Michael E. Stillman */

#ifndef _F4Computation_h_
#define _F4Computation_h_

#include "../comp-gb.hpp"
#include "f4.hpp"
#include "gausser.hpp"
#include <polyring.hpp>

class PolynomialRing;
class FreeModule;
class RingType;

class F4Computation : public GBComputation
{
  // Interface to the F4 linear algebra GB computation
  // Handles: translation of polynomials to/from the correct form
  //          different possible template instantiations

  const PolynomialRing *originalR;
  const FreeModule *F;  // determines whether the monomial order is a
                        // Schreyer order.
                        // Also determines degrees of elements in F.
  const RingType *K;
  const Gausser *KK;
  MonomialInfo *MI;
  F4Mem *Mem;

  F4GB *f4;

 public:
  F4Computation(Gausser *K,
                F4Mem *Mem,
                const Matrix *m,
                M2_bool collect_syz,
                int n_rows_to_keep,
                M2_arrayint gb_weights,
                int strategy,
                M2_bool use_max_degree,
                int max_degree);

  virtual ~F4Computation();

  virtual void remove_gb() { delete f4; }
  enum ComputationStatusCode computation_is_complete();

  virtual bool stop_conditions_ok() { return true; }
  void start_computation();

  virtual const PolynomialRing *get_ring() const { return originalR; }
  virtual Computation /* or null */ *set_hilbert_function(const RingElement *h);

  virtual const Matrix /* or null */ *get_gb();

  virtual const Matrix /* or null */ *get_mingens();

  virtual const Matrix /* or null */ *get_change();

  virtual const Matrix /* or null */ *get_syzygies();

  virtual const Matrix /* or null */ *get_initial(int nparts);

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

  virtual void show() const;  // debug display
};

GBComputation *createF4GB(const Matrix *m,
                          M2_bool collect_syz,
                          int n_rows_to_keep,
                          M2_arrayint gb_weights,
                          int strategy,
                          M2_bool use_max_degree,
                          int max_degree);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
