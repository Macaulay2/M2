/* Copyright 2005, Michael E. Stillman */

#ifndef _F4Computation_h_
#define _F4Computation_h_

#include "../comp-gb.hpp"
#include "F4.hpp"
#include "gausser.hpp"

class PolynomialRing;
class FreeModule;
class RingType;

class F4Computation : public GBComputation
{
  // Interface to the F4 linear algebra GB computation
  // Handles: translation of polynomials to/from the correct form
  //          different possible template instantiations

  const PolynomialRing *originalR;
  const FreeModule *F; // determines whether the monomial order is a 
		       // Schreyer order.
                       // Also determines degrees of elements in F.
  const RingType *K;
  const Gausser *KK;
  MonomialInfo *MI;

  F4GB *f4;
public:
  F4Computation(Gausser *K,
		const Matrix *m, 
		M2_bool collect_syz, 
		int n_rows_to_keep,
		M2_arrayint gb_weights,
		int strategy, 
		M2_bool use_max_degree,
		int max_degree);

  virtual ~F4Computation();

  virtual void remove_gb() {} //MES: write this

  enum ComputationStatusCode computation_is_complete();

  virtual bool stop_conditions_ok() { return true; }

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
// End:
