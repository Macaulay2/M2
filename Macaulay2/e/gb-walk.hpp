/* Copyright 2007, Michael E. Stillman */

#ifndef _gb_walk_
#define _gb_walk_

#include "comp-gb.hpp"
#include "polyring.hpp"

class GBWalker : public GBComputation {
protected:
  virtual bool stop_conditions_ok();

  GBWalker(const Matrix *gb_under_order1,
	   const MonomialOrdering *order1);
public:
  static GBWalker * create(const Matrix *gb_under_order1,
			   const MonomialOrdering *order1);

  virtual ~GBWalker();

  // GBComputation and Computation inherited routines //  
  virtual void remove_gb();

  virtual void start_computation();

  virtual const PolynomialRing *get_ring();

  virtual ComputationOrNull *set_hilbert_function(const RingElement *h);

  virtual const MatrixOrNull *get_gb();

  virtual const MatrixOrNull *get_mingens();

  virtual const MatrixOrNull *get_change();

  virtual const MatrixOrNull *get_syzygies();

  virtual const MatrixOrNull *get_initial(int nparts);

  virtual const MatrixOrNull *get_parallel_lead_terms(M2_arrayint w);

  virtual const MatrixOrNull *matrix_remainder(const Matrix *m);

  virtual void matrix_lift(const Matrix *m,
			   MatrixOrNull **result_remainder,
			   MatrixOrNull **result_quotient
			   );

  virtual int contains(const Matrix *m);

  virtual void text_out(buffer &o) const; 
  /* This displays statistical information, and depends on the
     gbTrace value */

  virtual int complete_thru_degree() const;
  // The computation is complete up through this degree.

  /* Debug display routines */
  virtual void show() const;
  void show_mem_usage();
};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
