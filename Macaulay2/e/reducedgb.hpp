// Copyright 2004 Michael E. Stillman.

#ifndef _reducedgb_hpp_
#define _reducedgb_hpp_

#include "comp-gb.hpp"
#include <vector>
#include "gbring.hpp"
#include "montable.hpp"
#include "montableZZ.hpp"
#include "gbweight.hpp"

class ReducedGB : public GBComputation
{
protected:
  GBRing *R;
  const PolynomialRing *originalR;
  const FreeModule *F;
  const FreeModule *Fsyz;
  VECTOR(POLY) polys;

  virtual bool stop_conditions_ok() { return true; }
  // If the stop conditions in _Stop are inappropriate,
  // return false, and use ERROR(...) to provide an error message.

  ReducedGB(GBRing *R0,
	    const PolynomialRing *originalR0,
	    const FreeModule *F0,
	    const FreeModule *Fsyz0);
public:

  virtual ~ReducedGB();

  static ReducedGB *create(const PolynomialRing *originalR0,
			   const FreeModule *F0,
			   const FreeModule *Fsyz0,
			   const GBWeight *wt0 = 0);

  virtual GBComputation * cast_to_GBComputation() { return this;} 

  virtual void start_computation() {}

  virtual int complete_thru_degree() const { return 0; }
  // The computation is complete up through this degree.

  // Recall that the status of the computation is maintained by the Computation class,

  ////////////////////////////////
  // Results of the computation //
  ////////////////////////////////
  virtual const MatrixOrNull *get_gb();

  virtual const MatrixOrNull *get_mingens();

  virtual const MatrixOrNull *get_change();

  virtual const MatrixOrNull *get_syzygies();

  virtual const MatrixOrNull *get_initial(int nparts);

  //////////////////////////////////////
  // Statistics and spair information //
  //////////////////////////////////////

  virtual void text_out(buffer &o);
  // This displays statistical information, and depends on the
  // gbTrace value.

  ////////////////////////////////
  // Normal forms and lifting ////
  ////////////////////////////////

  virtual const MatrixOrNull *matrix_remainder(const Matrix *m);

  virtual void matrix_lift(const Matrix *m,
			   MatrixOrNull **result_remainder,
			   MatrixOrNull **result_quotient);

  virtual int contains(const Matrix *m);

  ////////////////////////////////////////////////
  // The following are the functions which need //
  // to be provided by subclasses               //
  ////////////////////////////////////////////////

  virtual void set_gb(VECTOR(POLY) &polys0) = 0;

  virtual void minimalize(const VECTOR(POLY) &polys0,
			  bool auto_reduce=true) = 0;
  // I have to decide: does this ADD to the existing set?

  // Choose a minimal set of generators of the lead terms.
  // sort the resulting elements
  // auto reduce them
  // This class will be subclassed by:
  //   base is a field
  //   base is ZZ, strong GB
  //   base is ZZ, weak GB
  //   base is a frac field, # frac vars is given.
  //   ring has a local term order: reduction can not be complete...

  const VECTOR(POLY) &get() const { return polys; }

  virtual void remainder(POLY &f, bool use_denom, ring_elem &denom) = 0;
  // WARNING: this should only be used with term orders!
  // REALLY??

  virtual void remainder(gbvector *&f, bool use_denom, ring_elem &denom) = 0;
};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
