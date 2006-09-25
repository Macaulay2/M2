// Copyright 2004 Michael E. Stillman.

#ifndef _comp_gb_declared_hpp_
#define _comp_gb_declared_hpp_

#include "comp_gb.hpp"
#include "reducedgb.hpp"

class GBDeclared : public GBComputation
// This contains a GBComputation, which can be changed.
// For example, we can start with a computation, and then
// after it is done, we can jettison it, and consider only
// the GB object itself.
{
  ReducedGB *G;
  const Matrix *trimmed_gens;
  const Matrix *syz;
protected:

  virtual bool stop_conditions_ok() { return true; }
  // If the stop conditions in _Stop are inappropriate,
  // return false, and use ERROR(...) to provide an error message.


public:
  GBDeclared(const Matrix *m0,
	     const Matrix *gb,
	     const Matrix *change,
	     const Matrix *syz0);

  static GBComputation *create(const Matrix *m,
			       const Matrix *gb,
			       const Matrix *change,
			       const Matrix *syz);
  // Possibly returns NULL, if an error message is reported

  virtual ~GBDeclared() {}

  virtual void remove_gb() {}

  virtual GBComputation * cast_to_GBComputation() { return this;} 

  virtual void start_computation() {  }

  virtual int complete_thru_degree() const { return G->complete_thru_degree(); }
  // The computation is complete up through this degree.

  // Recall that the status of the computation is maintained by the Computation class,

  ////////////////////////////////
  // Results of the computation //
  ////////////////////////////////
  virtual const MatrixOrNull *get_gb() { return G->get_gb(); }

  virtual const MatrixOrNull *get_mingens() { return trimmed_gens; }

  virtual const MatrixOrNull *get_change() { return G->get_change(); }

  virtual const MatrixOrNull *get_syzygies() { return syz; }

  virtual const MatrixOrNull *get_initial(int nparts) { return G->get_initial(nparts); }

  ////////////////////////////////
  // Normal forms and lifting ////
  ////////////////////////////////

  virtual const MatrixOrNull *matrix_remainder(const Matrix *m) {
    return G->matrix_remainder(m);
  }

  virtual void matrix_lift(const Matrix *m,
			   MatrixOrNull **result_remainder,
			   MatrixOrNull **result_quotient) {
    return G->matrix_lift(m,result_remainder,result_quotient);
  }


  virtual int contains(const Matrix *m) { return G->contains(m); }

  //////////////////////////////////////
  // Statistics and spair information //
  //////////////////////////////////////

  virtual void text_out(buffer &o) { o << "declared GB"; }
  // This displays statistical information, and depends on the
  // gbTrace value.
};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
