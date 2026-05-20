// Copyright 2004 Michael E. Stillman.

#ifndef _comp_gb_declared_hpp_
#define _comp_gb_declared_hpp_

#include "comp-gb.hpp"
#include "reducedgb.hpp"

/**
    @ingroup gb

    @brief declared Groebner bases.
*/
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

  GBDeclared(const Matrix *leadterms,
             const Matrix *m0,
             const Matrix *gb,
             const Matrix *change,
             const Matrix *syz0);

  static GBComputation *create(const Matrix *m,
                               const Matrix *gb,
                               const Matrix *change,
                               const Matrix *syz);
  // Possibly returns NULL, if an error message is reported

  static GBComputation *create(const Matrix *leadterms,
                               const Matrix *m,
                               const Matrix *gb,
                               const Matrix *change,
                               const Matrix *syz);
  // Possibly returns NULL, if an error message is reported

  virtual ~GBDeclared() {}
  virtual void remove_gb() {}
  virtual GBComputation *cast_to_GBComputation() { return this; }
  virtual void start_computation() {}
  virtual int complete_thru_degree() const { return G->complete_thru_degree(); }
  // The computation is complete up through this degree.

  // Recall that the status of the computation is maintained by the Computation
  // class,

  virtual const Ring *get_ring() const { return G->get_ring(); }
  ////////////////////////////////
  // Results of the computation //
  ////////////////////////////////
  virtual const Matrix /* or null */ *get_gb() { return G->get_gb(); }
  virtual const Matrix /* or null */ *get_mingens() { return trimmed_gens; }
  virtual const Matrix /* or null */ *get_change() { return G->get_change(); }
  virtual const Matrix /* or null */ *get_syzygies() { return syz; }
  virtual const Matrix /* or null */ *get_initial(int nparts)
  {
    return G->get_initial(nparts);
  }

  virtual const Matrix /* or null */ *get_parallel_lead_terms(M2_arrayint w)
  {
    return G->get_parallel_lead_terms(w);
  }

  ////////////////////////////////
  // Normal forms and lifting ////
  ////////////////////////////////

  virtual const Matrix /* or null */ *matrix_remainder(const Matrix *m)
  {
    return G->matrix_remainder(m);
  }

  virtual M2_bool matrix_lift(const Matrix *m,
                              const Matrix /* or null */ **result_remainder,
                              const Matrix /* or null */ **result_quotient)
  {
    return G->matrix_lift(m, result_remainder, result_quotient);
  }

  virtual int contains(const Matrix *m) { return G->contains(m); }
  //////////////////////////////////////
  // Statistics and spair information //
  //////////////////////////////////////

  virtual void text_out(buffer &o) const { o << "declared GB"; }
  // This displays statistical information, and depends on the
  // M2_gbTrace value.
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
