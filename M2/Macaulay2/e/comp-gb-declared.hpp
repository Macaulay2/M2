// Copyright 2004 Michael E. Stillman.

#ifndef _comp_gb_declared_hpp_
#define _comp_gb_declared_hpp_

/**
 * @file comp-gb-declared.hpp
 * @brief `GBDeclared` --- a user-asserted Groebner basis the engine accepts without computing.
 *
 * `GBDeclared` is the `GBComputation` subclass produced by M2's
 * `forceGB`. Two constructors (and matching static `create`
 * factories) cover the two declaration flavours: `(m0, gb,
 * change, syz0)` takes the original generators `m0`, a claimed
 * reduced GB `gb`, the change-of-basis matrix expressing `gb` in
 * terms of `m0`, and a claimed syzygy module `syz0`; the
 * `(leadterms, m0, gb, change, syz0)` variant additionally lets
 * the caller declare the leading-term matrix explicitly rather
 * than letting the engine derive it from `gb`. The engine
 * stores the inputs and treats `gb` as the GB without
 * verification --- a wrong declaration produces wrong downstream
 * answers; the point of `forceGB` is exactly to skip the check.
 * Every subsequent `%`, `//`, or `quotient` runs against the
 * declared basis at the speed of an already-computed GB.
 *
 * Typical uses are GBs imported from external solvers (msolve,
 * OSCAR, Singular) when the user does not want to recompute,
 * theoretically known bases where the proof is on paper, and
 * caching long GB runs to disk for later reload. The internal
 * `ReducedGB* G` keeps the trusted basis in the engine's standard
 * reduced-GB form and most accessors (`get_gb`, `get_change`,
 * `get_initial`, `matrix_remainder`, `matrix_lift`, ...) forward
 * straight to `G`; `get_mingens` returns the declared
 * `trimmed_gens` and `get_syzygies` returns the declared `syz`.
 * `start_computation` is a no-op since there is nothing to
 * compute.
 *
 * @see comp-gb.hpp
 * @see comp-gb-proxy.hpp
 * @see reducedgb.hpp
 */

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
