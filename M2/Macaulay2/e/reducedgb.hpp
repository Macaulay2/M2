// Copyright 2004 Michael E. Stillman.

#ifndef _reducedgb_hpp_
#define _reducedgb_hpp_

/**
 * @file reducedgb.hpp
 * @brief `ReducedGB` --- abstract base for the canonicalising reduction pass that follows GB computation.
 *
 * Declares `ReducedGB`, a `GBComputation` subclass that takes
 * an already-computed Groebner basis and canonicalises it:
 * leading monomials become distinct, no tail term is divisible
 * by another element's leading monomial, and (over a field)
 * leading coefficients are normalised to 1. The base owns the
 * `GBRing *R`, `originalR`, source / target `FreeModule*`s,
 * and a `VECTOR(POLY) polys` of the current basis; the leading
 * monomial index (`MonomialTable` or `MonomialTableZZ` from
 * `montable.hpp` / `montableZZ.hpp`) is added by the
 * appropriate subclass. The static `ReducedGB::create` factory
 * picks the right subclass from the coefficient ring and call
 * shape.
 *
 * Concrete subclasses cover the cases that diverge: `ReducedGB_Field`
 * (`reducedgb-field.hpp`) enforces the field convention,
 * `ReducedGB_Field_Local` (`reducedgb-field-local.hpp`, which
 * extends `ReducedGB_Field` rather than `ReducedGB` directly)
 * adds the unit-of-the-maximal-ideal check required over local
 * rings, `ReducedGB_ZZ` (`reducedgb-ZZ.hpp`) handles
 * coefficient-aware reduction (multiple basis elements with the
 * same monomial support are allowed if they differ in
 * coefficient), and `MarkedGB` (`reducedgb-marked.hpp`) takes a
 * user-supplied `(leadterms, basis)` pair, accepts the marked
 * leading terms as-is, and runs `auto_reduce()` over the tails
 * (so no Buchberger loop runs but the polynomials themselves
 * are still reduced against each other's marked leads).
 *
 * @see comp-gb.hpp
 * @see gbring.hpp
 * @see montable.hpp
 * @see montableZZ.hpp
 */

#include "comp-gb.hpp"
#include <vector>
#include "gbring.hpp"
#include "montable.hpp"
#include "montableZZ.hpp"
#include "gbweight.hpp"
#include "polyring.hpp"

/**
    @ingroup reducedgb

    @brief Base class for reduced Groebner basis computation.

    These classes take a Groebner basis, and compute a corresponding minimal
   Groebner basis.
*/
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
                           const GBWeight *wt0 = nullptr);

  virtual GBComputation *cast_to_GBComputation() { return this; }
  virtual void start_computation() {}
  virtual int complete_thru_degree() const { return 0; }
  // The computation is complete up through this degree.

  // Recall that the status of the computation is maintained by the Computation
  // class,

  virtual const Ring *get_ring() const { return originalR; }
  ////////////////////////////////
  // Results of the computation //
  ////////////////////////////////
  virtual const Matrix /* or null */ *get_gb();

  virtual const Matrix /* or null */ *get_mingens();

  virtual const Matrix /* or null */ *get_change();

  virtual const Matrix /* or null */ *get_syzygies();

  virtual const Matrix /* or null */ *get_initial(int nparts);

  virtual const Matrix /* or null */ *get_parallel_lead_terms(M2_arrayint w);

  //////////////////////////////////////
  // Statistics and spair information //
  //////////////////////////////////////

  virtual void text_out(buffer &o) const;
  // This displays statistical information, and depends on the
  // M2_gbTrace value.

  ////////////////////////////////
  // Normal forms and lifting ////
  ////////////////////////////////

  virtual const Matrix /* or null */ *matrix_remainder(const Matrix *m);

  virtual M2_bool matrix_lift(const Matrix *m,
                              const Matrix /* or null */ **result_remainder,
                              const Matrix /* or null */ **result_quotient);

  virtual int contains(const Matrix *m);

  ////////////////////////////////////////////////
  // The following are the functions which need //
  // to be provided by subclasses               //
  ////////////////////////////////////////////////

  virtual void set_gb(VECTOR(POLY) & polys0) = 0;

  virtual void minimalize(const VECTOR(POLY) & polys0, bool auto_reduce = true)
  {
    (void) polys0;
    (void) auto_reduce;
  }

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

  //  const VECTOR(POLY) &get() const { return polys; }

  virtual void remainder(POLY &f, bool use_denom, ring_elem &denom) = 0;
  // WARNING: this should only be used with term orders!
  // REALLY??

  virtual void remainder(gbvector *&f, bool use_denom, ring_elem &denom) = 0;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
