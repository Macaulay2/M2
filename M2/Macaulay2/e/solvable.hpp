// Copyright 2003 Michael E. Stillman

#ifndef _solvable_hh_
#define _solvable_hh_

/**
 * @file solvable.hpp
 * @brief `SolvableAlgebra` --- scaffolding for a PBW algebra `x_j x_i = x_i x_j + q_{ij}` (multiplication unimplemented).
 *
 * Declares `SolvableAlgebra`, the `PolyRing` subclass intended
 * to model polynomial-shaped algebras whose generators do not
 * commute but satisfy a PBW-style straightening relation
 * `x_j x_i = x_i x_j + q_{ij}` for `i < j`, with each
 * commutator polynomial `q_{ij}` supplied as the `(i, j)` entry
 * of a constructor-provided `Matrix Q_`. The intended framework
 * covers Weyl algebras, quantum planes, universal enveloping
 * algebras `U(g)`, and the trivial zero-`Q` case where the
 * algebra degenerates to an ordinary polynomial ring.
 *
 * The class is currently a stub: `initialize_solvable(Q)` just
 * stashes the matrix and `SolvableAlgebra::mult_by_term`
 * (`solvable.cpp`) returns `ZERO_RINGELEM` behind a
 * "implement SolvableAlgebra::mult_by_term" `#warning`, so any
 * non-trivial product collapses to zero. `power` defers to
 * `Ring::power` and therefore inherits the same problem. The
 * public surface is in place (`create` / `cast_to_SolvableAlgebra`
 * / `is_solvable_algebra` / `mult_by_term`), so callers can
 * already plumb through the type, but the PBW reduction itself
 * has yet to be filled in.
 *
 * @see poly.hpp
 * @see weylalg.hpp
 * @see skewpoly.hpp
 */

#include "poly.hpp"

///// Ring Hierarchy ///////////////////////////////////

/**
 * @brief `PolyRing` subclass for solvable polynomial algebras (PBW-type
 * non-commutative rings where each pair of non-commuting variables
 * satisfies a relation of the form `x_j x_i = c_ij x_i x_j + lower`).
 *
 * @details `Q_` is the matrix of `lower` correction terms indexed by
 * variable pairs --- when `mult_by_term1` would normally commute
 * `x_j` past `x_i`, the engine looks up `Q_` to add in the
 * lower-order polynomial that the relation demands. Constructed via
 * `create(R, Q)` so the relations can be validated against the
 * ambient `PolynomialRing` before the new ring is exposed.
 */
class SolvableAlgebra : public PolyRing
{
  const Matrix *Q_;

  static SolvableAlgebra *create(const Ring *K,
                                 const Monoid *M,
                                 const Matrix *Q);

 protected:
  bool initialize_solvable(const Matrix *Q);
  SolvableAlgebra() {}
  virtual ~SolvableAlgebra();

 public:
  static SolvableAlgebra *create(const PolynomialRing *R, const Matrix *Q);

  virtual bool is_commutative_ring() const { return false; }
  virtual bool is_solvable_algebra() const { return true; }
  virtual const SolvableAlgebra *cast_to_SolvableAlgebra() const
  {
    return this;
  }
  virtual SolvableAlgebra *cast_to_SolvableAlgebra() { return this; }
  virtual ring_elem power(const ring_elem f, mpz_srcptr n) const;
  virtual ring_elem power(const ring_elem f, int n) const;

 public:
  virtual ring_elem mult_by_term(const ring_elem f,
                                 const ring_elem c,
                                 const int *m) const;
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
