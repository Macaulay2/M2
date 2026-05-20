// Copyright 2003 Michael E. Stillman

#ifndef _skewpoly_hh_
#define _skewpoly_hh_

/**
 * @file skewpoly.hpp
 * @brief `SkewPolynomialRing` --- polynomial ring with a designated set of anticommuting variables.
 *
 * Declares `SkewPolynomialRing`, the `PolyRing` subclass for
 * the super-commutative case: a set of variables passed to
 * `create(K, M, skewvars)` anticommutes pairwise and squares to
 * zero (`x_i x_j = -x_j x_i`, `x_i^2 = 0`), while every other
 * variable commutes with everything. Construction routes
 * through `initialize_skew(skewvars)` and delegates the
 * bookkeeping to the inherited `SkewMultiplication skew_` field
 * (declared in `skew.hpp`); arithmetic enters the skew code
 * path through the `is_skew_` flag inherited from
 * `PolynomialRing`. The overridden `mult_by_term` walks the
 * second factor and produces the sign by counting the
 * transpositions needed to move its skew variables past those
 * of the first.
 *
 * Reports `is_skew_commutative_ring()` true and
 * `is_commutative_ring()` false; `has_gcd()` is overridden to
 * false because the ring is not a domain (any skew generator
 * `x_i` is a zero divisor with itself). The `antipode(f)`
 * method implements the `R <-> R^op` isomorphism by twisting
 * each monomial of skew degree `d` by `(-1)^{d choose 2}`.
 * Exterior algebras are the familiar special case (every
 * variable skew); the broader setup with a strict subset is
 * what makes the class more general than a pure exterior
 * algebra.
 *
 * @see skew.hpp
 * @see poly.hpp
 * @see polyring.hpp
 */

#include "poly.hpp"

///// Ring Hierarchy ///////////////////////////////////

/**
 * @brief `PolyRing` subclass for skew-commutative (exterior-style)
 * polynomial rings: the listed `skewvars` anticommute among
 * themselves and square to zero.
 *
 * @details Constructed via `create(K, M, skewvars)` --- `skewvars` is the
 * array of variable indices that participate in the
 * anti-commutation. The data needed to apply the sign changes
 * lives in a `SkewMultiplication` instance attached to the
 * underlying ring; the `PolyRing` virtuals are overridden to
 * consult it during monomial multiplication.
 */
class SkewPolynomialRing : public PolyRing
{
  bool initialize_skew(M2_arrayint skewvars);

  SkewPolynomialRing() {}
  virtual ~SkewPolynomialRing();

 public:
  static SkewPolynomialRing *create(const Ring *K,
                                    const Monoid *M,
                                    M2_arrayint skewvars);

  void text_out(buffer &o) const;

  virtual bool has_gcd() const { return false; }
  virtual bool is_skew_commutative_ring() const { return true; }
  virtual bool is_commutative_ring() const { return false; }
  virtual const SkewPolynomialRing *cast_to_SkewPolynomialRing() const
  {
    return this;
  }
  virtual SkewPolynomialRing *cast_to_SkewPolynomialRing() { return this; }
  virtual ring_elem power(const ring_elem f, mpz_srcptr n) const;
  virtual ring_elem power(const ring_elem f, int n) const;

  // antipode: implements the isomorphism of R and the opposite of R
  // this is done by multiplying every monomial of f which has degree
  // d (in the skew commuting variables) by (-1)^(d choose 2).
  ring_elem antipode(const ring_elem f) const;

 protected:
  virtual ring_elem mult_by_term(const ring_elem f,
                                 const ring_elem c,
                                 const int *m) const;
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
