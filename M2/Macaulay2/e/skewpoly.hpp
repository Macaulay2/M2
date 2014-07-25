// Copyright 2003 Michael E. Stillman

#ifndef _skewpoly_hh_
#define _skewpoly_hh_

#include "poly.hpp"

///// Ring Hierarchy ///////////////////////////////////

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

  virtual bool has_gcd() const      { return false; }
  virtual bool is_skew_commutative_ring() const { return true; }
  virtual bool is_commutative_ring() const { return false; }

  virtual const SkewPolynomialRing * cast_to_SkewPolynomialRing()  const      { return this; }
  virtual       SkewPolynomialRing * cast_to_SkewPolynomialRing()             { return this; }

  virtual ring_elem power(const ring_elem f, mpz_t n) const;
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
