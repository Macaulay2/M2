// Copyright 2003 Michael E. Stillman

#ifndef _skewpoly_hh_
#define _skewpoly_hh_

#include "skew.hpp"
#include "polyring.hpp"

///// Ring Hierarchy ///////////////////////////////////

class SkewPolynomialRing : public PolynomialRing
{
protected:
  bool initialize_skew(M2_arrayint skewvars);
  SkewPolynomialRing() {}
  virtual ~SkewPolynomialRing();

public:
  static SkewPolynomialRing *create(const Ring *R,
				    M2_arrayint skewvars);

  virtual bool is_pid() const       { return false; }
  virtual bool has_gcd() const      { return false; }
  virtual bool is_graded() const    { return _isgraded; }
  virtual bool is_expensive() const { return true; }

  virtual bool is_skew_commutative_ring() const { return true; }
  virtual bool is_commutative_ring() const { return false; }

  virtual const SkewPolynomialRing * cast_to_SkewPolynomialRing()  const      { return this; }
  virtual       SkewPolynomialRing * cast_to_SkewPolynomialRing()             { return this; }

  virtual ring_elem power(const ring_elem f, mpz_t n) const;
  virtual ring_elem power(const ring_elem f, int n) const;

public:
  virtual ring_elem imp_mult_by_term(const ring_elem f, 
				     const ring_elem c, 
				     const int *m) const;
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
