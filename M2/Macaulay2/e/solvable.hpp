// Copyright 2003 Michael E. Stillman

#ifndef _solvable_hh_
#define _solvable_hh_

#include "polyring.hpp"

///// Ring Hierarchy ///////////////////////////////////

class SolvableAlgebra : public PolynomialRing
{
protected:
  bool initialize_solvable(const Matrix *Q);
  SolvableAlgebra() {}
  virtual ~SolvableAlgebra();

public:
  static SolvableAlgebra *create(const Ring *R,
				 const Matrix *Q);

  virtual bool is_pid() const       { return false; }
  virtual bool has_gcd() const      { return false; }
  virtual bool is_graded() const    { return _isgraded; }
  virtual bool is_expensive() const { return true; }

  virtual bool is_commutative_ring() const { return false; }
  virtual bool is_solvable_algebra() const { return true; }

  virtual const SolvableAlgebra * cast_to_SolvableAlgebra()  const      { return this; }
  virtual       SolvableAlgebra * cast_to_SolvableAlgebra()             { return this; }

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
