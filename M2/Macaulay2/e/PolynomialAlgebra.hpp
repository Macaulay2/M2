#ifndef _polynomial_algebra_hpp_
#define _polynomial_algebra_hpp_

#include "Polynomial.hpp"

struct CoefficientRingTypeExample
{
  typedef ring_elem ElementType;
};

class PolynomialAlgebra : public Ring
{
  PolynomialAlgebra(const Ring* K,
                M2_ArrayString names);
public:
  typedef Polynomial<CoefficientRingTypeExample> Poly;
  
  static PolynomialAlgebra* create(const Ring* K,
                               M2_ArrayString names,
                               const PolynomialRing* degreeRing);

  // these are all the functions from Ring that must exist for PolynomialAlgebra to be instantiated
  virtual int index_of_var(const ring_elem a) const;
  
  virtual void text_out(buffer &o) const;
  virtual unsigned int computeHashValue(const ring_elem a) const;
  virtual ring_elem from_long(long n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem from_rational(mpq_ptr q) const;
  virtual ring_elem var(int v) const;
  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;
  virtual int compare_elems(const ring_elem f, const ring_elem g) const;
  virtual ring_elem copy(const ring_elem f) const;
  virtual void remove(ring_elem &f) const;
  virtual ring_elem negate(const ring_elem f) const;
  virtual ring_elem add(const ring_elem f, const ring_elem g) const;
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const;
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const;
  virtual ring_elem invert(const ring_elem f) const;
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const;
  virtual void syzygy(const ring_elem a, const ring_elem b,
                      ring_elem &x, ring_elem &y) const;
  virtual void elem_text_out(buffer &o,
                             const ring_elem f,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const;
  virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const;
  
  // some internal functions for the above routines
  ring_elem mult_by_term_right(const ring_elem f, const ring_elem c, const Monom) const;
  ring_elem mult_by_term_left(const ring_elem f, const ring_elem c, const Monom) const;
  void add_to_end(ring_elem f, const ring_elem g) const;
  int compare_monoms(const Monom m1, const Monom m2) const;

  // casting functions
  virtual const PolynomialAlgebra * cast_to_PolynomialAlgebra()  const { return this; }
  virtual       PolynomialAlgebra * cast_to_PolynomialAlgebra()        { return this; }

  void debug_display(const Poly* f) const;
  void debug_display(const ring_elem ff) const;
private:
  std::vector<std::string> mVariableNames;
  const Ring& mCoefficientRing;
  unsigned int mNumVars;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
