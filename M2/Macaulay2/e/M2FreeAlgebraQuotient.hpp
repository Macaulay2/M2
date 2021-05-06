#ifndef _m2_free_algebra_quotient_hpp_
#define _m2_free_algebra_quotient_hpp_

#include "engine-includes.hpp"

#include <memory>

#include "M2FreeAlgebra.hpp"
#include "NCAlgebras/FreeAlgebraQuotient.hpp"
#include "NCAlgebras/FreeMonoid.hpp"
#include "Polynomial.hpp"
#include "ringelem.hpp"

class FreeAlgebra;
class Matrix;
class Monoid;
class Ring;
class RingMap;
class SumCollector;
class buffer;

using ExponentVector = int*;

class M2FreeAlgebraQuotient : public M2FreeAlgebraOrQuotient
{
private:
  const M2FreeAlgebra& mM2FreeAlgebra;
  const std::unique_ptr<FreeAlgebraQuotient> mFreeAlgebraQuotient;
  
  M2FreeAlgebraQuotient(
                        const M2FreeAlgebra& F,
                        std::unique_ptr<FreeAlgebraQuotient> A
                        );
public:
  static M2FreeAlgebraQuotient* create(const M2FreeAlgebra& F,
                                       const Matrix* GB,
                                       int maxdeg // -1 means complete GB, otherwise mx deg GB computed to
                                       );

  const M2FreeAlgebra& m2FreeAlgebra() const { return mM2FreeAlgebra; }
  const FreeAlgebraQuotient& freeAlgebraQuotient() const { return *mFreeAlgebraQuotient; }
  const FreeMonoid& monoid() const { return m2FreeAlgebra().monoid(); }
  const FreeAlgebra& freeAlgebra() const { return m2FreeAlgebra().freeAlgebra(); }
  
  const Monoid& degreeMonoid() const { return m2FreeAlgebra().monoid().degreeMonoid(); }
  const Ring* coefficientRing() const { return m2FreeAlgebra().coefficientRing(); }
  
  int numVars() const { return monoid().numVars(); }
  int n_vars() const { return numVars(); }
  
  // these are all the functions from Ring that must exist for M2FreeAlgebraQuotient to be instantiated
  virtual int index_of_var(const ring_elem a) const;
  
  virtual void text_out(buffer &o) const;
  virtual unsigned int computeHashValue(const ring_elem a) const;
  virtual ring_elem from_coefficient(const ring_elem a) const;
  virtual ring_elem from_long(long n) const;
  virtual ring_elem from_int(mpz_srcptr n) const;
  virtual bool from_rational(const mpq_srcptr q, ring_elem &result) const;

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

  virtual ring_elem power(const ring_elem f, mpz_srcptr n) const;
  virtual ring_elem power(const ring_elem f, int n) const;

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

  virtual engine_RawArrayPairOrNull list_form(const Ring *coeffR,
                                              const ring_elem f) const;

  virtual bool is_homogeneous(const ring_elem f) const;
  virtual void degree(const ring_elem f, int *d) const;
  virtual bool multi_degree(const ring_elem f, int *d) const;

  virtual SumCollector *make_SumCollector() const;

  long n_terms(const ring_elem f) const;

  bool is_homogeneous(const Poly* f) const;

  // returns true if f is homogeneous, and sets already_allocated_degree_vector
  // to be the LCM of the exponent vectors of the degrees of all terms in f.
  virtual bool multi_degree(const Poly* f, int *already_allocated_degree_vector) const;
  
  // lead coefficient, monomials and terms.
  ring_elem lead_coefficient(const Ring* coeffRing, const Poly* f) const;
  ring_elem lead_coefficient(const Ring* coeffRing, const ring_elem f) const
  {
    return lead_coefficient(coeffRing, reinterpret_cast<const Poly*>(f.get_Poly()));
  }

  #if 0
  // lead_monomial: returns an allocated Monomial meant for the front end of M2.
  const int* lead_monomial(const Poly* f) const;
  const int* lead_monomial(const ring_elem f) const { return lead_monomial reinterpret_cast<const Poly*>((f.get_Poly())); }
  #endif
  
  // lead terms, or get contiguous terms
  Poly* get_terms(const Poly* f, int lo, int hi) const
  {
    return m2FreeAlgebra().get_terms(f, lo, hi);
  }
  ring_elem get_terms(const ring_elem f, int lo, int hi) const
  {
    return m2FreeAlgebra().get_terms(f, lo, hi);
  }

  // casting functions
  virtual const M2FreeAlgebraQuotient * cast_to_M2FreeAlgebraQuotient()  const { return this; }
  virtual       M2FreeAlgebraQuotient * cast_to_M2FreeAlgebraQuotient()        { return this; }

  void debug_display(const Poly* f) const;
  void debug_display(const ring_elem ff) const;

  void makeTerm(Poly& result, const ring_elem a, const int* monom) const;
  
  ring_elem makeTerm(const ring_elem a, const int* monom) const;
  // 'monom' is in 'varpower' format
  // [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
