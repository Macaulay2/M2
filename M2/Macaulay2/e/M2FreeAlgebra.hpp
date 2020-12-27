#ifndef _m2_free_algebra_hpp_
#define _m2_free_algebra_hpp_

#include <M2/math-include.h>
#include "engine-includes.hpp"

#include <memory>
#include <string>
#include <vector>

#include "NCAlgebras/FreeAlgebra.hpp"
#include "NCAlgebras/FreeMonoid.hpp"
#include "Polynomial.hpp"
#include "ring.hpp"
#include "ringelem.hpp"

class PolynomialRing;
class RingMap;
class buffer;
struct Monoid;

using ExponentVector = int*;

//struct CoefficientRingTypeExample
//{
//  typedef ring_elem ElementType;
//};

class M2FreeAlgebraOrQuotient : public Ring
{
public:
  const Poly* toPoly(const ring_elem f) const { return reinterpret_cast<const Poly*>(f.get_Poly()); }

  ring_elem fromPoly(Poly* f) const { return reinterpret_cast<Nterm*>(f); }

  void appendFromModuleMonom(Poly& f, const ModuleMonom& m) const;

  ring_elem fromModuleMonom(const ModuleMonom& m) const;

public:  
  virtual const FreeAlgebra& freeAlgebra() const = 0;

  virtual int n_vars() const = 0;

  virtual const Ring* coefficientRing() const = 0;

  virtual ring_elem from_coefficient(const ring_elem a) const = 0;

  virtual ring_elem makeTerm(const ring_elem a, const int* monom) const = 0;
};

class M2FreeAlgebra : public M2FreeAlgebraOrQuotient
{
private:
  const std::unique_ptr<FreeAlgebra> mFreeAlgebra;

  M2FreeAlgebra(std::unique_ptr<FreeAlgebra> F);

public:
  static M2FreeAlgebra* create(const Ring* K,
                               const std::vector<std::string>& names,
                               const PolynomialRing* degreeRing,
                               const std::vector<int>& degrees,
                               const std::vector<int>& wtvecs,
                               const std::vector<int>& heftVector
                               );

  const FreeAlgebra& freeAlgebra() const { return *mFreeAlgebra; }
  const FreeMonoid& monoid() const { return freeAlgebra().monoid(); }
  const Monoid& degreeMonoid() const { return monoid().degreeMonoid(); }

  const PolynomialRing* degreeRing() const { return monoid().degreeRing(); }
  const Ring* coefficientRing() const { return freeAlgebra().coefficientRing(); }
  
  int numVars() const { return monoid().numVars(); }
  virtual int n_vars() const { return numVars(); }
  
  // these are all the functions from Ring that must exist for M2FreeAlgebra to be instantiated
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
  Poly* get_terms(const Poly* f, int lo, int hi) const;
  ring_elem get_terms(const ring_elem f, int lo, int hi) const
  {
    const Poly* result = get_terms(reinterpret_cast<const Poly*>(f.get_Poly()), lo, hi);
    return ring_elem(reinterpret_cast<const Poly*>(result));
  }

  // support functions
  virtual M2_arrayint support(const ring_elem a) const;

  // casting functions
  virtual const M2FreeAlgebra * cast_to_M2FreeAlgebra()  const { return this; }
  virtual       M2FreeAlgebra * cast_to_M2FreeAlgebra()        { return this; }

  void debug_display(const Poly* f) const;
  void debug_display(const ring_elem ff) const;

  ring_elem makeTerm(const ring_elem a, const int* monom) const;

  void makeTerm(Poly& result, const ring_elem a, const int* monom) const;
  // 'monom' is in 'varpower' format
  // [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
};

PolyList copyPolyVector(const M2FreeAlgebraOrQuotient* A,
                        const ConstPolyList& polys);
  
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
