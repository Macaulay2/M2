#ifndef _free_algebra_hpp_
#define _free_algebra_hpp_

#include "../polyring.hpp"
#include "../Polynomial.hpp"
#include "FreeMonoid.hpp"

struct CoefficientRingType
{
  typedef ring_elem ElementType;
};

using Poly = Polynomial<CoefficientRingType>;

class FreeAlgebra
{
private:
  FreeAlgebra(const Ring* K, const FreeMonoid* M);

public:
  static FreeAlgebra* create(const Ring* K,
                                   const std::vector<std::string>& names,
                                   const PolynomialRing* degreeRing,
                                   const std::vector<int>& degrees
                                   );

  const Ring* getCoefficientRing() const { return &mCoefficientRing; }
  const FreeMonoid& monoid() const { return mMonoid; }
  const Monoid& degreeMonoid() const { return monoid().degreeMonoid(); }

#if 0
  int numVars() const { return monoid().numVars(); }
  int n_vars() const { return numVars(); }
  
  // FM: Working on converting these functions from PolynomialAlgebra
  //     over to FreeAlgebra.  Do these look right?

  int index_of_var(const Poly* a) const;
  
  void text_out(buffer &o) const; // need?
  unsigned int computeHashValue(const Poly* a) const;
  Poly* from_coefficient(const ring_elem a) const;
  Poly* ring_elem from_long(long n) const;
  Poly* ring_elem from_int(mpz_srcptr n) const;
  // this was ring_elem& result.  Not sure whether we want Poly* or Poly&
  // the same goes for several items below.
  bool from_rational(const mpq_ptr q, Poly* result) const;  

  Poly* var(int v) const;
  bool promote(const Ring *R, const ring_elem f, Poly* result) const; // ring_elem&
  bool lift(const Ring *R, const ring_elem f, Poly* result) const;
  bool is_unit(const Poly* f) const;
  bool is_zero(const Poly* f) const;
  bool is_equal(const Poly* f, const Poly* g) const;
  int compare_elems(const Poly* f, const Poly* g) const;
  Poly* copy(const Poly* f) const;
  void remove(ring_elem &f) const; // ring_elem&
  Poly* negate(const Poly* f) const;
  Poly* add(const Poly* f, const Poly* g) const;
  Poly* subtract(const Poly* f, const Poly* g) const;
  Poly* mult(const Poly* f, const Poly* g) const;

  Poly* power(const Poly* f, mpz_t n) const;
  Poly* power(const Poly* f, int n) const;

  Poly* invert(const Poly* f) const;
  Poly* divide(const Poly* f, const Poly* g) const;
  void elem_text_out(buffer &o,
                     const Poly* f,
                     bool p_one,
                     bool p_plus,
                     bool p_parens) const;
  Poly* eval(const RingMap *map, const Poly* f, int first_var) const;

  engine_RawArrayPairOrNull list_form(const Ring *coeffR,
                                      const Poly* f) const;

  bool is_homogeneous(const Poly* f) const;
  void degree(const Poly* f, int *d) const;
  bool multi_degree(const Poly* f, int *d) const;

  SumCollector *make_SumCollector() const;

  long n_terms(const Poly* f) const;

  bool is_homogeneous(const Poly* f) const;

  // returns true if f is homogeneous, and sets already_allocated_degree_vector
  // to be the LCM of the exponent vectors of the degrees of all terms in f.
  bool multi_degree(const Poly* f, int *already_allocated_degree_vector) const;
  
  // lead coefficient, monomials and terms.
  Poly* lead_coefficient(const Ring* coeffRing, const Poly* f) const;

  // lead terms, or get contiguous terms
  Poly* get_terms(const Poly* f, int lo, int hi) const;

  // some internal functions for the above routines
  Poly* mult_by_term_right(const Poly* f, const Poly* c, const Monom) const;
  Poly* mult_by_term_left(const Poly* f, const Poly* c, const Monom) const;
  Poly* mult_by_term_left_and_right(const Poly* f, const Poly* c, const Monom, const Monom) const;
  void add_to_end(Poly* f, const Poly* g) const;

  void debug_display(const Poly* f) const;

  Poly* makeTerm(const Poly* a, const int* monom) const;
  // 'monom' is in 'varpower' format
  // [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)

  void setZero(Poly& f) const // resets f to zero
  {
    for (auto a : f.mCoefficients)
      mCoefficientRing.remove(a);
    
    f.mCoefficients.clear();
    f.mMonomials.clear();
  }
  
  Poly addPolys(const Poly& f, const Poly& g) const;

  const Poly* toPoly(const ring_elem f) const { return reinterpret_cast<const Poly*>(f.mPolyVal); }

  ring_elem fromPoly(Poly* f) const { return reinterpret_cast<Nterm*>(f); }  // is Nterm really what we want here?

#endif

private:
  const Ring& mCoefficientRing;
  const FreeMonoid mMonoid;

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

