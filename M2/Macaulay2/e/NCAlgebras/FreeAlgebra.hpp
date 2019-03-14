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

  // FM: Working on converting these functions from PolynomialAlgebra
  //     over to FreeAlgebra.  Do these look right?

  int numVars() const { return monoid().numVars(); }
  
  int index_of_var(const Poly* a) const;
  
  void text_out(buffer &o) const; // need?
  unsigned int computeHashValue(const Poly* a) const;
  Poly* from_coefficient(const ring_elem a) const;
  Poly* from_long(long n) const;
  Poly* from_int(mpz_srcptr n) const;

  // if fails, then return nullptr
  Poly* from_rational(const mpq_ptr q) const;  

  Poly* var(int v) const;

  // if not lift/promoteable, return nullptr
  Poly* promote(const Ring *R, const ring_elem f) const; 
  Poly* lift(const Ring *R, const ring_elem f) const;

  bool is_unit(const Poly* f) const;
  bool is_zero(const Poly* f) const;
  bool is_equal(const Poly* f, const Poly* g) const;
  int compare_elems(const Poly* f, const Poly* g) const;
  Poly* copy(const Poly* f) const;
  
  void remove(Poly*& f) const;
  
  // copy negate 
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
  ring_elem lead_coefficient(const Ring* coeffRing, const Poly* f) const;

  // lead terms, or get contiguous terms
  Poly* get_terms(const Poly* f, int lo, int hi) const;

  // some internal functions for the above routines
  Poly* mult_by_term_right(const Poly* f, const ring_elem c, const Monom) const;
  Poly* mult_by_term_left(const Poly* f, const ring_elem c, const Monom) const;
  Poly* mult_by_term_left_and_right(const Poly* f, const ring_elem c, const Monom, const Monom) const;
  void add_to_end(Poly* f, const Poly* g) const;

  void debug_display(const Poly* f) const;

  Poly* makeTerm(const ring_elem a, const int* monom) const;
  // 'monom' is in 'varpower' format (i.e. from the front end)
  // [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)

  void setZero(Poly& f) const // resets f to zero
  {
    for (auto a : f.mCoefficients)
      mCoefficientRing.remove(a);
    
    f.mCoefficients.clear();
    f.mMonomials.clear();
  }
  
  Poly addPolys(const Poly& f, const Poly& g) const;

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

