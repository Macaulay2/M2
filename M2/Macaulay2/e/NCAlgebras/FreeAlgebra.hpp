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

  const Ring* coefficientRing() const { return &mCoefficientRing; }
  const FreeMonoid& monoid() const { return mMonoid; }
  const Monoid& degreeMonoid() const { return monoid().degreeMonoid(); }
  int numVars() const { return monoid().numVars(); }
  
  unsigned int computeHashValue(const Poly& a) const; // TODO

  void init(Poly& f) const {}
  void clear(Poly& f) const; // TODO

  void from_coefficient(Poly& result, const ring_elem a) const; // TODO
  void from_long(Poly& result, long n) const; // TODO
  void from_int(Poly& result, mpz_srcptr n) const; // TODO
  void copy(Poly& result, const Poly& f) const; // TODO
  bool from_rational(Poly& result, const mpq_ptr q) const; // TODO
  void var(Poly& result, int v) const;

  long n_terms(const Poly& f) const;  // TODO
  bool is_unit(const Poly& f) const;  // TODO
  bool is_zero(const Poly& f) const;  // TODO
  bool is_equal(const Poly& f, const Poly& g) const;
  int compare_elems(const Poly& f, const Poly& g) const; // TODO
  
  void negate(Poly& result, const Poly& f) const; // TODO
  void add(Poly& result, const Poly& f, const Poly& g) const;
  void subtract(Poly& result, const Poly& f, const Poly& g) const; // TODO
  void mult(Poly& result, const Poly& f, const Poly& g) const; // TODO
  void power(Poly& result, const Poly& f, int n) const; // TODO
  void power(Poly& result, const Poly& f, mpz_srcptr n) const; // TODO

  void elem_text_out(buffer &o,
                     const Poly& f,
                     bool p_one,
                     bool p_plus,
                     bool p_parens) const; // TODO

  bool is_homogeneous(const Poly& f) const; // TODO
  void degree(const Poly& f, int *d) const; // TODO
  // returns true if f is homogeneous, and sets already_allocated_degree_vector
  // to be the LCM of the exponent vectors of the degrees of all terms in f.
  bool multi_degree(const Poly& f, int *already_allocated_degree_vector) const; // TODO

  void mult_by_term_right(Poly& result, const Poly& f, const ring_elem c, const Monom) const; // TODO
  void mult_by_term_left(Poly& result, const Poly& f, const ring_elem c, const Monom) const; // TODO
  void mult_by_term_left_and_right(Poly& result, const Poly& f, const ring_elem c, const Monom, const Monom) const; // TODO
  void add_to_end(Poly& f, const Poly& g) const; // TODO
  
#if 0  
  Poly* eval(const RingMap *map, const Poly* f, int first_var) const;

  int index_of_var(const Poly& a) const;
  
  void text_out(buffer &o) const; // need?

  // if not lift/promoteable, return nullptr
  Poly* promote(const Ring *R, const ring_elem f) const; 
  Poly* lift(const Ring *R, const ring_elem f) const;


  engine_RawArrayPairOrNull list_form(const Ring *coeffR,
                                      const Poly* f) const;


  SumCollector *make_SumCollector() const;

  // lead coefficient, monomials and terms.
  ring_elem lead_coefficient(const Ring* coeffRing, const Poly* f) const;

  // lead terms, or get contiguous terms
  Poly* get_terms(const Poly* f, int lo, int hi) const;

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

