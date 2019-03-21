#ifndef _free_algebra_hpp_
#define _free_algebra_hpp_

#include "../polyring.hpp"
#include "../Polynomial.hpp"
#include "FreeMonoid.hpp"
#include "WordTable.hpp"

struct CoefficientRingType
{
  typedef ring_elem ElementType;
};

using Poly = Polynomial<CoefficientRingType>;
using PolyList = VECTOR(Poly*);
using ConstPolyList = VECTOR(const Poly*);

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
  void clear(Poly& f) const;
  void setZero(Poly& f) const;

  void from_coefficient(Poly& result, const ring_elem a) const;
  void from_long(Poly& result, long n) const;
  void from_int(Poly& result, mpz_srcptr n) const; 
  bool from_rational(Poly& result, const mpq_ptr q) const; 
  void copy(Poly& result, const Poly& f) const;
  void swap(Poly& f, Poly& g) const; // TODO
  void var(Poly& result, int v) const;
  void from_word(Poly& result, const std::vector<int>& word) const; 
  void from_word(Poly& result, ring_elem coeff, const std::vector<int>& word) const; 
  
  long n_terms(const Poly& f) const { return f.numTerms(); }  
  bool is_unit(const Poly& f) const;
  bool is_zero(const Poly& f) const { return n_terms(f) == 0; }
  bool is_equal(const Poly& f, const Poly& g) const;
  int compare_elems(const Poly& f, const Poly& g) const;
  
  void negate(Poly& result, const Poly& f) const; 
  void add(Poly& result, const Poly& f, const Poly& g) const;
  void subtract(Poly& result, const Poly& f, const Poly& g) const;
  void mult(Poly& result, const Poly& f, const Poly& g) const;
  void power(Poly& result, const Poly& f, int n) const;
  void power(Poly& result, const Poly& f, mpz_ptr n) const;

  void elem_text_out(buffer &o,
                     const Poly& f,
                     bool p_one,
                     bool p_plus,
                     bool p_parens) const;

  bool is_homogeneous(const Poly& f) const;
  void degree(const Poly& f, int *d) const;
  // returns true if f is homogeneous, and sets already_allocated_degree_vector
  // to be the LCM of the exponent vectors of the degrees of all terms in f.
  bool multi_degree(const Poly& f, int *already_allocated_degree_vector) const;

  void mult_by_term_right(Poly& result,
                          const Poly& f,
                          const ring_elem c,
                          const Monom m) const;
  void mult_by_term_left(Poly& result,
                         const Poly& f,
                         const ring_elem c,
                         const Monom m) const;
  void mult_by_term_left_and_right(Poly& result,
                                   const Poly& f,
                                   const ring_elem c,
                                   const Monom leftM,
                                   const Monom rightM) const;
  void mult_by_term_left(Poly& result,
                         const Poly& f,
                         const ring_elem c,
                         const Word& w) const;
  void mult_by_term_right(Poly& result,
                          const Poly& f,
                          const ring_elem c,
                          const Word& w) const;
  void mult_by_term_left_and_right(Poly& result,
                                   const Poly& f,
                                   const ring_elem c,
                                   const Word& leftW,
                                   const Word& rightW) const;
  void add_to_end(Poly& f, const Poly& g) const;
  void lead_term_as_poly(Poly& result, const Poly& f) const;
  void mult_by_coeff(Poly& result, const Poly& f, const ring_elem c) const;
  
  SumCollector* make_SumCollector() const;

  void lead_word(Word& result, const Poly& f) const;
  void lead_word_prefix(Word& result, const Poly& f, int endIndex) const;
  void lead_word_suffix(Word& result, const Poly& f, int beginIndex) const;

#if 0  
  Poly* eval(const RingMap *map, const Poly* f, int first_var) const;

  int index_of_var(const Poly& a) const;
  
  void text_out(buffer &o) const; // need?

  // if not lift/promoteable, return nullptr
  Poly* promote(const Ring *R, const ring_elem f) const; 
  Poly* lift(const Ring *R, const ring_elem f) const;


  engine_RawArrayPairOrNull list_form(const Ring *coeffR,
                                      const Poly* f) const;

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
      coefficientRing()->remove(a);
    
    f.mCoefficients.clear();
    f.mMonomials.clear();
  }

#endif
  
private:
  const Ring& mCoefficientRing;
  const FreeMonoid& mMonoid;
};

class FreeAlgebraElement
{
public:
  FreeAlgebraElement(const FreeAlgebra* F)
    : mRing(F)
  {
    mRing->init(mPoly);
  }
  ~FreeAlgebraElement()
  {
    mRing->clear(mPoly);
  }
  Poly& operator*()
  {
    return mPoly;
  }
  const Poly& operator*() const
  {
    return mPoly;
  }
  bool operator==(const FreeAlgebraElement& g) const
  {
    return this->mRing->is_equal(**this,*g);
  }
  FreeAlgebraElement operator+(const FreeAlgebraElement& g)
  {
    FreeAlgebraElement result(mRing);
    mRing->add(*result, **this, *g);
    return result;  // this is a copy
  }
  FreeAlgebraElement operator-(const FreeAlgebraElement& g)
  {
    FreeAlgebraElement result(mRing);
    mRing->subtract(*result, **this, *g);
    return result;  // this is a copy
  }
  FreeAlgebraElement operator*(const FreeAlgebraElement& g)
  {
    FreeAlgebraElement result(mRing);
    mRing->mult(*result, **this, *g);
    return result;  // this is a copy
  }
  FreeAlgebraElement operator^(int n)
  {
    FreeAlgebraElement result(mRing);
    mRing->power(*result, **this, n);
    return result;  // this is a copy
  }
  
private:
  const FreeAlgebra* mRing;
  Poly mPoly;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

