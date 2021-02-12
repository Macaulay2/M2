#ifndef _free_algebra_hpp_
#define _free_algebra_hpp_

#include "Polynomial.hpp"             // for Poly, Monom
#include "NCAlgebras/FreeMonoid.hpp"  // for FreeMonoid
#include "NCAlgebras/Word.hpp"        // for Word
#include "newdelete.hpp"              // for our_new_delete
#include "ring.hpp"                   // for Ring (ptr only), SumCollector
#include "ringelem.hpp"               // for ring_elem
#include "style.hpp"                  // for GEOHEAP_SIZE

#include <gmp.h>                      // for mpz_srcptr, mpq_srcptr
#include <iosfwd>                     // for ostream, string
#include <utility>                    // for pair
#include <vector>                     // for vector

class Monoid;
class PolynomialRing;
class RingMap;
class buffer;

class FreeAlgebra : public our_new_delete
{
private:
  const Ring& mCoefficientRing;
  const FreeMonoid& mMonoid;

private:
  FreeAlgebra(const Ring* K, const FreeMonoid* M);

public:
  static FreeAlgebra* create(const Ring* K,
                             const std::vector<std::string>& names,
                             const PolynomialRing* degreeRing,
                             const std::vector<int>& degrees,
                             const std::vector<int>& wtvecs,
                             const std::vector<int>& heftVector
                             );

  const Ring* coefficientRing() const { return &mCoefficientRing; }
  const FreeMonoid& monoid() const { return mMonoid; }
  const Monoid& degreeMonoid() const { return monoid().degreeMonoid(); }
  int numVars() const { return monoid().numVars(); }
  
  unsigned int computeHashValue(const Poly& a) const; // TODO

  void init(Poly& f) const {}
  void clear(Poly& f) const;
  void setZero(Poly& f) const;

  void copy(Poly& result, Poly::const_iterator fBegin, Poly::const_iterator fEnd) const;
  
  void from_coefficient(Poly& result, const ring_elem a) const;
  void from_long(Poly& result, long n) const;
  void from_int(Poly& result, mpz_srcptr n) const; 
  bool from_rational(Poly& result, const mpq_srcptr q) const; 
  void copy(Poly& result, const Poly& f) const;
  void swap(Poly& f, Poly& g) const;
  void var(Poly& result, int v) const;

  M2_arrayint support(const Poly& f) const;
  
  void from_word(Poly& result, const Word& word) const;
  void from_word(Poly& result, const std::vector<int>& word) const; 
  void from_word(Poly& result, ring_elem coeff, const Word& word) const; 
  void from_word(Poly& result, ring_elem coeff, const std::vector<int>& word) const; 
  
  long n_terms(const Poly& f) const { return f.numTerms(); }  
  bool is_unit(const Poly& f) const;
  bool is_zero(const Poly& f) const { return n_terms(f) == 0; }
  bool is_equal(const Poly& f, const Poly& g) const;
  int compare_elems(const Poly& f, const Poly& g) const;

  void add(Poly& result,
           Poly::const_iterator fBegin,
           Poly::const_iterator fEnd,
           Poly::const_iterator gBegin,
           Poly::const_iterator gEnd) const;
  void addScalarMultipleOf(Poly& result,
                           Poly::const_iterator fBegin,
                           Poly::const_iterator fEnd,
                           Poly::const_iterator gBegin,
                           Poly::const_iterator gEnd,
                           ring_elem coeff) const;  
  
  void negate(Poly& result, const Poly& f) const; 
  void add(Poly& result, const Poly& f, const Poly& g) const;
  
  void subtract(Poly& result, const Poly& f, const Poly& g) const;
  void subtractScalarMultipleOf(Poly& result, 
                                const Poly& f,
                                const Poly& g,
                                ring_elem coeff) const;
  void mult(Poly& result, const Poly& f, const Poly& g) const;
  void power(Poly& result, const Poly& f, int n) const;
  void power(Poly& result, const Poly& f, mpz_srcptr n) const;

  ring_elem eval(const RingMap *map, const Poly& f, int first_var) const;

  void makeMonic(Poly& result, Poly& f) const;
  void makeMonicInPlace(Poly& f) const;
  
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

  // Returns the pair (d, ishomog) where
  // d is the largest heft of the degree of each monomial in 'f'.
  // ishomog is true when all monomials have the same *heft* degree
  std::pair<int, bool> heft_degree(const Poly& f) const;
  
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
  void mult_by_term_left_and_right(Poly& result,
                                   const Poly& f,
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
  void mult_by_term_left_and_right(Poly& result,
                                   const Poly& f,
                                   const Word& leftW,
                                   const Word& rightW) const;
  void add_to_end(Poly& f, const Poly& g) const;
  void add_to_end(Poly& f, ring_elem coeff, const Monom& monom) const;
  
  void lead_term_as_poly(Poly& result, const Poly& f) const;
  void mult_by_coeff(Poly& result, const Poly& f, const ring_elem c) const;
  
  SumCollector* make_SumCollector() const;

  Word lead_word(const Poly& f) const;
  Word lead_word_prefix(const Poly& f, int endIndex) const;
  Word lead_word_suffix(const Poly& f, int beginIndex) const;

  // this stuff has yet to be moved over from ../M2FreeAlgebra
#if 0  

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
};

// For debugging purposes
class FreeAlgebraElement
{
public:
  FreeAlgebraElement(const FreeAlgebra* F)
    : mRing(F)
  {
    mRing->init(mPoly);
  }
  FreeAlgebraElement(const FreeAlgebra* F, const Poly& f)
    : mRing(F),
      mPoly(f)
  {
  }
  ~FreeAlgebraElement()
  {
    mRing->clear(mPoly);
  }
  const FreeAlgebra& ring() const
  {
    return *mRing;
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
  FreeAlgebraElement operator-() const
  {
    FreeAlgebraElement result(mRing);
    mRing->negate(*result, **this);
    return result;
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

std::ostream& operator<<(std::ostream& o, const FreeAlgebraElement& f);

// FreeAlgebraHeap and the SumCollector below are used for eval and mult

class FreeAlgebraHeap
{
  const FreeAlgebra& F;  // Our elements will be vectors in here
  Poly heap[GEOHEAP_SIZE];
  int top_of_heap;

 public:
  FreeAlgebraHeap(const FreeAlgebra& F);
  ~FreeAlgebraHeap();

  void add(const Poly& f);
  void value(Poly& result);  // Returns the linearized value, and resets the FreeAlgebraHeap.

  const Poly& debug_list(int i) const
  {
    return heap[i];
  }  // DO NOT USE, except for debugging purposes!
};

class SumCollectorFreeAlgebraHeap : public SumCollector
{
  FreeAlgebraHeap H;

 public:
  SumCollectorFreeAlgebraHeap(const FreeAlgebra& F) : H(F) {}
  ~SumCollectorFreeAlgebraHeap() {}
  virtual void add(ring_elem f1)
  {
    auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
    H.add(*f);
  }
  virtual ring_elem getValue()
  {
    Poly* result = new Poly;
    H.value(*result);
    return ring_elem(reinterpret_cast<void *>(result));
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

