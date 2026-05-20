#ifndef _free_algebra_hpp_
#define _free_algebra_hpp_

/**
 * @file NCAlgebras/FreeAlgebra.hpp
 * @brief Free associative algebra `k<x_1,...,x_n>` over an arbitrary coefficient ring.
 *
 * Declares the non-commutative analogue of `PolyRing`: a
 * `FreeAlgebra` pairs a coefficient `Ring` reference with a
 * `shared_ptr<FreeMonoid>` carrying the word side (names,
 * degrees, weight and heft vectors, monoid order). Polynomial
 * values are the shared `Poly` type from `Polynomial.hpp`,
 * stored as a vector of `(coefficient, word)` pairs sorted by
 * the monoid order; multiplication is
 * `Sum_{i,j} (c_i*d_j) * (m_i ~ n_j)` where `~` is
 * concatenation. A `SumCollector` from `ring.hpp` combines like
 * terms during accumulation.
 *
 * `FreeAlgebra` does not inherit from `Ring`; the `Ring`-shaped
 * facade lives in `M2FreeAlgebra` and `M2FreeAlgebraQuotient`.
 * The static `create(...)` factory takes the coefficient ring,
 * variable names, degree ring, and per-variable degree / weight
 * / heft vectors, builds the matching `FreeMonoid`, and returns
 * the object the NCGroebner / NCF4 engines consume.
 *
 * @see FreeMonoid.hpp
 * @see Word.hpp
 * @see Polynomial.hpp
 * @see NCGroebner.hpp
 * @see FreeAlgebraQuotient.hpp
 * @see M2FreeAlgebra.hpp
 */

#include "Polynomial.hpp"             // for Poly, Monom
#include "NCAlgebras/FreeMonoid.hpp"  // for FreeMonoid
#include "NCAlgebras/Word.hpp"        // for Word
#include "newdelete.hpp"              // for our_new_delete
#include "ring.hpp"                   // for Ring (ptr only), SumCollector
#include "ringelem.hpp"               // for ring_elem
#include "style.hpp"                  // for GEOHEAP_SIZE

#include <iosfwd>                     // for ostream, string
#include <utility>                    // for pair
#include <vector>                     // for vector

class Monoid;
class PolynomialRing;
class RingMap;
class buffer;

/**
 * @brief Free associative algebra over a coefficient ring: the
 * non-commutative analogue of `PolynomialRing`.
 *
 * @details Bundles the coefficient ring with a `FreeMonoid` of words on the
 * generators and exposes the operation surface the non-commutative
 * arithmetic / Groebner stack (`NCGroebner`, `NCF4`) reduces against:
 * `from_word`, `add`, `subtract`, `mult`, `mult_by_term_left_and_right`,
 * `power`, and the lead-term accessors. Created through the static
 * `create()` factory from a coefficient ring plus the variable names,
 * degrees, weight vectors, and heft vector; the constructor itself is
 * private.
 */
class FreeAlgebra : public our_new_delete
{
private:
  const Ring& mCoefficientRing;
  std::shared_ptr<FreeMonoid> mMonoid;


private:
  FreeAlgebra(const Ring* K, std::shared_ptr<FreeMonoid> M);

public:
  static FreeAlgebra* create(const Ring* K,
                             const std::vector<std::string>& names,
                             const PolynomialRing* degreeRing,
                             const std::vector<int>& degrees,
                             const std::vector<int>& wtvecs,
                             const std::vector<int>& heftVector
                             );

  const Ring* coefficientRing() const { return &mCoefficientRing; }
  const FreeMonoid& monoid() const { return *mMonoid; }
  const Monoid& degreeMonoid() const { return monoid().degreeMonoid(); }
  int numVars() const { return monoid().numVars(); }
  
  unsigned int computeHashValue(const Poly& a) const; // TODO

  void init(Poly& f) const { (void) f; }
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
  // FIXME: copied from ring.hpp because this doesn't inherit from Ring
  inline const_monomial degree(const Poly& f) const
  {
    auto d = degreeMonoid().make_one();
    multi_degree(f, d);
    return d;
  }
  // returns true if f is homogeneous, and sets already_allocated_degree_vector
  // to be the LCM of the exponent vectors of the degrees of all terms in f.
  bool multi_degree(const Poly& f, monomial already_allocated_degree_vector) const;

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

  Poly* makeTerm(const ring_elem a, const_varpower monom) const;

  void setZero(Poly& f) const // resets f to zero
  {
    for (auto a : f.mCoefficients)
      coefficientRing()->remove(a);
    
    f.mCoefficients.clear();
    f.mMonomials.clear();
  }

#endif
};

/**
 * @brief Owned `Poly` value paired with its `FreeAlgebra*`, providing
 * natural operator-overloaded arithmetic.
 *
 * @details Debugging / scripting convenience: wraps a `Poly` so the algebra
 * pointer is implicit and `+`, `-`, `*`, unary `-`, and `^n`
 * (power) can be written directly. The destructor calls
 * `FreeAlgebra::clear`, so callers do not have to manage the
 * underlying `Poly` lifetime.
 */
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

/**
 * @brief Geobucket-style accumulator for many `Poly` summands in the free
 * associative algebra.
 *
 * @details Maintains `GEOHEAP_SIZE` size-tiered buckets; `add(f)` slots `f`
 * into the smallest non-empty bucket it overflows and merges
 * upward, so an O(`n`) chain of additions costs amortised O(`n log
 * n`) work rather than O(`n^2`). `value()` linearises the buckets
 * into a single `Poly` and resets the heap. Used by `mult` and
 * evaluation paths that build a polynomial as a long sum of
 * `mult_by_term_left_and_right` contributions.
 */
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

/**
 * @brief `SumCollector` adapter that funnels engine-side `ring_elem` adds
 * into a `FreeAlgebraHeap`.
 *
 * @details Used wherever the engine's generic `SumCollector` interface
 * (e.g. for `RingMap` evaluation) needs to accumulate
 * free-associative polynomials: each `add(ring_elem f1)` casts
 * down to `Poly*` and pushes through to `FreeAlgebraHeap::add`,
 * and `getValue()` returns the linearised sum as a fresh
 * `ring_elem`.
 */
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

