#ifndef _free_algebra_quotient_hpp_
#define _free_algebra_quotient_hpp_

/**
 * @file NCAlgebras/FreeAlgebraQuotient.hpp
 * @brief A `FreeAlgebra` modulo a two-sided ideal carried by an embedded `NCGroebner`.
 *
 * Declares the non-commutative analogue of `PolyRingQuotient`
 * (the commutative quotient ring lives in `polyquotient.hpp`;
 * the `PolyQuotient` name in `polyring.hpp` is just a stale
 * forward declaration). The class holds a reference to its
 * ambient `FreeAlgebra` together with an `NCGroebner mGroebner`
 * that owns the stored Gröbner basis of the defining ideal
 * (the word-table / suffix-tree indices for divisor lookup
 * also live inside `NCGroebner` --- in-source comments here
 * confirm the lookup tables were "placed in NCGroebner
 * object"). The constructor takes a precomputed
 * `ConstPolyList& GB` plus a `maxdeg` cap. `normalizeInPlace`,
 * `mult`, `power`, and the other arithmetic entries compute in
 * the ambient algebra and then reduce modulo the GB via the
 * embedded `NCGroebner`. The full `Ring`-like surface is
 * implemented here even though the class does not inherit from
 * `Ring`; `M2FreeAlgebraQuotient` wraps it to provide the
 * engine-wide `Ring` facade.
 *
 * The companion `FreeAlgebraQuotientElement` at the bottom of
 * the header is a lightweight RAII handle around one `Poly`
 * (calls `init`/`clear` on construction/destruction) with
 * overloaded `+` / `-` / `*` / `^` / `==`, exposed for unit
 * tests and interactive debugging. A two-sided GB in a free
 * algebra is in general infinite, so `maxdeg` is how the
 * engine keeps the computation finite at the cost of incomplete
 * reduction past that degree.
 *
 * @see FreeAlgebra.hpp
 * @see FreeMonoid.hpp
 * @see NCGroebner.hpp
 * @see Polynomial.hpp
 * @see M2FreeAlgebraQuotient.hpp
 * @see polyquotient.hpp
 */

#include "NCAlgebras/FreeAlgebra.hpp" // for FreeAlgebra
#include "NCAlgebras/FreeMonoid.hpp"  // for FreeMonoid
#include "NCAlgebras/NCGroebner.hpp"  // for NCGroebner
#include "Polynomial.hpp"             // for Poly, ConstPolyList
#include "newdelete.hpp"              // for our_new_delete
#include "ringelem.hpp"               // for ring_elem

#include <vector>                     // for vector

class Ring;
class SumCollector;
class buffer;
struct Monoid;
struct RingMap;

/**
 * @brief Quotient of a `FreeAlgebra` by a Groebner basis up to a fixed
 * degree bound.
 *
 * @details Owns the defining basis through an `NCGroebner` and forwards the
 * usual ring operations to `mFreeAlgebra`, then runs
 * `normalizeInPlace` on the result so every output stays in
 * canonical form modulo the relations. `mMaxdeg` bounds the
 * degrees up to which the GB is reliable. Note that this class
 * does NOT inherit from `Ring` --- the engine uses the wrapper
 * `M2FreeAlgebra` / `M2FreeAlgebraOrQuotient` to lift it into the
 * `Ring` hierarchy.
 */
class FreeAlgebraQuotient : public our_new_delete
{
private:
  const FreeAlgebra& mFreeAlgebra;
  NCGroebner mGroebner;
  // placed word tables in NCGroebner object
  //WordTable mWordTable;
  //SuffixTree mWordTable;
  int mMaxdeg;
  
public:
  FreeAlgebraQuotient(const FreeAlgebra& A, const ConstPolyList& GB, int maxdeg);

  const FreeMonoid& monoid() const { return mFreeAlgebra.monoid(); }
  const Monoid& degreeMonoid() const { return monoid().degreeMonoid(); }
  const FreeAlgebra& freeAlgebra() const { return mFreeAlgebra; } 
  
  const Ring* coefficientRing() const { return mFreeAlgebra.coefficientRing(); }

  int numVars() const { return monoid().numVars(); }
  
  unsigned int computeHashValue(const Poly& a) const; // TODO

  void normalizeInPlace(Poly& f) const;
  
  void init(Poly& f) const { (void) f; }
  void clear(Poly& f) const;
  void setZero(Poly& f) const;

  void from_coefficient(Poly& result, const ring_elem a) const;
  void from_long(Poly& result, long n) const;
  void from_int(Poly& result, mpz_srcptr n) const; 
  bool from_rational(Poly& result, const mpq_srcptr q) const; 
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
  void power(Poly& result, const Poly& f, mpz_srcptr n) const;

  ring_elem eval(const RingMap *map, const Poly& f, int first_var) const;
  
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

  SumCollector* make_SumCollector() const;
};

/**
 * @brief Owned `Poly` value paired with its `FreeAlgebraQuotient*`,
 * providing operator-overloaded arithmetic for debugging / scripting.
 *
 * @details Quotient counterpart of `FreeAlgebraElement`: the destructor calls
 * `FreeAlgebraQuotient::clear`, and the arithmetic operators feed
 * through to the quotient's `add` / `subtract` / `mult` / ...
 * methods so each result is automatically reduced modulo the
 * defining ideal.
 */
// for debugging purposes
class FreeAlgebraQuotientElement
{
public:
  FreeAlgebraQuotientElement(const FreeAlgebraQuotient* F)
    : mRing(F)
  {
    mRing->init(mPoly);
  }
  ~FreeAlgebraQuotientElement()
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
  bool operator==(const FreeAlgebraQuotientElement& g) const
  {
    return this->mRing->is_equal(**this,*g);
  }
  FreeAlgebraQuotientElement operator+(const FreeAlgebraQuotientElement& g)
  {
    FreeAlgebraQuotientElement result(mRing);
    mRing->add(*result, **this, *g);
    return result;  // this is a copy
  }
  FreeAlgebraQuotientElement operator-(const FreeAlgebraQuotientElement& g)
  {
    FreeAlgebraQuotientElement result(mRing);
    mRing->subtract(*result, **this, *g);
    return result;  // this is a copy
  }
  FreeAlgebraQuotientElement operator*(const FreeAlgebraQuotientElement& g)
  {
    FreeAlgebraQuotientElement result(mRing);
    mRing->mult(*result, **this, *g);
    return result;  // this is a copy
  }
  FreeAlgebraQuotientElement operator^(int n)
  {
    FreeAlgebraQuotientElement result(mRing);
    mRing->power(*result, **this, n);
    return result;  // this is a copy
  }
  
private:
  const FreeAlgebraQuotient* mRing;
  Poly mPoly;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

