// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_POLY_RING_GUARD
#define MATHICGB_POLY_RING_GUARD

#include "MonoMonoid.hpp"
#include "PrimeField.hpp"
#include <assert.h>
#include <string>
#include <vector>
#include "memtailor/memtailor.h"
#include <cstdio>
#include <cstring>
#include <limits>

MATHICGB_NAMESPACE_BEGIN

#define LT (-1)
#define EQ 0
#define GT 1


template<class T>
PrimeField<
  typename std::make_unsigned<
    typename std::remove_reference<T>::type
  >::type
> makeField(T charac) {
  return charac;
}

/** Returns a^-1 mod modulus. It is required that 0 < a < modulus. */
template<class T>
T modularInverse(T a, T modulus) {
  MATHICGB_ASSERT(0 < a);
  MATHICGB_ASSERT(a < modulus);
  auto f = makeField(modulus);
  return f.inverse(f.toElementInRange(a)).value();
}

template<class T>
struct ModularProdType {};
template<> struct ModularProdType<uint8> {typedef uint16 type;};
template<> struct ModularProdType<uint16> {typedef uint32 type;};
template<> struct ModularProdType<uint32> {typedef uint64 type;};
template<> struct ModularProdType<int8> {typedef int16 type;};
template<> struct ModularProdType<int16> {typedef int32 type;};
template<> struct ModularProdType<int32> {typedef int64 type;};

/** Returns a*b mod modulus.  It is required that 0 <= a, b < modulus. */
template<class T>
T modularProduct(T a, T b, T modulus) {
  MATHICGB_ASSERT(0 < a);
  MATHICGB_ASSERT(a < modulus);
  MATHICGB_ASSERT(0 <= b);
  MATHICGB_ASSERT(b < modulus);
  auto f = makeField(modulus);
  return f.product(f.toElementInRange(a), f.toElementInRange(b)).value();
}

/** Returns a+b mod modulus.  It is required that 0 <= a, b < modulus. */
template<class T>
T modularSum(T a, T b, T modulus) {
  MATHICGB_ASSERT(0 < a);
  MATHICGB_ASSERT(a < modulus);
  MATHICGB_ASSERT(0 <= b);
  MATHICGB_ASSERT(b < modulus);
  auto f = makeField(modulus);
  return f.sum(f.toElementInRange(a), f.toElementInRange(b)).value();
} 

/** Returns -a mod modulus. It is required that 0 <= a < modulus. */
template<class T>
T modularNegative(T a, T modulus) {
  MATHICGB_ASSERT(0 < a);
  MATHICGB_ASSERT(a < modulus);
  auto f = makeField(modulus);
  return f.negative(f.toElementInRange(a)).value();
}

/** Returns -a mod modulus. It is required that 0 < a < modulus. */
template<class T>
T modularNegativeNonZero(T a, T modulus) {
  MATHICGB_ASSERT(0 < a);
  MATHICGB_ASSERT(a < modulus);
  auto f = makeField(modulus);
  return f.negativeNonZero(f.toElementInRange(a)).value();
}

typedef int32 exponent ;
typedef uint32 HashValue;
typedef long coefficient;
typedef MonoMonoid<exponent> Monoid;

/// This typedef should really be for an unsigned type.
typedef PrimeField<coefficient> Field;


typedef exponent* vecmonomial; // includes a component
typedef coefficient const_coefficient;

#define OLDMON
#ifdef OLDMON
class Monomial;

class ConstMonomial
{
  friend class PolyRing;
  friend class OrderA;
  friend class OrderB;
  friend class OrderC;
  friend class OrderD;
  friend class OrderE;
public:
  //* Wrap a raw pointer to create a monomial
  // Assumptions:
  //  1. This is done in the presence of a PolyRing
  //  2. Space for the monomial has been created
  ConstMonomial() : mValue(0) {}
  ConstMonomial(const exponent *val) : mValue(val) {}

  inline const Monomial& castAwayConst() const;

  bool isNull() const { return mValue == 0; }

  exponent const * unsafeGetRepresentation() const { return mValue; }


  exponent component() const { return *mValue; }

  operator Monoid::ConstMonoRef() const {
    MATHICGB_ASSERT(!isNull());
    return Monoid::toRef(mValue);
  }
  operator Monoid::ConstMonoPtr() const {
    return Monoid::toRef(mValue).ptr();
  }

private:
  const exponent& operator[](size_t index) const { return mValue[index]; }
  const exponent& operator*() const { return *mValue; }

protected:
  exponent const* mValue;
};

class Monomial : public ConstMonomial
{
  friend class PolyRing;
  friend class OrderA;
  friend class OrderB;
  friend class OrderC;
  friend class OrderD;
  friend class OrderE;
public:
  //* Wrap a raw pointer to create a monomial
  // Assumptions:
  //  1. This is done in the presence of a PolyRing
  //  2. Space for the monomial has been created
  Monomial() : ConstMonomial() {}
  Monomial(exponent *val) : ConstMonomial(val) {}

  void swap(Monomial& monomial) {
    std::swap(mValue, monomial.mValue);
  }

  exponent * unsafeGetRepresentation() { return const_cast<exponent *>(mValue); }
  exponent const * unsafeGetRepresentation() const { return mValue; }

  operator Monoid::MonoRef() {
    MATHICGB_ASSERT(!isNull());
    return Monoid::toRef(unsafeGetRepresentation());
  }
  operator Monoid::MonoPtr() {
    return Monoid::toRef(unsafeGetRepresentation()).ptr();
  }

private:
  const exponent& operator[](size_t index) const { return mValue[index]; }
  exponent& operator[](size_t index) { return unsafeGetRepresentation()[index]; }

  const exponent& operator*() const { return *mValue; }
  exponent& operator*() { return * const_cast<exponent *>(mValue); }
};

inline const Monomial& ConstMonomial::castAwayConst() const
{
  return reinterpret_cast<const Monomial&>(*this);
}

typedef Monomial monomial;
typedef ConstMonomial const_monomial;
#else
typedef MonoMonoid<exponent>::MonoPtr monomial;
typedef MonoMonoid<exponent>::MonoPtr Monomial;
typedef MonoMonoid<exponent>::ConstMonoPtr const_monomial;
typedef MonoMonoid<exponent>::ConstMonoPtr ConstMonomial;
#endif

struct NewConstTerm {
  coefficient coef;
  MonoMonoid<exponent>::ConstMonoPtr mono;
};

struct NewTerm {
  coefficient coef;
  MonoMonoid<exponent>::MonoPtr mono;

  operator NewConstTerm() const {
    NewConstTerm t = {coef, mono};
    return t;
  }
};

struct const_term {
  const_term() {}
  const_term(const_coefficient c, const_monomial m) : coeff(c), monom(m) {}

  const_coefficient coeff;
  const_monomial monom;
};

struct term {
  term() {}
  term(coefficient c, monomial m) : coeff(c), monom(m) {}

  coefficient coeff;
  monomial monom;

  operator const_term() const {return const_term(coeff, monom);}
};

class PolyRing {
public:
  typedef MonoMonoid<exponent> Monoid;

  /// This typedef should really be for an unsigned type.
  typedef PrimeField<coefficient> Field;

  /// @todo: make this dependent on the monoid and field once all code
  /// has been migrated from ::term to PolyRing::Term.
  typedef mgb::term Term;

  PolyRing(
    coefficient charac,
    int nvars,
    bool lexBaseOrder,
    std::vector<exponent>&& weights
  );
  PolyRing(const Field& field, Monoid&& monoid);

  size_t getMemoryUse() const {
    // todo: Make this more accurate.
    return 0;
  }

  coefficient charac() const { return mField.charac(); }
  size_t getNumVars() const { return varCount();}
  size_t varCount() const {return monoid().varCount();}

  // Allocate a monomial from an arena A.
  // This monomial may only be freed if no other elements that were allocated
  // later are live on A.  In this case, use freeMonomial(A,m) to free 'm'.
  Monomial allocMonomial(memt::Arena &A) const {
    exponent* ptr = static_cast<exponent*>(A.alloc(maxMonomialByteSize()));
#ifdef MATHICGB_DEBUG
    // fill with value that do not make sense to catch bugs in debug
    // mode. The maximum value of setting all bits increases the
    // chances of getting an assert.
    std::fill_n(reinterpret_cast<char*>(ptr), maxMonomialByteSize(),
                ~static_cast<char>(0));
#endif
    return ptr;
  }

  // Free monomial 'm' obtained by allocMonomial(A) by calling
  // freeMonomial(A,m) Recall this only works if this was the last
  // thing allocated in A.
  void freeTopMonomial(memt::Arena &A, Monomial m) const {
    A.freeTop(m.unsafeGetRepresentation());
  }

  //  Allocate a monomial from a pool that has had its size set to 
  //   maxMonomialByteSize()
  //  Free monomials here using the SAME pool
  Monomial allocMonomial(memt::BufferPool &P) const {
    exponent* ptr = static_cast<exponent*>(P.alloc());
#ifdef MATHICGB_DEBUG
    // fill with value that do not make sense to catch bugs in debug
    // mode. The maximum value of setting all bits increases the
    // chances of getting an assert.
    std::fill_n(reinterpret_cast<char*>(ptr), maxMonomialByteSize(),
                ~static_cast<char>(0));
#endif
    return ptr;
  }

  // Free monomial 'm' obtained by allocMonomial(P) 
  // by calling freeMonomial(P,m)
  void freeMonomial(memt::BufferPool &P, Monomial m) const {
    P.free(m.unsafeGetRepresentation());
  }

  // Only call this method for monomials returned by allocMonomial().
  void freeMonomial(Monomial m) const {
    monoid().freeRaw(Monoid::MonoPtr(m));
  }

  // Free monomials allocated here by calling freeMonomial().
  monomial allocMonomial() const {
    return Monoid::rawPtr(monoid().alloc().release());
  }

  bool fromPool(ConstMonomial m) const {
    return monoid().fromPool(m);
  }




  coefficient toCoefficient(int64 value) const;
  coefficient coefficientNegate(coefficient coeff) const;
  coefficient coefficientNegateNonZero(coefficient coeff) const;
  coefficient coefficientSubtract(coefficient a, coefficient b) const;

  void coefficientFromInt(coefficient &result, int a) const;
  void coefficientSetOne(coefficient &result) const { result = 1; }
  void coefficientAddOneTo(coefficient &result) const;
  void coefficientReciprocalTo(coefficient &result) const;
  void coefficientNegateTo(coefficient &result) const; // result = -result
  void coefficientAddTo(coefficient &result, coefficient a, coefficient b) const; // result += a*b
  void coefficientAddTo(coefficient &result, coefficient a) const; // result += a
  void coefficientMultTo(coefficient &result, coefficient a) const; // result *= a
  void coefficientMult(coefficient a, coefficient b, coefficient &result) const; // result = a*b
  void coefficientDivide(coefficient a, coefficient b, coefficient &result) const; // result /= a
  void coefficientSet(coefficient &result, coefficient a) const { result = a; }
  bool coefficientIsZero(coefficient a) const { return a == 0; }
  bool coefficientIsOne(coefficient a) const { return a == 1; }
  bool coefficientEQ(coefficient a, coefficient b) const { return a == b; }

  size_t maxMonomialByteSize() const { return maxMonomialSize() * sizeof(exponent); }

  size_t maxMonomialSize() const { return monoid().entryCount(); }

  ///////////////////////////////////////////
  // Monomial Routines //////////////////////
  ///////////////////////////////////////////

  HashValue monomialHashValue(ConstMonomial m) const {
    return monoid().hash(m);
  }

  void monomialSetExponent(Monomial m, size_t var, exponent c) const {
    monoid().setExponent(var, c, m);
  }

  void monomialSetExternalExponents(Monomial m, exponent* exponents) const {
    monoid().setExternalExponents(exponents, m);
  }

  exponent monomialExponent(ConstMonomial m, size_t var) const {
    return monoid().exponent(m, var);
  }

  // This function only sets component and the monomial itself. NOT weights, degree, or hash value
  //TODO: get Bjarke to name this function!!
  void mysteriousSPairMonomialRoutine(ConstMonomial newSig,
                                      ConstMonomial newLead,
                                      ConstMonomial baseDivSig,
                                      ConstMonomial baseDivLead,
                                      Monomial result) const;

  // Returns the weight (degree) of a. Takes the first weight if
  // working with several weight vectors.
  exponent weight(ConstMonomial a) const;

  void setWeightsAndHash(Monomial& a) const;

  inline void setWeightsOnly(Monomial& a) const;

  inline void setHashOnly(Monomial& a) const;

  // returns LT, EQ, or GT, depending on sig ? (m2 * sig2).
  int monomialCompare(ConstMonomial a, 
                      ConstMonomial b) const; 
  // returns LT, EQ or GT
  int monomialCompare(ConstMonomial sig, 
                      ConstMonomial m2, 
                      ConstMonomial sig2) const;

  // If this method returns true for monomials a and b then it is guaranteed
  // the multiplying a and b together will not overflow the underlying
  // exponent integer. Does not work for negative exponents.
  bool monomialHasAmpleCapacity(ConstMonomial mono) const;

  bool monomialLT(ConstMonomial a, ConstMonomial b) const {
    return monoid().lessThan(a, b);
  }

  bool monomialEQ(ConstMonomial a, ConstMonomial b) const;

  /// as monomialEQ, but optimized for the case that the answer is true.
  bool monomialEqualHintTrue(ConstMonomial a, ConstMonomial b) const;

  exponent monomialGetComponent(ConstMonomial a) const { return *a.mValue; }

  void monomialChangeComponent(Monomial a, int x) const {
    monoid().setComponent(x, a);
  }

  void monomialSetIdentity(Monomial& result) const;

  void monomialEi(Monoid::Component i, Monomial &result) const;

  void monomialMult(ConstMonomial a, ConstMonomial b, Monomial &result) const;

  void monomialMultTo(Monomial &a, ConstMonomial b) const; // a *= b

  /// Result is set to b/a. a must divide b.
  void monomialDivide(ConstMonomial a, ConstMonomial b, Monomial &result) const;

  /// sets result to a/b, even if that produces negative exponents.
  void monomialDivideToNegative(ConstMonomial a, ConstMonomial b, Monomial &result) const;

  /// Sets aColonB = a:b and bColonA = b:a.
  void monomialColons(ConstMonomial a, ConstMonomial b, monomial aColonB, monomial bColonA) const;

  /// returns true if b divides a.  Components are ignored.
  bool monomialIsDivisibleBy(ConstMonomial a, ConstMonomial b) const;

  /// Returns true if ab is the product of a and b.
  bool monomialIsProductOf
    (ConstMonomial a, ConstMonomial b, ConstMonomial ab) const;

  /// As monomialIsProductOf but optimized for the case that the result
  /// is true.
  bool monomialIsProductOfHintTrue
    (ConstMonomial a, ConstMonomial b, ConstMonomial ab) const;

  /// As monomialIsProductOfHintTwo(), but checks two products are equal.
  /// The return value is true if a1*b = a1b and a2*b = a2b.
  MATHICGB_INLINE bool monomialIsTwoProductsOfHintTrue(
    ConstMonomial a1,
    ConstMonomial a2,
    ConstMonomial b,
    ConstMonomial a1b,
    ConstMonomial a2b) const;

  /// Returns the hash of the product of a and b.
  HashValue monomialHashOfProduct(ConstMonomial a, ConstMonomial b) const {
    return monoid().hashOfProduct(a, b);
  }

  void monomialCopy(ConstMonomial  a, Monomial &result) const;

  void monomialQuotientAndMult(ConstMonomial a, 
                               ConstMonomial b, 
                               ConstMonomial c, 
                               Monomial& result) const;
  // result is set to (a//b) * c

  inline bool monomialRelativelyPrime(ConstMonomial a, 
                                      ConstMonomial b) const;

  void monomialFindSignature(ConstMonomial v1,
                             ConstMonomial v2,
                             ConstMonomial u1,
                             Monomial& t1) const; 
  // answer into the already allocated t1

  size_t monomialSizeOfSupport(ConstMonomial m) const;

  inline void monomialLeastCommonMultiple(ConstMonomial a, 
                                          ConstMonomial b, 
                                          Monomial& l) const;

  bool monomialIsLeastCommonMultiple(ConstMonomial a, 
                                     ConstMonomial b, 
                                     ConstMonomial l) const;

  // Returns true if there is a variable var such that hasLarger raises var to
  // a strictly greater exponent than both smaller1 and smaller2 does.
  inline bool monomialHasStrictlyLargerExponent(
    ConstMonomial hasLarger,
    ConstMonomial smaller1,
    ConstMonomial smaller2) const;

  void monomialParse(std::istream& i, 
                     Monomial& result) const;

  void monomialDisplay(std::ostream& o, 
                       ConstMonomial a, 
                       bool print_comp=true, 
                       bool print_one=true) const;
  void monomialDisplay(FILE* file,
                       ConstMonomial a, 
                       bool printComponent = true, 
                       bool printOne = true) const;

  void printMonomialFrobbyM2Format(std::ostream& out, ConstMonomial m) const;

  ///////////////////////////////////////////
  ///////////////////////////////////////////

  const Monoid& monoid() const {return mMonoid;}
  const Field& field() const {return mField;}

private:
  Field mField;
  Monoid mMonoid;
};

inline exponent PolyRing::weight(ConstMonomial a) const {
  return monoid().degree(a);
}

////////////////////////////////////////////////
// New Monomial Routines ///////////////////////
////////////////////////////////////////////////

inline bool PolyRing::monomialEQ(ConstMonomial a, ConstMonomial b) const
{
  return monoid().equal(a, b);
}

inline bool PolyRing::monomialEqualHintTrue(
  const ConstMonomial a,
  const ConstMonomial b
) const {
  return monoid().equalHintTrue(a, b);
}

inline bool PolyRing::monomialIsProductOfHintTrue(
  const ConstMonomial a, 
  const ConstMonomial b, 
  const ConstMonomial ab
) const {
  return monoid().isProductOfHintTrue(a, b, ab);
}

MATHICGB_INLINE bool PolyRing::monomialIsTwoProductsOfHintTrue(
  const ConstMonomial a1,
  const ConstMonomial a2,
  const ConstMonomial b,
  const ConstMonomial a1b,
  const ConstMonomial a2b
) const {
  return monoid().isTwoProductsOfHintTrue(a1, a2, b, a1b, a2b);
}

inline bool PolyRing::monomialIsProductOf(
  ConstMonomial a, 
  ConstMonomial b, 
  ConstMonomial ab
) const {
  return monoid().isProductOf(a, b, ab);
}

inline void PolyRing::monomialMult(ConstMonomial a, 
                                   ConstMonomial b, 
                                   Monomial &result) const
{
  monoid().multiply(a, b, result);
}

inline void PolyRing::setWeightsOnly(Monomial& a1) const
{
  monoid().setOrderData(a1);
}

inline void PolyRing::setHashOnly(Monomial& a1) const
{
  monoid().setHash(a1);
}

inline int PolyRing::monomialCompare(ConstMonomial a, ConstMonomial b) const
// returns LT, EQ or GT
{
  return monoid().compare(a, b);
}

inline bool PolyRing::monomialIsDivisibleBy(ConstMonomial a,
                                            ConstMonomial b) const
{
  return monoid().divides(b, a);
}

inline void PolyRing::monomialDivide(ConstMonomial a, 
                                     ConstMonomial b, 
                                     Monomial& result) const
{
  return monoid().divide(b, a, result);
}

inline void PolyRing::monomialColons(
  ConstMonomial a,
  ConstMonomial b,
  monomial aColonB,
  monomial bColonA
) const {
  monoid().colons(a, b, aColonB, bColonA);
}

inline void PolyRing::monomialDivideToNegative(ConstMonomial a, 
                                               ConstMonomial b, 
                                               Monomial& result) const 
{
  monoid().divideToNegative(b, a, result);
}

inline bool PolyRing::monomialRelativelyPrime(ConstMonomial a, 
                                              ConstMonomial b) const
{
  return monoid().relativelyPrime(a, b);
}

inline void PolyRing::monomialLeastCommonMultiple(
  ConstMonomial a,
  ConstMonomial b,
  Monomial& l) const
{
  monoid().lcm(a, b, l);
}

inline bool PolyRing::monomialHasStrictlyLargerExponent(
  ConstMonomial hasLarger,
  ConstMonomial smaller1,
  ConstMonomial smaller2) const 
{
  return !monoid().dividesLcm(hasLarger, smaller1, smaller2);
}


////////////////////////////////////////////////
// Old Monomial Routines ///////////////////////
////////////////////////////////////////////////

inline bool PolyRing::monomialIsLeastCommonMultiple(
  ConstMonomial a,
  ConstMonomial b,
  ConstMonomial l) const
{
  return monoid().isLcm(a, b, l);
}

inline void PolyRing::coefficientReciprocalTo(coefficient& result) const
{
  result = field().inverse(field().toElementInRange(result)).value();
}

inline void PolyRing::coefficientDivide(coefficient a, coefficient b, coefficient &result) const
 // result = a/b
{
  result = field().quotient
    (field().toElementInRange(a), field().toElementInRange(b)).value();
}

inline void PolyRing::coefficientFromInt(coefficient &result, int a) const
{
  result = field().toElement(a).value();
}

inline void PolyRing::coefficientAddOneTo(coefficient &result) const
{
  result = field().plusOne(field().toElementInRange(result)).value();
}

inline void PolyRing::coefficientNegateTo(coefficient& result) const {
  result = field().negative(field().toElementInRange(result)).value();
}

inline coefficient PolyRing::toCoefficient(const int64 value) const {
  return field().toElement(value).value();
}

inline coefficient PolyRing::coefficientNegate(const coefficient coeff) const {
  return field().negative(field().toElementInRange(coeff)).value();
}

inline coefficient PolyRing::coefficientNegateNonZero(
  const coefficient coeff
) const {
  return field().negativeNonZero(field().toElementInRange(coeff)).value();
}

inline coefficient PolyRing::coefficientSubtract(
  const coefficient a,
  const coefficient b
) const {
  return field().difference
    (field().toElementInRange(a), field().toElementInRange(b)).value();
}

inline void PolyRing::coefficientAddTo
(coefficient &result, coefficient a, coefficient b) const
// result += a*b
{
  const auto prod =
    field().product(field().toElementInRange(a), field().toElementInRange(b));
  result = field().sum(field().toElementInRange(result), prod).value();
}

inline void PolyRing::coefficientAddTo(coefficient &result, coefficient a) const
 // result += a
{
  result = field().sum
    (field().toElementInRange(result), field().toElementInRange(a)).value();
}

inline void PolyRing::coefficientMultTo
(coefficient &result, coefficient a) const
  // result *= a
{
  result = field().product
    (field().toElementInRange(result), field().toElementInRange(a)).value();
}

inline void PolyRing::coefficientMult
(coefficient a, coefficient b, coefficient &result) const
{
  result = field().product
    (field().toElementInRange(a), field().toElementInRange(b)).value();
}

inline bool PolyRing::monomialHasAmpleCapacity(ConstMonomial mono) const {
  return monoid().hasAmpleCapacity(mono);
}

/// Returns true if a and b are the same object.
inline bool operator==(const PolyRing& a, const PolyRing& b) {
  return &a == &b;
}

/// As !(a == b).
inline bool operator!=(const PolyRing& a, const PolyRing& b) {
  return !(a == b);
}

MATHICGB_NAMESPACE_END
#endif
