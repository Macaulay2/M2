// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_PRIME_FIELD_GUARD
#define MATHICGB_PRIME_FIELD_GUARD

#include <vector>
#include <limits>
#include <type_traits>
#include <ostream>

MATHICGB_NAMESPACE_BEGIN

/// Implements arithmetic in a prime field. T must be an unsigned integer type
/// that is used to store the elements of the field. The characteristic of the
/// field must be a prime not exceeding std::numeric_limits<T>::max().
template<class T>
class PrimeField {
public:
  typedef T RawElement;

  class Element {
  public:
    // Re-instate this assert once all code has moved to using PrimeField
    // properly so that coefficients are no longer signed.
    //static_assert(!std::numeric_limits<T>::is_signed, "");
    static_assert(std::numeric_limits<T>::is_integer, "");

    /// This constructor/conversion needs to go away as soon as all code
    /// has been converted to using PrimeField properly. Kill it with fire!
    Element(const RawElement& e): mValue(e) {}

    /// This conversion needs to go away as soon as all code
    /// has been converted to using PrimeField properly. Kill it with fire!
    operator RawElement&() {return mValue;}

    /// This conversion needs to go away as soon as all code
    /// has been converted to using PrimeField properly. Kill it with fire!
    operator const RawElement&() const {return mValue;}

    /// This method needs to go away as soon as all code
    /// has been converted to using PrimeField properly. Kill it with fire!
    bool operator==(const RawElement e) const {return value() == e.value();}

    /// This method needs to go away as soon as all code
    /// has been converted to using PrimeField properly. Kill it with fire!
    bool operator!=(const RawElement e) const {return !(*this == e);}

    Element(const Element& e): mValue(e.value()) {}

    Element& operator=(const Element& e) {
      mValue = e.value();
      return *this;
    }

    bool operator==(const Element e) const {return value() == e.value();}
    bool operator!=(const Element e) const {return !(*this == e);}

    T value() const {return mValue;}

  private:
    friend class PrimeField;
    // Uncomment this constructor once the public one is gone.
    //Element(const T value): mValue(value) {}

    friend class PrimeFile;
    T mValue;
  };

  typedef Element& ElementRef;
  typedef const Element& ConstElementRef;
  typedef Element* ElementPtr;
  typedef const Element* ConstElementPtr;

  typedef std::vector<Element> ElementVector;

  PrimeField(const T primeCharacteristic): mCharac(primeCharacteristic) {}

  Element zero() const {return Element(0);}
  Element one() const {return Element(1);}
  Element minusOne() const {return Element(charac() - 1);}

  bool isZero(const Element a) const {return a == zero();}
  bool isOne(const Element a) const {return a == one();}

  /// Returns true if a is strictly in the upper half of the range of values.
  /// These can be considered as a negative number with smaller absolute
  /// value, which can be useful for example when printing the value.
  bool isNegative(const Element a) const {
    return a.value() > (charac() + 1) / 2;
  }

  T charac() const {return mCharac;}

  /// Assumes that i is in the range [0;charac()).
  template<class Integer>
  Element toElementInRange(Integer&& i) const {
    typedef typename std::remove_reference<Integer>::type NoRefInteger;
    static_assert(std::numeric_limits<NoRefInteger>::is_integer, "");

    MATHICGB_ASSERT(0 <= i);
    typedef typename std::make_unsigned<NoRefInteger>::type Unsigned;
    //replace the below assert with this uncommented one once we get rid
    //of signed element types.
    //MATHICGB_ASSERT(static_cast<Unsigned>(i) < charac());
    MATHICGB_ASSERT(i < charac());
    return Element(i);
  }

  template<class Integer>
  Element toElement(Integer&& i) const {
    typedef typename std::remove_reference<Integer>::type NoRefInteger;
    static_assert(std::numeric_limits<NoRefInteger>::is_integer, "");

    // We need to take the modulus of i to put it into the range [0;charac()).
    // That is more tricky to get right than it might seem.
    //
    // The sign of a % b is implementation defined in C++ if either of a or b
    // are negative. We need the positive remainder so the operands have to be
    // positive. Another reason for this is that we could not allow b to be
    // converted to a signed integer since it might not be representable that
    // way.
    //
    // If Integer is signed and i is std::numeric_limits<Integer>::min() then
    // it is undefined behavior to evaluate the expression -i since -i is not
    // representable, leading to a signed overflow. So we have to cast to
    // unsigned before doing the minus.
    typedef typename std::make_unsigned<NoRefInteger>::type Unsigned;
    if (i < 0) {
      // Negate i to get a positive number, then negate again to cancel out
      // the first negation. The first cast to unsigned is to avoid
      // undefined behavior from -i. The second is there because apparently,
      // at least on GCC, the unary - re-introduces signed and we need to
      // be unsigned to avoid sign extension. We need zero extension.
      const auto unsignedNegative =
        static_cast<Unsigned>(-static_cast<Unsigned>(i));
      return negative(Element(unsignedNegative % charac()));
    } else
      return Element(static_cast<Unsigned>(i) % charac());
  }

  T toValue(Element e) const {return e.value;}

  Element sum(const Element a, const Element b) const {
    const auto s = a.value() + b.value();
    // The sum overflowed if and only if a.value() > s. In that case
    // subtraction of charac() will overflow again in the other direction,
    // leaving us with the correct result.
    // @todo: extend precision to rule out overflow without a branch.
    if (a.value() > s || s >= charac()) {
      MATHICGB_ASSERT(s - charac() < charac());
      return Element(s - charac());
    } else
      return Element(s);
  }

  Element plusOne(const Element a) const {
    const auto s = a.value() + 1;
    if (s == charac())
      return Element(0);
    else
      return Element(s);
  }

  Element difference(const Element a, const Element b) const {
    if (a.value() < b.value()) {
      MATHICGB_ASSERT(a.value() - b.value() + charac() < charac());
      return Element(a.value() - b.value() + charac());
    } else
      return Element(a.value() - b.value());
  }

  Element negative(const Element a) const {
    if (a.value() == 0)
      return a;
    else
      return negativeNonZero(a);
  }

  Element negativeNonZero(const Element a) const {
    MATHICGB_ASSERT(!isZero(a));
    return Element(charac() - a.value());
  }

  Element product(const Element a, const Element b) const;

  /// Returns a times the inverse of b.
  Element quotient(const Element a, const Element b) const {
    return product(a, inverse(b));
  }

  /// Returns the multiplicative inverse a^-1 mod charac(). a must not be zero.
  Element inverse(const Element a) const;

private:
  const T mCharac;
};

namespace PrimeFieldInternal {
  template<class T>
  struct ModularProdType {};
  template<> struct ModularProdType<uint8> {typedef uint16 type;};
  template<> struct ModularProdType<uint16> {typedef uint32 type;};
  template<> struct ModularProdType<uint32> {typedef uint64 type;};

  template<> struct ModularProdType<int8> {typedef int16 type;};
  template<> struct ModularProdType<int16> {typedef int32 type;};
  template<> struct ModularProdType<int32> {typedef int64 type;};

  // @todo: Remove this typedef when possible. 64 bits is not enough
  // to store a 64 bit product. We need it right now because
  // coefficients are handled as 64 bit in the legacy PolyRing.
  template<> struct ModularProdType<uint64> {typedef uint64 type;};
  template<> struct ModularProdType<long unsigned int> {typedef uint64 type;};
  template<> struct ModularProdType<int64> {typedef uint64 type;};
  template<> struct ModularProdType<long int> {typedef uint64 type;};
}

template<class T>
auto PrimeField<T>::product(
  const Element a,
  const Element b
) const -> Element {
  typedef typename PrimeFieldInternal::ModularProdType<T>::type BigT;
  BigT bigProd = static_cast<BigT>(a.value()) * b.value();
  MATHICGB_ASSERT(a.value() == 0 || bigProd / a.value() == b.value());
  return Element(static_cast<T>(bigProd % charac()));
}

template<class T>
auto PrimeField<T>::inverse(const Element elementA) const -> Element {
  // We do two turns of the extended Euclidian algorithm per
  // loop. Usually the sign of x changes each time through the loop,
  // but we avoid that by representing every other x as its negative,
  // which is the value minusLastX. This way no negative values ever
  // appear and we do not need any extra bits.

  auto a = elementA.value();
  MATHICGB_ASSERT(0 < a);
  MATHICGB_ASSERT(a < charac());
#ifdef MATHICGB_DEBUG
  const auto origA = a;
#endif
  auto b = charac();
  auto minusLastX = static_cast<T>(0);
  auto x = static_cast<T>(1);
  while (true) {
    MATHICGB_ASSERT(x <= charac());
    MATHICGB_ASSERT(minusLastX <= charac());

    // first turn
    if (a == 1)
      break;
    const auto firstQuotient = b / a;
    b -= firstQuotient * a;
    minusLastX += firstQuotient * x;

    // second turn
    if (b == 1) {
      MATHICGB_ASSERT(!isZero(minusLastX));
      MATHICGB_ASSERT(minusLastX < charac());
      x = charac() - minusLastX;
      break;
    }
    const auto secondQuotient = a / b;
    a -= secondQuotient * b;
    x += secondQuotient * minusLastX;
  }
  MATHICGB_ASSERT(x >= 1);
  MATHICGB_ASSERT(x < charac());

  const  Element inverseElement(x);
  MATHICGB_ASSERT(isOne(product(elementA, inverseElement)));
  return inverseElement;
}

/// Returns true if a and b are the same object.
template<class E>
bool operator==(const PrimeField<E>& a, const PrimeField<E>& b) {
  return &a == &b;
}

/// As !(a == b).
template<class E>
bool operator!=(const PrimeField<E>& a, const PrimeField<E>& b) {
  return !(a == b);
}

template<class T>
std::ostream& operator<<(
  std::ostream& out,
  typename PrimeField<T>::Element e
) {
  out << e.value();
  return out;
}

MATHICGB_NAMESPACE_END
#endif
