// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_POLY_GUARD
#define MATHICGB_POLY_GUARD

#include "PolyRing.hpp"
#include "Range.hpp"
#include <vector>
#include <ostream>
#include <utility>
#include <cstdio>
#include <iterator>

MATHICGB_NAMESPACE_BEGIN

/// Stores a polynomial.
class Poly {
public:
  typedef PolyRing::Field Field;
  typedef Field::Element Coef;
  typedef Field::ConstElementRef ConstCoefRef;

  typedef PolyRing::Monoid Monoid;
  typedef Monoid::Mono Mono;
  typedef Monoid::MonoRef MonoRef;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::MonoPtr MonoPtr;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;

  /// Constructs the zero polynomial in the given ring.
  Poly(const PolyRing& ring): mRing(ring), mMonos(ring.monoid()) {}

  Poly(const Poly& poly):
    mRing(poly.ring()), mCoefs(poly.mCoefs), mMonos(poly.mMonos)
  {}

  Poly(const Poly&& poly):
    mRing(poly.ring()),
    mCoefs(std::move(poly.mCoefs)),
    mMonos(std::move(poly.mMonos))
  {}

  const PolyRing& ring() const {return mRing;}
  const Monoid& monoid() const {return ring().monoid();}
  const Field& field() const {return ring().field();}

  bool isZero() const {return mCoefs.empty();}
  size_t termCount() const {return mCoefs.size();}
  size_t getMemoryUse() const {
    return sizeof(mCoefs.front()) * mCoefs.capacity() +
      mMonos.memoryBytesUsed();
  }

  /// Returns a polynomial whose terms have been permuted to be in
  /// descending order.
  ///
  /// Making the copy is not wasteful, because doing the permutation in-place
  /// would be require swapping monomials which is slow if they are large.
  /// The returned object is not copy (return value optimization) and using
  /// move assignment this code will only create the single copy of a Poly
  /// p that is necessary to avoid an in-place operation:
  ///
  ///   p = p.polyWithTermsDescending()
  Poly polyWithTermsDescending() {
    // *** Sort terms in descending order of monomial.
    // It would be possible but cumbersome to implement a sort directly
    // on mMonos. That way no allocation would need to happen, however
    // it is not clear that that would be any faster, since swapping around
    // monomials in-place is slow. Swapping terms is faster, since terms
    // just refer to the monomials. This way is also easier to implement.
    //
    /// @todo: make a separate TermSorter object that allows the temporary
    /// vector to be reused between sorts. This should matter for sorting input
    /// ideals where there might be a lot of polynomials to go through.
    auto greaterOrEqual = [&](const NewConstTerm& a, const NewConstTerm& b) {
      return monoid().lessThan(*b.mono, *a.mono);
    };
    auto terms = rangeToVector(*this);
    std::sort(std::begin(terms), std::end(terms), greaterOrEqual);

    // *** Make a new polynomial with terms in that order
    Poly poly(ring());
    poly.reserve(termCount());
    poly.append(terms);

    MATHICGB_ASSERT(poly.termsAreInDescendingOrder());
    MATHICGB_ASSERT(poly.termCount() == termCount());
    return poly;
  }

  /// Appends the given term as the last term in the polynomial.
  void append(ConstCoefRef coef, ConstMonoRef mono) {
    mCoefs.push_back(coef);
    mMonos.push_back(mono);
  }

  /// Appends the given term as the last term in the polynomial.
  void append(const NewConstTerm& term) {
    MATHICGB_ASSERT(term.mono != nullptr);
    append(term.coef, *term.mono);
  }

  /// Appends each term in the range r to the end of the polynomial.
  template<class Range>
  void append(const Range& r) {
    for (const auto& term : r)
      append(term);
  }

  /// As append(range(termsBegin, termsEnd))
  template<class ForwardIterator>
  void append(
    const ForwardIterator& termsBegin,
    const ForwardIterator& termsEnd
  ) {
    append(range(termsBegin, termsEnd));
  }

  /// Hint that space for the give number of terms is going to be needed.
  /// This serves the same purpose as std::vector<>::reserve.
  void reserve(size_t spaceForThisManyTerms) {
    mMonos.reserve(spaceForThisManyTerms * monoid().entryCount());
  }

  /// Makes the polynomial monic by multiplying by the multiplicative inverse
  /// of leadCoef(). Calling this method is an error if isZero().
  void makeMonic() {
    MATHICGB_ASSERT(!isZero());
    if (isMonic())
      return;
    auto multiplier = field().inverse(leadCoef());
    for (auto& coef : mCoefs)
      coef = field().product(coef, multiplier);
    MATHICGB_ASSERT(isMonic());
  }

  void setToZero() {
    mCoefs.clear();
    mMonos.clear();
  }

  Poly& operator=(const Poly& poly) {return *this = Poly(poly);}
  Poly& operator=(Poly&& poly) {
    MATHICGB_ASSERT(&ring() == &poly.ring());
    mCoefs = std::move(poly.mCoefs);
    mMonos = std::move(poly.mMonos);
    return *this;
  }


  // *** Accessing the coefficients of the terms in the polynomial.

  /// Returns the coefficient of the leading term.
  const Coef& leadCoef() const {
    MATHICGB_ASSERT(!isZero());
    return mCoefs.front();
  }

  /// Returns true if the polynomial is monic. A polynomial is monic if
  /// the coefficient of the leading monomial is 1. If you are asking this
  /// question about a polynomial, that likely means that you are expecting
  /// the polynomial not to be zero. So it is an error to ask if the zero
  /// polynomial is monic - you'll get an assert to help pinpoint the error.
  bool isMonic() const {
    MATHICGB_ASSERT(!isZero());
    return field().isOne(leadCoef());
  }

  typedef Field::ElementVector CoefVector;
  typedef CoefVector::const_iterator ConstCoefIterator;
  typedef Range<ConstCoefIterator> ConstCoefIteratorRange;

  ConstCoefIterator coefBegin() const {return mCoefs.begin();}
  ConstCoefIterator coefEnd() const {return mCoefs.end();}
  ConstCoefIteratorRange coefRange() const {
    return range(coefBegin(), coefEnd());
  }


  // *** Accessing the monomials of the terms in the polynomial

  /// Returns the monomial of the leading term.
  ConstMonoRef leadMono() const {
    MATHICGB_ASSERT(!isZero());
    return mMonos.front();
  }

  /// Returns the monomial of the last term.
  ConstMonoRef backMono() const {
    MATHICGB_ASSERT(!isZero());
    return mMonos.back();
  }

  /// Returns true if the terms are in descending order. The terms are in
  /// descending order when mono(0) >= mono(1) >= ... >= backMono.
  /// The coefficient of the terms are not considered in this comparison.
  bool termsAreInDescendingOrder() const {
    auto greaterThanOrEqual = [&](ConstMonoRef a, ConstMonoRef b) {
      return !monoid().lessThan(a, b);
    };
    return std::is_sorted(monoBegin(), monoEnd(), greaterThanOrEqual);
  }

  typedef Monoid::MonoVector MonoVector;
  typedef MonoVector::const_iterator ConstMonoIterator;
  typedef Range<ConstMonoIterator> ConstMonoIteratorRange;

  ConstMonoIterator monoBegin() const {return mMonos.begin();}
  ConstMonoIterator monoEnd() const {return mMonos.end();}
  ConstMonoIteratorRange monoRange() const {
    return range(monoBegin(), monoEnd());
  }


  // *** Iteration through terms

  class ConstTermIterator {
  public:
    typedef std::forward_iterator_tag iterator_category;
    typedef NewConstTerm value_type;
    typedef ptrdiff_t difference_type;
    typedef value_type* pointer;
    typedef value_type& reference;

    ConstTermIterator() {}

    ConstTermIterator& operator++() {
      ++mIt;
      return *this;
    }

    value_type operator*() const {
      auto pair = *mIt;
      NewConstTerm term = {pair.first, pair.second};
      return term;
    }

    bool operator==(const ConstTermIterator& it) const {return mIt == it.mIt;}
    bool operator!=(const ConstTermIterator& it) const {return mIt != it.mIt;}

    const Coef& coef() const {return (*mIt).first;}
    ConstMonoRef mono() const {return (*mIt).second;}

  private:
    friend class Poly;
    typedef Zip<ConstCoefIterator, ConstMonoIterator> Iterator;
    ConstTermIterator(const Iterator& it): mIt(it) {}

    Iterator mIt;
  };

  typedef Range<ConstTermIterator> ConstTermIteratorRange;

  ConstTermIterator begin() const {return makeZip(coefBegin(), monoBegin());}
  ConstTermIterator end() const {return makeZip(coefEnd(), monoEnd());}
  ConstTermIteratorRange termRange() const {return range(begin(), end());}

  NewConstTerm leadTerm() const {
    MATHICGB_ASSERT(!isZero());
    NewConstTerm term = {leadCoef(), leadMono().ptr()};
    return term;
  }

private:
  friend bool operator==(const Poly &a, const Poly &b);

  const PolyRing& mRing;
  CoefVector mCoefs;
  MonoVector mMonos;
};

inline bool operator==(const Poly& a, const Poly& b) {
  MATHICGB_ASSERT(a.ring() == b.ring());
  return a.mCoefs == b.mCoefs && a.mMonos == b.mMonos;
}

MATHICGB_NAMESPACE_END
#endif
