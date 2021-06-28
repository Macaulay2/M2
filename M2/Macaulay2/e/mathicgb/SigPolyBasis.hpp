// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_SIG_POLY_BASIS_GUARD
#define MATHICGB_SIG_POLY_BASIS_GUARD

#include "PolyRing.hpp"
#include "Poly.hpp"
#include "MonoLookup.hpp"
#include "PolyBasis.hpp"
#include "MonoProcessor.hpp"
#include <vector>
#include <set>

MATHICGB_NAMESPACE_BEGIN

#ifndef MATHICGB_USE_RATIO_RANK
#define MATHICGB_USE_RATIO_RANK true
#endif

/// Stores a basis of polynomials that each have a signature. Designed for
/// use in signature Groebner basis algorithms.
class SigPolyBasis {
public:
  typedef PolyRing::Field Field;

  typedef PolyRing::Monoid Monoid;
  typedef Monoid::Mono Mono;
  typedef Monoid::MonoRef MonoRef;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::MonoPtr MonoPtr;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;

  typedef MonoProcessor<Monoid> Processor;

  SigPolyBasis(
    const PolyRing& R,
    int monoLookupType,
    int monTableType,
    bool preferSparseReducers
  );
  ~SigPolyBasis();

  const PolyRing& ring() const {return mBasis.ring();}
  const Monoid& monoid() const {return ring().monoid();}
  const Field& field() const {return ring().field();}

  const Poly& poly(size_t index) const {return mBasis.poly(index);}
  size_t size() const {return mBasis.size();}

  const PolyBasis& basis() const {return mBasis;}

  ConstMonoRef leadMono(size_t gen) const {
    MATHICGB_ASSERT(gen < size());
    return mBasis.leadMono(gen);
  }

  coefficient leadCoef(size_t gen) const {
    MATHICGB_ASSERT(gen < size());
    return mBasis.leadCoef(gen);
  }

  ConstMonoRef sigLeadRatio(size_t gen) const {
    MATHICGB_ASSERT(gen < size());
    return *mSigLeadRatio[gen];
  }

  // Signifies that the module has taken on another e_i.
  // Must call this before adding a polynomial to the basis with
  // a signature in the new component.
  void addComponent();

  // Takes over ownership of sig and f. sig must come from the pool
  // of ring().
  void insert(Mono sig, std::unique_ptr<Poly> f);

  ConstMonoRef signature(size_t gen) const {
    MATHICGB_ASSERT(gen < size());
    return *mSignatures[gen];
  }

  // Returns the index of a basis element that regular reduces term in
  // signature sig. Returns -1 if no such element exists. A basis element
  // u is a regular reducer if leadTerm(u) divides term
  // and (term / leadTerm(u)) * signature(u) < sig.
  size_t regularReducer(ConstMonoRef sig, ConstMonoRef term) const;

  // Uses the functionality in the divisor finder for
  // computing up to maxDivisors low ratio base divisors.
  // The divisors are placed into divisors.
  void lowBaseDivisors(
    std::vector<size_t>& divisors,
    size_t maxDivisors,
    size_t newGenerator
  ) const;

  // Uses the functionality in the divisor finder for
  // computing a high base divisor. Returns the index
  // of the divisor or -1 if none are found.
  size_t highBaseDivisor(size_t newGenerator) const;

  // Find the basis element g_i whose signature S divides sig
  // such that (S/sig)g_i has minimal leading term. Returns i.
  size_t minimalLeadInSig(ConstMonoRef sig) const;

  // Returns true if poly can be singular reduced in signature sig.
  // In other words, returns true if there is a basis element with
  // lead term M and signature S such that M divides the lead term N
  // of poly and such that N/M*S == sig. This is slow because it is
  // currently only used for asserts - implement a fast version if
  // that changes.
  bool isSingularTopReducibleSlow(const Poly& poly, ConstMonoRef sig) const;

  void display(std::ostream& out) const;
  void displayFancy(std::ostream& out, const Processor& processor) const;
  size_t getMemoryUse() const;

  // Compares the signature/lead ratio of basis element a to basis element b
  // and returns LT, EQ or GT.
  inline int ratioCompare(size_t a, size_t b) const;

  /// Post processes all signatures. This currently breaks all sorts
  /// of internal invariants - it's supposed to be a temporary hack.
  void postprocess(const Processor& processor);

  class StoredRatioCmp {
  public:
    // Stores the ratio numerator/denominator and prepares it for comparing
    // to the sig/lead ratios in basis.
    StoredRatioCmp(
      ConstMonoRef numerator,
      ConstMonoRef denominator,
      const SigPolyBasis& basis);

    // compares the stored ratio to the basis element with index be.
    inline int compare(size_t be) const;

  private:
    StoredRatioCmp(const StoredRatioCmp&); // not available
    void operator=(const StoredRatioCmp&); // not available

    const SigPolyBasis& mBasis;
    size_t mRatioRank;
    Mono mRatio;
    mutable Mono mTmp;
  };

private:
  // Slow versions use simpler code. Used to check results in debug mode.
  size_t regularReducerSlow(ConstMonoRef sig, ConstMonoRef term) const;
  size_t minimalLeadInSigSlow(ConstMonoRef sig) const;
  size_t highBaseDivisorSlow(size_t newGenerator) const;
  void lowBaseDivisorsSlow(
    std::vector<size_t>& divisors,
    size_t maxDivisors,
    size_t newGenerator
  ) const;

  friend class StoredRatioCmp;

  const MonoLookup& monoLookup() const {return mBasis.monoLookup();}

  std::unique_ptr<MonoLookup::Factory const> const mMonoLookupFactory;

  /// The ratio rank can change at each insert!
  size_t ratioRank(size_t index) const {
    MATHICGB_ASSERT(index < size());
    return mRatioRanks[index];
  }

  // Only useful for comparing to basis elements. Two ratios might get the same
  // rank without being equal. All ranks can change when a new generator
  // is added.
  size_t ratioRank(ConstMonoRef ratio) const;

  std::vector<MonoPtr> mSignatures;

  // the ratio signature/initial term including negative entries and module component
  std::vector<MonoPtr> mSigLeadRatio;

  // true if giving each generator an integer id based on its
  // position in a sorted order of sig-lead ratios.
  static const bool mUseRatioRank = MATHICGB_USE_RATIO_RANK;
  static const bool mUseStoredRatioRank = MATHICGB_USE_RATIO_RANK;

  class RatioOrder {
  public:
    RatioOrder(std::vector<MonoPtr>& ratio, const Monoid& monoid):
      mRatio(ratio), mMonoid(monoid) {}

    bool operator()(size_t a, size_t b) const {
      return mMonoid.lessThan(*mRatio[a], *mRatio[b]);
    }

  private:
    std::vector<MonoPtr>& mRatio;
    const Monoid& mMonoid;
  };

  typedef std::multiset<size_t, RatioOrder> RatioSorted;
  typedef size_t Rank;
  RatioSorted mRatioSorted;
  std::vector<Rank> mRatioRanks;

  std::vector<MonoLookup*> mSignatureLookup;

  // Contains those lead terms that are minimal.
  std::unique_ptr<MonoLookup> const mMinimalMonoLookup;

  PolyBasis mBasis;
  bool const mPreferSparseReducers;
  mutable monomial mTmp;
};

inline int SigPolyBasis::ratioCompare(size_t a, size_t b) const {
  if (mUseRatioRank) {
#ifdef MATHICGB_DEBUG
    int const value =
      monoid().compare(sigLeadRatio(a), sigLeadRatio(b));
#endif
    if (mRatioRanks[a] < mRatioRanks[b]) {
      MATHICGB_ASSERT_NO_ASSUME(value == LT);
      return LT;
    } else if (mRatioRanks[a] > mRatioRanks[b]) {
      MATHICGB_ASSERT_NO_ASSUME(value == GT);
      return GT;
    } else {
      MATHICGB_ASSERT_NO_ASSUME(value == EQ);
      return EQ;
    }
  } else {
    // A/a < B/b   <=>  A < (B/b)a

    monoid().divideToNegative(signature(b), leadMono(b), mTmp);
    monoid().multiplyInPlace(leadMono(a), mTmp);
    const auto value = monoid().compare(signature(a), mTmp);
    MATHICGB_ASSERT
      (value == monoid().compare(sigLeadRatio(a), sigLeadRatio(b)));
    return value;
  }
}

inline int SigPolyBasis::StoredRatioCmp::compare(size_t be) const {
  if (SigPolyBasis::mUseStoredRatioRank) {
#ifdef MATHICGB_DEBUG
    const auto value =
      mBasis.monoid().compare(*mRatio, mBasis.sigLeadRatio(be));
#endif
    SigPolyBasis::Rank otherRank = mBasis.ratioRank(be);
    if (mRatioRank < otherRank) {
      MATHICGB_ASSERT_NO_ASSUME(value == LT);
      return LT;
    } else if (mRatioRank > otherRank) {
      MATHICGB_ASSERT_NO_ASSUME(value == GT);
      return GT;
    } else {
      MATHICGB_ASSERT_NO_ASSUME(value == EQ);
      return EQ;
    }
  } else {
    mBasis.monoid().multiply(*mRatio, mBasis.leadMono(be), *mTmp);
    const auto value = mBasis.monoid().compare(*mTmp, mBasis.signature(be));
    MATHICGB_ASSERT
      (value == mBasis.monoid().compare(*mRatio, mBasis.sigLeadRatio(be)));
    return value;
  }
}

MATHICGB_NAMESPACE_END
#endif
