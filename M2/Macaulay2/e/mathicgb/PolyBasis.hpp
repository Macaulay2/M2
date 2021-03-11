// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_POLY_BASIS_GUARD
#define MATHICGB_POLY_BASIS_GUARD

#include "Poly.hpp"
#include "MonoLookup.hpp"
#include <vector>
#include <memory>

MATHICGB_NAMESPACE_BEGIN

class PolyRing;
class Basis;

/// Stores a basis of polynomials. Designed for use in Groebner basis
/// algorithms.
class PolyBasis {
public:
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::Mono Mono;
  typedef Monoid::MonoRef MonoRef;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::MonoPtr MonoPtr;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;

  PolyBasis(
    const PolyRing& ring,
    std::unique_ptr<MonoLookup> monoLookup
  );

  ~PolyBasis();

  /// Returns a basis of the initial ideal.
  std::unique_ptr<Basis> initialIdeal() const;

  /// Inserts a polynomial into the basis at index size() - or index size() - 1
  /// after calling, since size() will increase by one.
  /// Lead monomials must be unique among basis elements.
  void insert(std::unique_ptr<Poly> poly);

  /// Returns the index of a basis element whose lead term divides mon.
  /// Returns -1 if there is no such basis element.
  size_t divisor(ConstMonoRef mon) const;

  /// As divisor(mon), but if there is more than one divisor then the divisor
  /// is chosen according to some notion of which reducer is better.
  size_t classicReducer(ConstMonoRef mon) const;

  /// Replaces basis element at index with the given new polynomial. The lead
  /// term of the new polynomial must be the same as the previous one.
  /// This is useful for auto-tail-reduction.
  void replaceSameLeadTerm(size_t index, std::unique_ptr<Poly> newValue) {
    MATHICGB_ASSERT(index < size());
    MATHICGB_ASSERT(!retired(index));
    MATHICGB_ASSERT(newValue.get() != nullptr);
    MATHICGB_ASSERT(!newValue->isZero());
    MATHICGB_ASSERT
      (monoid().equal(leadMono(index), newValue->leadMono()));
    mMonoLookup->remove(leadMono(index));
    delete mEntries[index].poly;
    mEntries[index].poly = newValue.release();
    mMonoLookup->insert(leadMono(index), index);    
    MATHICGB_ASSERT(mEntries[index].poly != nullptr);
  }

  /// Returns the number of basis elements, including retired elements.
  size_t size() const {return mEntries.size();}

  const PolyRing& ring() const {return mRing;}
  const Monoid& monoid() const {return ring().monoid();}

  /// Returns a data structure containing the lead monomial of each basis
  /// element.
  const MonoLookup& monoLookup() const {return *mMonoLookup;}

  /// Retires the basis element at index, which returns ownership of the
  /// polynomial to the caller and frees most resources associated to
  /// that basis elements.
  std::unique_ptr<Poly> retire(size_t index);

  /// Returns an basis containing all non-retired basis elements and
  /// retires all those basis elements. The point of the simultaneous
  /// retirement is that this way no polynomials need be copied.
  std::unique_ptr<Basis> toBasisAndRetireAll();

  /// Returns true if the basis element at index has been retired.
  bool retired(size_t index) const {
    MATHICGB_ASSERT(index < size());
    return mEntries[index].retired;
  }

  /// Returns the basis element polynomial at index.
  Poly& poly(size_t index) {
    MATHICGB_ASSERT(index < size());
    MATHICGB_ASSERT(!retired(index));
    return *mEntries[index].poly;
  }

  /// Returns the basis element polynomial at index.
  const Poly& poly(size_t index) const {
    MATHICGB_ASSERT(index < size());
    MATHICGB_ASSERT(!retired(index));
    return *mEntries[index].poly;
  }

  /// Returns the lead monomial of poly(index).
  ConstMonoRef leadMono(size_t index) const {
    MATHICGB_ASSERT(index < size());
    MATHICGB_ASSERT(!retired(index));
    return poly(index).leadMono();
  }

  /// Returns the lead coefficient of poly(index).
  coefficient leadCoef(size_t index) const {
    MATHICGB_ASSERT(index < size());
    MATHICGB_ASSERT(!retired(index));
    return poly(index).leadCoef();
  }

  /// Returns true if the leading monomial of the basis element at index is not
  /// divisible by the lead monomial of any other basis element. Lead
  /// monomials are required to be unique among basis elements, so the case
  /// of several equal lead monomials does not occur.
  bool leadMinimal(size_t index) const {
    MATHICGB_ASSERT(index < size());
    MATHICGB_ASSERT(!retired(index));
    MATHICGB_SLOW_ASSERT(mEntries[index].leadMinimal == leadMinimalSlow(index));
    return mEntries[index].leadMinimal;
  }

  /// Returns true if the lead monomial of poly is not divisible by the
  /// lead monomial of any basis element. Equality counts as divisibility.
  bool leadMinimal(const Poly& poly) const {
    MATHICGB_ASSERT(&poly != 0);
    return mMonoLookup->divisor(poly.leadMono()) !=
      static_cast<size_t>(-1);
  }

  /// Returns the number of basis elements with minimal lead monomial.
  size_t minimalLeadCount() const;

  /// Returns the index of the basis element of maximal index
  /// whose lead monomial is minimal.
  size_t maxIndexMinimalLead() const;

  /// Returns the basis element polynomial at index.
  const Poly& basisElement(size_t index) const {
    MATHICGB_ASSERT(index < size());
    MATHICGB_ASSERT(!retired(index));
    return *mEntries[index].poly;
  }

  /// Returns the number of monomials across all the basis elements.
  /// Monomials that appear in more than one basis element are counted more
  /// than once.
  size_t monomialCount() const;

  /// Returns how many bytes has been allocated by this object.
  size_t getMemoryUse() const;

  
  void usedAsStart(size_t index) const {
    MATHICGB_ASSERT(index < size());
    ++mEntries[index].usedAsStartCount;
  }

  unsigned long long usedAsStartCount(size_t index) const {
    MATHICGB_ASSERT(index < size());
    return mEntries[index].usedAsStartCount;
  }
  
  void usedAsReducer(size_t index) const {
    MATHICGB_ASSERT(index < size());
    ++mEntries[index].usedAsReducerCount;
  }

  unsigned long long usedAsReducerCount(size_t index) const {
    MATHICGB_ASSERT(index < size());
    return mEntries[index].usedAsReducerCount;
  }

  void wasPossibleReducer(size_t index) const {
    MATHICGB_ASSERT(index < size());
    ++mEntries[index].possibleReducerCount;
  }

  unsigned long long wasPossibleReducerCount(size_t index) const {
    MATHICGB_ASSERT(index < size());
    return mEntries[index].possibleReducerCount;
  }

  void wasNonSignatureReducer(size_t index) const {
    MATHICGB_ASSERT(index < size());
    ++mEntries[index].nonSignatureReducerCount;
  }

  unsigned long long wasNonSignatureReducerCount(size_t index) const {
    MATHICGB_ASSERT(index < size());
    return mEntries[index].nonSignatureReducerCount;
  }
  
private:
  // Slow versions use simpler code. Used to check results in debug mode.
  bool leadMinimalSlow(size_t index) const;
  size_t divisorSlow(ConstMonoRef mon) const;

  class Entry {
  public:
    Entry();

    Poly* poly;
    bool leadMinimal;
    bool retired;

    // Statistics on reducer choice in reduction
    mutable unsigned long long usedAsStartCount;
    mutable unsigned long long usedAsReducerCount;
    mutable unsigned long long possibleReducerCount;
    mutable unsigned long long nonSignatureReducerCount;
  };
  typedef std::vector<Entry> EntryCont;
  typedef EntryCont::iterator EntryIter;
  typedef EntryCont::const_iterator EntryCIter;

  const PolyRing& mRing;
  std::unique_ptr<MonoLookup> mMonoLookup;
  std::vector<Entry> mEntries;
};

MATHICGB_NAMESPACE_END
#endif
