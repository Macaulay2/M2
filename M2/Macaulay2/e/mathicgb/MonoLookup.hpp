// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_MONO_LOOKUP_GUARD
#define MATHICGB_MONO_LOOKUP_GUARD

#include "PolyRing.hpp"
#include <vector>

MATHICGB_NAMESPACE_BEGIN

class PolyBasis;
class SigPolyBasis;

// Supports queries on the lead terms of the monomials in a PolyBasis.
// todo: rename to MonomialLookup.
class MonoLookup
{
public:
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;

  virtual ~MonoLookup();

  // Call after construction. Can be called multiple times, but only if the
  // parameter object is the same each time.
  virtual void setBasis(const PolyBasis& basis) = 0;

  // Call after construction. Can be called multiple times, but only if the
  // parameter object is the same each time.
  virtual void setSigBasis(const SigPolyBasis& sigBasis) = 0;

  virtual void insert(ConstMonoRef mono, size_t index) = 0;

  // Returns the index of a basis element that regular reduces mono in
  // signature sig. Returns -1 if no such element exists. A basis element
  // u is a regular reducer if leadTerm(u) divides mono
  // and (mono / leadTerm(u)) * signature(u) < sig.
  virtual size_t regularReducer(ConstMonoRef sig, ConstMonoRef mono) const = 0;

  // Returns the index of a basis element whose lead term divides mono. The
  // strategy used to break ties is up to the implementation of the interface,
  // but the outcome must be deterministic.
  virtual size_t classicReducer(ConstMonoRef mono) const = 0;

  virtual std::string getName() const = 0;

  virtual size_t getMemoryUse() const = 0;

  virtual size_t highBaseDivisor(size_t newGenerator) const = 0;
  virtual void lowBaseDivisors(
    std::vector<size_t>& divisors,
    size_t maxDivisors,
    size_t newGenerator
  ) const = 0;
  virtual size_t minimalLeadInSig(ConstMonoRef sig) const = 0;

  virtual int type() const = 0;

  /// Prints a human-readable description of the type codes for the
  /// implementations of this interface.
  static void displayCodes(std::ostream& out);

  class Factory {
  public:
    virtual std::unique_ptr<MonoLookup> make
      (bool preferSparseReducers, bool allowRemovals) const = 0;
    virtual ~Factory() {}
  };
  static std::unique_ptr<Factory> makeFactory
    (const Monoid& monoid, int type);

  class EntryOutput {
  public:
    // Stop whatever is happening if proceed returns false.
    virtual bool proceed(size_t index) = 0;
  };

  // Calls consumer.proceed(index) for each element whose lead term
  // divides mono. Stops the search if proceed returns false.
  virtual void multiples(ConstMonoRef mono, EntryOutput& consumer) const = 0;

  // Returns the index of a basis element whose lead term divides mono.
  virtual size_t divisor(ConstMonoRef mono) const = 0;

  // Calls consumer.proceed(index) for each element whose term
  // mono divides. Stops the search if proceed returns false.
  virtual void divisors(ConstMonoRef mono, EntryOutput& consumer) const = 0;

  // Removes multiples of mono. An element equal to mono counts as a multiple.
  virtual void removeMultiples(ConstMonoRef mono) = 0;

  // Removes entries whose monomial are equal to mono.
  virtual void remove(ConstMonoRef mono) = 0;

  // Returns how many elements are in the data structure.
  virtual size_t size() const = 0;
};

MATHICGB_NAMESPACE_END
#endif
