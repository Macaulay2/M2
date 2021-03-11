// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_REDUCER_HELPER_GUARD
#define MATHICGB_REDUCER_HELPER_GUARD

// This namespace contains functions and classees that are useful for
// writing subclasses of Reducer.

#include "PolyRing.hpp"

MATHICGB_NAMESPACE_BEGIN

namespace ReducerHelper {
  // ************** Configurations **********************

  // Common base class for the configurations offered here.
  // The configurations are designed to work with
  // mathic::TourTree, mathic::Heap and mathic::Geobucket.
  class ConfigurationBasics {
  public:
    ConfigurationBasics(const PolyRing& ring): mRing(ring) {}
    const PolyRing& ring() const {return mRing;}

    // Special fields for TourTree and Heap
    static const bool fastIndex = true;

    // Special fields for Geobuckets:
    static const size_t geoBase = 4;
    static const size_t minBucketSize = 8;

    static const bool minBucketBinarySearch = false;
    static const bool trackFront = true;
    static const bool premerge = false;
    static const bool collectMax = false;
    static const int bucketStorage = 1;
    static const size_t insertFactor = 1;

  private:
    const PolyRing& mRing;
  };

  // Base class for a configuration with deduplication turned off.
  // This cannot be a template since then all the names would be hidden
  // in any subclasses.
  class PlainConfiguration : public ConfigurationBasics {
  public:
    PlainConfiguration(const PolyRing& ring): ConfigurationBasics(ring) {}

    static const bool supportDeduplication = false;
    typedef bool CompareResult;
    bool cmpLessThan(bool r) const {return r;}

    // These last two members are not supposed to be called.
    // The dummy deduplicate function has to be a template since we do not
    // know what type Entry is.
    template<class Entry>
    Entry deduplicate(Entry a, Entry b) const {
      MATHICGB_ASSERT(false);
      return a;
    }

    bool cmpEqual(bool) const {
      MATHICGB_ASSERT(false);
      return false;
    }
  };

  // Base class for a configuration with deduplication turned on.
  // This cannot be a template since then all the names would be hidden
  // in any subclasses.
  class DedupConfiguration : public ConfigurationBasics {
  public:
    DedupConfiguration(const PolyRing& ring): ConfigurationBasics(ring) {}

    static const bool supportDeduplication = true;
    typedef int CompareResult;
    bool cmpLessThan(int r) const {return r == LT;}
    bool cmpEqual(int r) const {return r == EQ;}
  };

  // ************** Utility functions **********************

  inline const_term allocTermCopy(const PolyRing& ring, const_term term) {
    monomial mono = ring.allocMonomial();
    ring.monomialCopy(term.monom, mono);
    return const_term(term.coeff, mono);
  }
}

MATHICGB_NAMESPACE_END
#endif
