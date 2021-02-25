// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_S_PAIRS_GUARD
#define MATHICGB_S_PAIRS_GUARD

#include "PolyBasis.hpp"
#include <utility>
#include "mathic/mathic.h"
#include <memory>

MATHICGB_NAMESPACE_BEGIN

class PolyBasis;

// Stores the set of pending S-pairs for use in the classic Buchberger
// algorithm. Also eliminates useless S-pairs and orders the S-pairs.
class SPairs {
public:
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::Exponent Exponent;
  typedef Monoid::Mono Mono;
  typedef Monoid::MonoPtr MonoPtr;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;
  typedef Monoid::MonoRef MonoRef;
  typedef Monoid::ConstMonoRef ConstMonoRef;

  /// This monoid is used for computations to determine whether to eliminate
  /// an S-pair. These computations do not require anything beyond just
  /// considering the exponents. Since recomputing characteristics of
  /// lcms such as hash and degree is expensive (unlike for a product), it is
  /// worthwhile to disable those characteristics that we do not need.
  typedef MonoMonoid<Exponent, true, false, false> BareMonoid;

  /// This monoid is used to order S-pairs by their lcm. Here we need to
  /// to store the ordering data for fast comparison, but we do not need
  /// hashes.
  typedef MonoMonoid<Exponent, true, false, true> OrderMonoid;

  SPairs(const PolyBasis& basis, bool preferSparseSPairs);

  // Returns the number of S-pairs in the data structure.
  size_t pairCount() const {return mQueue.pairCount();}

  // Returns true if no pending S-pairs remain.
  bool empty() const {return mQueue.empty();}

  // Removes the minimal S-pair from the data structure and returns it.
  // The S-polynomial of that pair is assumed to reduce to zero, either
  // because it already does, or because it did not reduce to zero and then
  // that caused the addition of another basis element. If this assumption
  // is broken, S-pair elimination may give incorrect results.
  //
  // Returns the pair (invalid,invalid) if there are no S-pairs left to
  // return, where invalid is static_cast<size_t>(-1). This can happen even
  // if empty() returned false prior to calling pop(), since the S-pairs in
  // the queue may have been found to be useless.
  std::pair<size_t, size_t> pop();

  // As pop(), but only pops S-pairs whose lcm have the passed-in
  // weight. If deg is already 0, then instead set deg to the weight
  // of the returned S-pair, if any.
  std::pair<size_t, size_t> pop(exponent& w);

  // Add the pairs (index,a) to the data structure for those a such that
  // a < index. Some of those pairs may be eliminated if they can be proven
  // to be useless. index must be a valid index of a basis element
  // and must only be called once per basis element and must be 1 more
  // than the value of index for the previous call to addPairs, starting
  // at zero for the first call.
  void addPairs(size_t index);

  // As addPairs, but assuming auto-reduction of the basis will happen.
  // This method assumes that if lead(index) divides lead(x) for a basis
  // element x, then x will be retired from the basis and reduced. toReduce
  // will contain those indices x.
  void addPairsAssumeAutoReduce(size_t index, std::vector<size_t>& toRetireAndReduce);

  // Returns true if the S-pair (a,b) is known to be useless. Even if the
  // S-pair is not useless now, it will become so later. At the latest, an
  // S-pair becomes useless when its S-polynomial has been reduced to zero.
  bool eliminated(size_t a, size_t b) const {
    return mEliminated.bitUnordered(a, b);
  }

  const Monoid& monoid() const {return mMonoid;}
  const PolyBasis& basis() const {return mBasis;}

  size_t getMemoryUse() const;

  struct Stats {
    Stats():
      sPairsConsidered(0),
      relativelyPrimeHits(0),
      buchbergerLcmSimpleHits(0),
      buchbergerLcmAdvancedHits(0),
      buchbergerLcmCacheHits(0),
      late(false),
      buchbergerLcmSimpleHitsLate(0),
      buchbergerLcmCacheHitsLate(0)
    {}

    unsigned long long sPairsConsidered;
    unsigned long long relativelyPrimeHits;
    unsigned long long buchbergerLcmSimpleHits;
    unsigned long long buchbergerLcmAdvancedHits;
    unsigned long long buchbergerLcmCacheHits;
    bool late;  // if set to true then simpleBuchbergerLcmCriterion sets the following 2 instead:
    unsigned long long buchbergerLcmSimpleHitsLate;
    unsigned long long buchbergerLcmCacheHitsLate;
  };
  Stats stats() const;

  std::string name() const;

private:
  const BareMonoid& bareMonoid() const {return mBareMonoid;}
  const OrderMonoid& orderMonoid() const {return mOrderMonoid;}

  // Returns true if Buchberger's second criterion for eliminating useless
  // S-pairs applies to the pair (a,b). Define
  //   l(a,b) = lcm(lead(a), lead(b)).
  // The criterion says that if there is some other basis element c such that
  //   lead(c)|l(a,b)
  // and
  //   l(a,c) has a representation and
  //   l(b,c) has a representation
  // then (a,b) has a representation too, so we do not need to reduce it.
  //
  // This criterion is easy to get wrong in cases where
  //   l(a,b) = l(a,c) = l(b,c)
  // since then there is the potential to erroneously eliminate all the three
  // pairs among a,b,c on the assumption that the other two pairs will reduce
  // to zero. In fact only one of the pairs should be eliminated. We leave such
  // cases to the advanced criterion, except if an S-pair has already been
  // eliminated - in that case we do not check to see if the lcm's are the same
  // as it is not necessary to do so.
  bool simpleBuchbergerLcmCriterion(
    size_t a,
    size_t b,
    BareMonoid::ConstMonoRef lcmAB
  ) const;

  // As the non-slow version, but uses simpler and slower code.
  bool simpleBuchbergerLcmCriterionSlow(size_t a, size_t b) const;

  // Improves on Buchberger's second criterion by using connection in a graph
  // to determine if an S-pair can be eliminated. This can eliminate some pairs
  // that cannot be eliminated by looking at any one triple of generators.
  //
  // The algorithm is based on considering an undirected graph G.
  // Each vertex of G represents a basis element whose lead monomial divides
  // lcmAB. There is an edge (c,d) if lcm(c,d) != lcm(a,b) or if (c,d) has
  // been eliminated. It is a theorem that if there is a path from a to b
  // in G then (a,b) is a useless S-pair that can be eliminated.
  bool advancedBuchbergerLcmCriterion
    (size_t a, size_t b, BareMonoid::ConstMonoRef lcmAB) const;

  // As the non-slow version, but uses simpler and slower code.
  bool advancedBuchbergerLcmCriterionSlow(size_t a, size_t b) const;

  const Monoid& mMonoid;
  OrderMonoid mOrderMonoid;
  BareMonoid mBareMonoid;

  class QueueConfiguration {
  public:
    QueueConfiguration(
      const PolyBasis& basis,
      const OrderMonoid& orderMonoid,
      const bool preferSparseSPairs
    ):
      mBasis(basis),
      mMonoid(basis.ring().monoid()),
      mOrderMonoid(orderMonoid),
      mPreferSparseSPairs(preferSparseSPairs) {}

    typedef OrderMonoid::Mono PairData;
    void computePairData
    (size_t col, size_t row, OrderMonoid::MonoRef m) const;

    typedef bool CompareResult;
    bool compare(
      size_t colA, size_t rowA, OrderMonoid::ConstMonoPtr a,
      size_t colB, size_t rowB, OrderMonoid::ConstMonoPtr b
    ) const {
      const auto cmp = orderMonoid().compare(*a, *b);
      if (cmp == GT)
        return true;
      if (cmp == LT)
        return false;
      
      const bool aRetired = mBasis.retired(rowA) || mBasis.retired(colA);
      const bool bRetired = mBasis.retired(rowB) || mBasis.retired(colB);
      if (aRetired || bRetired)
        return !bRetired;
      
      if (mPreferSparseSPairs) {
        const auto termCountA =
          mBasis.basisElement(colA).termCount() +
          mBasis.basisElement(rowA).termCount();
        const auto termCountB =
          mBasis.basisElement(colB).termCount() +
          mBasis.basisElement(rowB).termCount();
        if (termCountA > termCountB)
          return true;
        if (termCountA < termCountB)
          return false;
      }
      return colA + rowA > colB + rowB;
    }
    bool cmpLessThan(bool v) const {return v;}

    // The following methods are not required of a configuration.
	OrderMonoid::Mono allocPairData() {return orderMonoid().alloc();}
	void freePairData(OrderMonoid::Mono&& mono) {
      return orderMonoid().free(std::move(mono));
    }

  private:
    const Monoid& monoid() const {return mMonoid;}
    const OrderMonoid& orderMonoid() const {return mOrderMonoid;}

	const PolyBasis& mBasis;
    const Monoid& mMonoid;
    const OrderMonoid& mOrderMonoid;
    const bool mPreferSparseSPairs;
  };
  typedef mathic::PairQueue<QueueConfiguration> Queue;
  Queue mQueue;

  // The bit at (i,j) is set to true if it is known that the S-pair between
  // basis element i and j does not have to be reduced. This can be due to a
  // useless S-pair criterion eliminating that pair, or it can be because the
  // S-polynomial of that pair has already been reduced.
  mathic::BitTriangle mEliminated;
  const PolyBasis& mBasis;
  mutable Stats mStats;

  static const bool mUseBuchbergerLcmHitCache = true;
  mutable std::vector<size_t> mBuchbergerLcmHitCache;

  enum Connection { // used in advancedBuchbergerLcmCriterion().
    NotConnected, // not known to be connected to a or b
    ConnectedA, // connected to a
    ConnectedB // connected to b
  };
  // Variable used only inside advancedBuchbergerLcmCriterion().
  mutable std::vector<std::pair<size_t, Connection> >
    mAdvancedBuchbergerLcmCriterionGraph;

  friend void mathic::PairQueueNamespace::constructPairData<QueueConfiguration>
  (void*, mathic::PairQueueNamespace::Index, mathic::PairQueueNamespace::Index, QueueConfiguration&);
  friend void mathic::PairQueueNamespace::destructPairData<QueueConfiguration>
  (OrderMonoid::Mono*, mathic::PairQueueNamespace::Index, mathic::PairQueueNamespace::Index, QueueConfiguration&);
};

MATHICGB_NAMESPACE_END

namespace mathic {
  namespace PairQueueNamespace {
    template<>
    inline void constructPairData<mgb::SPairs::QueueConfiguration>(
      void* memory,
      const Index col,
      const Index row,
      mgb::SPairs::QueueConfiguration& conf
    ) {
      MATHICGB_ASSERT(memory != 0);
      MATHICGB_ASSERT(col > row);
      auto pd = new (memory)
        mgb::SPairs::OrderMonoid::Mono(conf.allocPairData());
      conf.computePairData(col, row, *pd);
    }
    
    template<>
    inline void destructPairData(
      mgb::SPairs::OrderMonoid::Mono* pd,
      const Index col,
      const Index row,
      mgb::SPairs::QueueConfiguration& conf
    ) {
      MATHICGB_ASSERT(pd != 0);
      MATHICGB_ASSERT(col > row);
      conf.freePairData(std::move(*pd));
    }	
  }
}

#endif
