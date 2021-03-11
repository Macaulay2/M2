// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_SIG_S_PAIRS_GUARD
#define MATHICGB_SIG_S_PAIRS_GUARD

#include "PolyRing.hpp"
#include "KoszulQueue.hpp"
#include "SigSPairQueue.hpp"
#include "mathic/mathic.h"
#include "memtailor/memtailor.h"
#include <vector>

MATHICGB_NAMESPACE_BEGIN

class Poly;
class ModuleMonoSet;
class SigPolyBasis;
class Reducer;

// Handles S-pairs in signature Grobner basis algorithms. Responsible
// for eliminating S-pairs, storing S-pairs and ordering S-pairs.
class SigSPairs
{
public:
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::Mono Mono;
  typedef Monoid::MonoRef MonoRef;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::MonoPtr MonoPtr;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;

  SigSPairs(
    const PolyRing *R0,
    const SigPolyBasis *GB0,
    ModuleMonoSet *Hsyz0,
    Reducer* reducer,
    bool postponeKoszuls,
    bool useBaseDivisors,
    bool useSingularCriterionEarly,
    size_t queueType);
  ~SigSPairs();

  typedef std::vector<std::pair<size_t, size_t> > PairContainer;
  Mono popSignature(PairContainer& pairs);

  // fills in all the S-pairs with i.
  void newPairs(size_t i);

  struct Stats {
    unsigned long long spairsConstructed; // all spairs
    unsigned long long spairsFinal; // spairs given to client
    unsigned long long nonregularSPairs; // spairs eliminated by being non-regular
    unsigned long long highBaseDivisorHits; // spairs eliminated by high base divisor
    unsigned long long lowBaseDivisorHits; // spairs eliminated by low base divisor
    unsigned long long hasHighBaseDivisor; // generators that have a high base divisor
    unsigned long long hasLowBaseDivisor; // generators that have a low base divisor
    unsigned long long syzygyModuleHits; // spairs eliminated by syzygy module
    unsigned long long earlyRelativelyPrimePairs;
    unsigned long long earlySingularCriterionPairs;
    unsigned long long queuedPairs; // number actually placed on spair triangle
    unsigned long long duplicateSignatures; // number of spairs removed due to duplicate signature

    Stats():
      spairsConstructed(0),
      spairsFinal(0),
      nonregularSPairs(0),
      highBaseDivisorHits(0),
      lowBaseDivisorHits(0),
      hasHighBaseDivisor(0),
      hasLowBaseDivisor(0),
      syzygyModuleHits(0),
      earlyRelativelyPrimePairs(0),
      earlySingularCriterionPairs(0),
      queuedPairs(0),
      duplicateSignatures(0) {}
  };
  Stats getStats() const {return mStats;}

  size_t pairCount() const;

  size_t getMemoryUse() const;
  size_t getKnownSyzygyBitsMemoryUse() const;

  // Informs the s-pair handler that the syzygy between gen1 and gen2
  // is a known syzygy.
  void setKnownSyzygy(size_t gen1, size_t gen2);
  void setKnownSyzygies(std::vector<std::pair<size_t, size_t> >& pairs);

  const PolyRing& ring() {return *R;}
  const Monoid& monoid() {return ring().monoid();}

  std::string name();

private:
  void makePreSPairs(size_t newGen);

  struct BaseDivisor { // a low ratio base divisor
    size_t baseDivisor; // the index of the generator that is the base divisor
    size_t ratioLessThan; // consider generators with ratio less than this
    monomial baseMonomial; // the monomial that has to divide to get a hit
  };
  typedef std::vector<BaseDivisor> BaseDivContainer;
  void setupBaseDivisors(
    BaseDivisor& divisor1,
    BaseDivisor& divisor2,
    size_t& highDivisorCmp,
    size_t newGenerator);
  
  const PolyRing *R;

  // if true, apply the early singular criterion
  bool const mUseSingularCriterionEarly;

  // true if low ratio base divisors are used to speed up S-pair elimination.
  const bool mUseBaseDivisors;

  // True if high ratio base divisors are used to speed up S-pair elimination.
  // The syzygy should have already been inserted into the syzygy module.
  const bool mUseHighBaseDivisors;

  // one entry for every s-pair, which is set to true if the
  // s-pair is known to be a syzygy. Only used if
  // mUseBaseDivisors is true.
  mathic::BitTriangle mKnownSyzygyTri;

  // From elsewhere
  ModuleMonoSet *Hsyz; // we often modify this
  const SigPolyBasis *GB;
  //  Reducer* mReducer;
  const bool mPostponeKoszuls;

  typedef std::vector<PreSPair> PrePairContainer;

  std::unique_ptr<SigSPairQueue> mQueue;
  SigSPairQueue::IndexSigs mIndexSigs;

  mutable Stats mStats;
};

MATHICGB_NAMESPACE_END
#endif
