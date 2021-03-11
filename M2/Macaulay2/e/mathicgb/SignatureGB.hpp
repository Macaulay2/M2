// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_SIGNATURE_G_B_GUARD
#define MATHICGB_SIGNATURE_G_B_GUARD

#include "PolyRing.hpp"
#include "ModuleMonoSet.hpp"
#include "SigPolyBasis.hpp"
#include "SigSPairs.hpp"
#include "Reducer.hpp"
#include "KoszulQueue.hpp"
#include "SPairs.hpp"
#include "MonoProcessor.hpp"
#include <map>

MATHICGB_NAMESPACE_BEGIN

class SigSPairs;

class SignatureGB {
public:
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::Mono Mono;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;
  typedef Monoid::MonoVector MonoVector;
  typedef MonoProcessor<Monoid> Processor;
  typedef Monoid::Component Component;

  SignatureGB(
    Basis&& basis,
    Processor&& processor,
    Reducer::ReducerType reductiontyp,
    int divlookup_type,
    int montable_type,
    bool postponeKoszul,
    bool useBaseDivisors,
    bool preferSparseReducers,
    bool useSingularCriterionEarly,
    size_t queueType);

  void computeGrobnerBasis();

  SigPolyBasis* getGB() { return GB.get(); }
  ModuleMonoSet* getSyzTable() { return mProcessor->processingNeeded() ? Hsyz2.get() : Hsyz.get(); }
  SigSPairs* getSigSPairs() { return SP.get(); }

  size_t getMemoryUse() const;
  void displayStats(std::ostream& out) const;
  void displayPaperStats(std::ostream& out) const;
  void displayMemoryUse(std::ostream& out) const;
  void displaySomeStats(std::ostream& out) const;

  void setBreakAfter(unsigned int elements) {
    mBreakAfter = elements;
  }

  void setPrintInterval(unsigned int reductions) {
    mPrintInterval = reductions;
  }

  const Monoid& monoid() const {return R->monoid();}

private:
  unsigned int mBreakAfter;
  unsigned int mPrintInterval;




  bool processSPair(Mono sig, const SigSPairs::PairContainer& pairs);
  bool step();

  const PolyRing *R;

  bool const mPostponeKoszul;

  // Currently we use either both criteria (high and loow) or neither.
  bool const mUseBaseDivisors;

  SigSPairs::PairContainer mSpairTmp; // use only for getting S-pairs

  // stats //////////
  size_t stats_sPairSignaturesDone; // distinct S-pair signatures done
  size_t stats_sPairsDone; // total S-pairs done
  size_t stats_koszulEliminated; // S-pairs eliminated due to Koszul queue
  size_t stats_SignatureCriterionLate; // # spairs removed due to being a syz signature

  // S-pairs eliminated due to relatively prime criterion
  size_t stats_relativelyPrimeEliminated;

  size_t stats_pairsReduced; // # spairs actually sent for reduction

  mic::Timer mTimer;
  double stats_nsecs;

  std::unique_ptr<SigPolyBasis> GB;
  KoszulQueue mKoszuls;
  std::unique_ptr<ModuleMonoSet> Hsyz;
  std::unique_ptr<ModuleMonoSet> Hsyz2;
  std::unique_ptr<Reducer> reducer;
  std::unique_ptr<SigSPairs> SP;
  std::unique_ptr<MonoProcessor<Monoid>> mProcessor;
};

MATHICGB_NAMESPACE_END
#endif
