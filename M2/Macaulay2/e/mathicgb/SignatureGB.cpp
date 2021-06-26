// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "SignatureGB.hpp"

#include "Basis.hpp"
#include "MonoLookup.hpp"
#include "SigSPairs.hpp"
#include "ModuleMonoSet.hpp"
#include "LogDomain.hpp"
#include "mathic/mathic.h"
#include <limits>

MATHICGB_DEFINE_LOG_DOMAIN(
  SigBasisChanged,
  "Show the signature basis after each change."
);

MATHICGB_DEFINE_LOG_DOMAIN(
  SigSPairFinal,
  "Show the outcome of final processing for S-pairs."
);

MATHICGB_NAMESPACE_BEGIN

int tracingLevel = 0;

SignatureGB::SignatureGB(
  Basis&& basis,
  Processor&& processor,
  Reducer::ReducerType reductiontyp,
  int divlookup_type,
  int montable_type,
  bool postponeKoszul,
  bool useBaseDivisors,
  bool preferSparseReducers,
  bool useSingularCriterionEarly,
  size_t queueType
):
  mBreakAfter(0),
  mPrintInterval(0),
  R(basis.getPolyRing()),
  mPostponeKoszul(postponeKoszul),
  mUseBaseDivisors(useBaseDivisors),
  stats_sPairSignaturesDone(0),
  stats_sPairsDone(0),
  stats_koszulEliminated(0),
  stats_SignatureCriterionLate(0),
  stats_relativelyPrimeEliminated(0),
  stats_pairsReduced(0),
  stats_nsecs(0.0),
  GB(make_unique<SigPolyBasis>(*R, divlookup_type, montable_type, preferSparseReducers)),
  mKoszuls(R->monoid()),
  Hsyz(ModuleMonoSet::make(R->monoid(), montable_type, basis.size(), !mPostponeKoszul)),
  Hsyz2(ModuleMonoSet::make(R->monoid(), montable_type, basis.size(), !mPostponeKoszul)),
  reducer(Reducer::makeReducer(reductiontyp, *R)),
  SP(make_unique<SigSPairs>(R, GB.get(), Hsyz.get(), reducer.get(), mPostponeKoszul, mUseBaseDivisors, useSingularCriterionEarly, queueType))
{
  mProcessor = make_unique<MonoProcessor<Monoid>>(std::move(processor));
  if (basis.size() > std::numeric_limits<Component>::max())
    mathic::reportError("Dimension of module too large.");
  const auto componentCount = Component(basis.size());
  mProcessor->setComponentCount(componentCount);

  // Populate GB
  for (Component j = 0; j < componentCount; j++)
    GB->addComponent();

  for (Component i = 0; i < componentCount; i++) {
    auto g = make_unique<Poly>(*basis.getPoly(i));
    g->makeMonic();

    auto sig = monoid().alloc();
    monoid().setIdentity(*sig);
    monoid().setComponent(i, *sig);
    mProcessor->preprocess(*sig);

    GB->insert(std::move(sig), std::move(g));
  }

  // Populate SP
  for (size_t i = 0; i < basis.size(); i++)
    SP->newPairs(i);
}

void SignatureGB::computeGrobnerBasis()
{
  size_t counter = 0;

  mTimer.reset();
  std::ostream& out = std::cout;

  while (step()) {
    if (mBreakAfter > 0 && GB->size() > mBreakAfter) {
      break;
      const size_t pairs = SP->pairCount();
      size_t sigs = 0;
      size_t syzygySigs = 0;
      while (true) {
        auto sig = SP->popSignature(mSpairTmp);
        if (sig.isNull())
          break;
        ++sigs;
        if (Hsyz->member(*sig))
          ++syzygySigs;
        else
          GB->minimalLeadInSig(*sig);
      }
      const double syzygyRatio = static_cast<double>(syzygySigs) / sigs;
      std::cerr << "*** Early exit statistics ***\n"
       << "remaining spairs: " << pairs << '\n'
       << "remaining spair signatures: " << sigs << '\n'
       << "spair signature syzygies: " << syzygySigs
       << " (" << syzygyRatio * 100 << "% of sigs)\n";
      break;
    }
    if (mPrintInterval == 0 || (++counter % mPrintInterval) != 0)
      continue;
    out << "\n-------------------------------------------------------------\n";
    displayMemoryUse(out);
    out << "\n";
    displaySomeStats(out);
  }
  //  exit(1);
  /*displayMemoryUse(std::cout);
  std::cout << "\n";
  displaySomeStats(std::cout);*/

  //  displayMemoryUse(std::cout);
  stats_nsecs = mTimer.getMilliseconds() / 1000.0;
  //GB->displayBrief(out);

  if (mProcessor->processingNeeded()) {
    GB->postprocess(*mProcessor);
    Hsyz->forAll([&](ConstMonoRef mono) {
      auto sig = R->allocMonomial();
      R->monoid().copy(mono, sig);
      mProcessor->postprocess(sig);
      Hsyz2->insert(sig);
    });
  }
}

bool SignatureGB::processSPair
  (Mono sig, const SigSPairs::PairContainer& pairs)
{
  MATHICGB_ASSERT(!pairs.empty());

  // the module term to reduce is multiple * GB->getSignature(gen)
  size_t gen = GB->minimalLeadInSig(*sig);
  MATHICGB_ASSERT(gen != static_cast<size_t>(-1));
  monomial multiple = R->allocMonomial();
  monoid().divide(GB->signature(gen), *sig, multiple);
  GB->basis().usedAsStart(gen);

  // reduce multiple * GB->getSignature(gen)
  auto f = reducer->regularReduce(*sig, multiple, gen, *GB);

  R->freeMonomial(multiple);

  if (f == nullptr) { // singular reduction
    MATHICGB_LOG(SigSPairFinal) << "   eliminated by singular criterion.\n";
    return true;
  }

  if (f->isZero()) { // reduction to zero
    // todo: what are the correct ownership relations here?
    auto ptr = sig.release();
    Hsyz->insert(*ptr);
    SP->setKnownSyzygies(mSpairTmp);
    MATHICGB_LOG(SigSPairFinal) << "   s-reduced to zero.\n";
    return false;
  }

  // new basis element
  MATHICGB_ASSERT(!GB->isSingularTopReducibleSlow(*f, *sig));

  GB->insert(std::move(sig), std::move(f));

  MATHICGB_LOG(SigSPairFinal) << "   s-reduced to new basis element.\n";

  MATHICGB_IF_STREAM_LOG(SigBasisChanged) {
    stream << "New signature basis element. The basis is now:\n";
    GB->displayFancy(stream, *mProcessor);
  };

  SP->newPairs(GB->size()-1);

  return true;
}

bool SignatureGB::step() {
  auto sig = SP->popSignature(mSpairTmp);
  if (sig.isNull())
    return false;
  ++stats_sPairSignaturesDone;
  stats_sPairsDone += mSpairTmp.size();

  MATHICGB_IF_STREAM_LOG(SigSPairFinal) {
    stream << "Final processing of signature ";
    R->monomialDisplay(stream, Monoid::toOld(*sig));
    stream << '\n';
  };

  if (Hsyz->member(*sig)) {
    ++stats_SignatureCriterionLate;
    SP->setKnownSyzygies(mSpairTmp);
    MATHICGB_LOG(SigSPairFinal) << "   eliminated by signature criterion.\n";
    return true;
  }

  while (!mKoszuls.empty() && R->monoid().lessThan(mKoszuls.top(), *sig))
    mKoszuls.pop();
  
  if (!mKoszuls.empty() && R->monoid().equal(mKoszuls.top(), *sig)) {
    ++stats_koszulEliminated;
    // This signature is of a syzygy that is not in Hsyz, so add it
    // todo: what are the correct ownership relations here?
    auto ptr = sig.release();
    Hsyz->insert(*ptr);
    SP->setKnownSyzygies(mSpairTmp);
    MATHICGB_LOG(SigSPairFinal) << "   eliminated by Koszul criterion.\n";
    return true;
  }

  if (mPostponeKoszul) {
    // Relatively prime check
    for (auto it = mSpairTmp.begin(); it != mSpairTmp.end(); ++it) {
      auto a = GB->leadMono(it->first);
      auto b = GB->leadMono(it->second);
      if (monoid().relativelyPrime(a, b)) {
        ++stats_relativelyPrimeEliminated;
        // todo: what are the correct ownership relations here?
        auto ptr = sig.release();
        Hsyz->insert(*ptr);
        SP->setKnownSyzygies(mSpairTmp);
        MATHICGB_LOG(SigSPairFinal) <<
          "   eliminated by relatively prime criterion.\n";
        return true;
      }
    }
  }
#ifdef MATHICGB_DEBUG
  for (auto it = mSpairTmp.begin(); it != mSpairTmp.end(); ++it) {
    auto a = GB->leadMono(it->first);
    auto b = GB->leadMono(it->second);
    MATHICGB_ASSERT(!monoid().relativelyPrime(a, b));
  }
#endif

  // Reduce the pair
  ++stats_pairsReduced;
  if (!processSPair(std::move(sig), mSpairTmp) || !mPostponeKoszul)
    return true;
  for (auto it = mSpairTmp.begin(); it != mSpairTmp.end(); ++it) {
    std::pair<size_t, size_t> p = *it;
    if (GB->ratioCompare(p.first, p.second) == LT)
      std::swap(p.first, p.second);

    auto greaterSig = GB->signature(p.first);
    auto smallerLead = GB->leadMono(p.second);   
    monomial koszul = R->allocMonomial();
    monoid().multiply(greaterSig, smallerLead, koszul);
    if (Hsyz->member(koszul))
      R->freeMonomial(koszul);
    else
      mKoszuls.push(koszul);
  }
  return true;
}

size_t SignatureGB::getMemoryUse() const {
  return
    GB->getMemoryUse() +
    Hsyz->getMemoryUse() +
    R->getMemoryUse() +
    reducer->getMemoryUse() +
    mSpairTmp.capacity() * sizeof(mSpairTmp.front()) +
    SP->getMemoryUse() +
    mKoszuls.getMemoryUse();
}

void SignatureGB::displayStats(std::ostream &o) const
{
  o << "-- stats: -- \n";
  o << " strategy: signature"
    << (mPostponeKoszul ? "-postpone" : "")
    << (mUseBaseDivisors ? "-basediv" : "") << '\n';
  o << " reduction type: " << reducer->description() << '\n';
  o << " divisor tab type: " << GB->basis().monoLookup().getName() << '\n';
  o << " syzygy tab type: " << Hsyz->name() << '\n';
  o << " S-pair queue type: " << SP->name() << '\n';
  o << " total compute time:  " << stats_nsecs << " -- seconds" << '\n';

  displayMemoryUse(o);
  displaySomeStats(o);
  o << std::flush;
}

void SignatureGB::displayPaperStats(std::ostream& out) const {
  SigSPairs::Stats stats = SP->getStats();
  //Reducer::Stats reducerStats = reducer->sigStats();
  mic::ColumnPrinter pr;
  pr.addColumn(true, " ");
  pr.addColumn(false, " ");
  pr.addColumn(true, " ");

  std::ostream& name = pr[0];
  std::ostream& value = pr[1];
  std::ostream& extra = pr[2];

  const unsigned long long considered0 = GB->size() * (GB->size() - 1) / 2;
  const unsigned long long considered = stats.spairsConstructed;

  if (considered0 != considered) {
    name << "WARNING!!! #spairsConstructed is not correct!!\n";
    value << '\n';
    extra << '\n';
    }

  name << "S-pairs considered:\n";
  value << mic::ColumnPrinter::commafy(considered) << '\n';
  extra << '\n';

  name << "Removed via non-regular criterion:\n";
  value << mic::ColumnPrinter::commafy(stats.nonregularSPairs) << '\n';
  extra << '\n';

  name << "Removed via low base divisor:\n";
  value << mic::ColumnPrinter::commafy(stats.lowBaseDivisorHits) << '\n';
  extra << '\n';
  
  name << "Removed via high base divisor:\n";
  value << mic::ColumnPrinter::commafy(stats.highBaseDivisorHits) << '\n';
  extra << '\n';

  name << "Removed via signature criterion:\n";
  value << mic::ColumnPrinter::commafy(stats.syzygyModuleHits) << '\n';
  extra << '\n';

  name << "Removed via relatively prime criterion:\n";
  value << mic::ColumnPrinter::commafy(stats.earlyRelativelyPrimePairs) << '\n';
  extra << '\n';

  name << "Removed via singular criterion (early):\n";
  value << mic::ColumnPrinter::commafy(stats.earlySingularCriterionPairs) << '\n';
  extra << '\n';

  name << "Number of queued pairs:\n";
  value << mic::ColumnPrinter::commafy(stats.queuedPairs) << '\n';
  extra << '\n';

  name << "Removed via duplicate signature:\n";
  value << mic::ColumnPrinter::commafy(stats.duplicateSignatures) << '\n';
  extra << '\n';

  name << "Removed via signature criterion (late):\n";
  value << mic::ColumnPrinter::commafy(stats_SignatureCriterionLate) << '\n';
  extra << '\n';

  name << "Removed via Koszul criterion (late):\n";
  value << mic::ColumnPrinter::commafy(stats_koszulEliminated) << '\n';
  extra << '\n';

  name << "Removed via relatively prime criterion (late):\n";
  value << mic::ColumnPrinter::commafy(stats_relativelyPrimeEliminated) << '\n';
  extra << '\n';

  /*name << "Removed (singular reduction):\n";
  value << mic::ColumnPrinter::commafy(reducerStats.singularReductions) << '\n';
  extra << '\n';*/

  //unsigned long long nonzeroReductions = stats_pairsReduced - reducerStats.singularReductions - reducerStats.zeroReductions;

  /*name << "Number of pairs reduced to signature GB elems:\n";
  value << mic::ColumnPrinter::commafy(nonzeroReductions) << '\n';
  extra << '\n';*/

  /*name << "Number of pairs reduced to new syzygy signatures:\n";
  value << mic::ColumnPrinter::commafy(reducerStats.zeroReductions) << '\n';
  extra << '\n';*/

  /*
  unsigned long long nleft = considered 
    - stats.nonregularSPairs 
    - stats.lowBaseDivisorHits 
    - stats.highBaseDivisorHits
    - stats.syzygyModuleHits 
    - stats.earlyRelativelyPrimePairs
    - stats.earlySingularCriterionPairs;
  if (nleft != stats.queuedPairs) {
    name << "WARNING!!! queuedPairs is not correct!!\n";
    value << '\n';
    extra << '\n';
    }
  nleft = nleft
    - stats.duplicateSignatures
    - stats_SignatureCriterionLate
    - stats_koszulEliminated - stats_relativelyPrimeEliminated
    - reducerStats.singularReductions
    - reducerStats.zeroReductions
    - nonzeroReductions;
  
  name << "Number of spairs unaccounted for:\n";
  value << mic::ColumnPrinter::commafy(nleft) << '\n';
  extra << '\n';*/

#ifdef MATHIC_TRACK_DIV_MASK_HIT_RATIO
  name << "Divisor Mask Stats" << '\n';
  value << '\n';
  extra << '\n';

  name << "  DivMasks Computed: " << '\n';
  value << mic::ColumnPrinter::commafy(mathic::DivMaskStats::maskComputes) << '\n';
  extra << '\n';

  name << "  DivMask checks: " << '\n';
  value << mic::ColumnPrinter::commafy(mathic::DivMaskStats::maskChecks) << '\n';
  extra << '\n';

  name << "  DivMask hits: " << '\n';
  value << mic::ColumnPrinter::commafy(mathic::DivMaskStats::maskHits) << '\n';
  extra << '\n';

  name << "  DivMask divisor checks: " << '\n';
  value << mic::ColumnPrinter::commafy(mathic::DivMaskStats::divChecks) << '\n';
  extra << '\n';

  name << "  DivMask divisor divides: " << '\n';
  value << mic::ColumnPrinter::commafy(mathic::DivMaskStats::divDivides) << '\n';
  extra << '\n';

  name << "  DivMask divisor hits: " << '\n';
  value << mic::ColumnPrinter::commafy(mathic::DivMaskStats::divHits) << '\n';
  extra << '\n';
#endif

  out << "*** Statistics for ISSAC 2012 Paper ***\n" << pr << std::flush;
}

void SignatureGB::displaySomeStats(std::ostream& out) const {
  mic::ColumnPrinter pr;
  pr.addColumn(true, " ");
  pr.addColumn(false, " ");
  pr.addColumn(true, " ");

  std::ostream& name = pr[0];
  std::ostream& value = pr[1];
  std::ostream& extra = pr[2];

  const size_t basisSize = GB->size();
  const double mseconds = mTimer.getMilliseconds();
  const size_t pending = SP->pairCount();

  name << "Time spent:\n";
  value << mTimer << '\n';
  extra << mic::ColumnPrinter::oneDecimal(mseconds / basisSize)
    << " ms per basis element\n";

  const double pendingRatio = static_cast<double>(pending) / basisSize;
  name << "Basis elements:\n";
  value << mic::ColumnPrinter::commafy(basisSize) << '\n';
  extra << mic::ColumnPrinter::oneDecimal(pendingRatio)
    << " Sp pend per basis ele\n";

  const size_t basisTermCount = GB->basis().monomialCount();
  name << "Terms for basis:\n";
  value << mic::ColumnPrinter::commafy(basisTermCount) << '\n';
  extra << mic::ColumnPrinter::ratioInteger(basisTermCount, basisSize)
    << " terms per basis ele\n";

  const size_t minLeadCount = GB->basis().minimalLeadCount();
  name << "Minimum lead terms:\n";
  value << mic::ColumnPrinter::commafy(minLeadCount) << '\n';
  extra << mic::ColumnPrinter::percentInteger(minLeadCount, basisSize)
    << " basis ele have min lead\n";

  const size_t lastMinLead = GB->basis().maxIndexMinimalLead() + 1;
  const size_t timeSinceLastMinLead = basisSize - lastMinLead;
  name << "Index of last min lead:\n";
  value << mic::ColumnPrinter::commafy(lastMinLead) << '\n';
  extra << mic::ColumnPrinter::percentInteger(timeSinceLastMinLead, basisSize)
    << " of basis added since then\n";

  const size_t minSyz = Hsyz->elementCount();
  const double syzBasisRatio =
    static_cast<double>(minSyz) / basisSize;
  name << "Minimal syzygies:\n";
  value << mic::ColumnPrinter::commafy(minSyz) << '\n';
  extra << mic::ColumnPrinter::oneDecimal(syzBasisRatio)
    << " syzygies per basis element\n";

  const size_t queuedKoszuls = mKoszuls.size();
  const double quedRatio = static_cast<double>(queuedKoszuls) / minSyz;
  name << "Queued Koszul syzygies:\n";
  value << mic::ColumnPrinter::commafy(queuedKoszuls) << '\n';
  extra << mic::ColumnPrinter::oneDecimal(quedRatio)
    << " queued koszuls per msyzygy\n";

  const unsigned long long considered = GB->size() * (GB->size() - 1) / 2;
  name << "S-pairs considered:\n";
  value << mic::ColumnPrinter::commafy(considered) << '\n';
  extra << '\n';

  unsigned long long earlyNonElim;
  SigSPairs::Stats stats = SP->getStats();
  const unsigned long long lowElim = stats.lowBaseDivisorHits;
  const unsigned long long highElim = stats.highBaseDivisorHits;
  const unsigned long long syzElim = stats.syzygyModuleHits;
  earlyNonElim = considered - lowElim - highElim - syzElim;

  name << "S-pairs not early elim:\n";
  value << mic::ColumnPrinter::commafy(earlyNonElim) << '\n';
  extra << mic::ColumnPrinter::percentInteger(earlyNonElim, considered)
    << " of considered\n";

  name << "Syz module S-pair elim:\n";
  value << mic::ColumnPrinter::commafy(syzElim) << '\n';
  extra << mic::ColumnPrinter::percentInteger(syzElim, considered)
    << " of considered\n";

  name << "Low bdiv S-pair elim:\n";
  value << mic::ColumnPrinter::commafy(lowElim) << '\n';
  extra << mic::ColumnPrinter::percentInteger(lowElim, considered)
    << " of considered\n";

  name << "High bdiv S-pair elim:\n";
  value << mic::ColumnPrinter::commafy(highElim) << '\n';
  extra << mic::ColumnPrinter::percentInteger(highElim, considered)
    << " of considered\n";

  const unsigned long long hadLow = stats.hasLowBaseDivisor;
  name << "Basis ele had low bdiv:\n";
  value << mic::ColumnPrinter::commafy(hadLow) << '\n';
  extra << mic::ColumnPrinter::percentInteger(hadLow, basisSize)
    << " of basis ele\n";

  const unsigned long long hadHigh = stats.hasHighBaseDivisor;
  name << "Basis ele had high bdiv:\n";
  value << mic::ColumnPrinter::commafy(hadHigh) << '\n';
  extra << mic::ColumnPrinter::percentInteger(hadHigh, basisSize)
    << " of basis ele\n";

  name << "S-pairs pending:\n";
  value << mic::ColumnPrinter::commafy(pending) << '\n';
  extra << mic::ColumnPrinter::percentInteger(pending, considered)
    << " of considered\n";

  const size_t done = stats_sPairsDone;
  name << "S pairs done:\n";
  value << mic::ColumnPrinter::commafy(done) << '\n';
  extra << mic::ColumnPrinter::percentInteger(done, earlyNonElim)
    << " of not early elim\n";

  const size_t sigsDone = stats_sPairSignaturesDone;
  const double perSig = static_cast<double>(done) / sigsDone;
  name << "S pair sigs done:\n";
  value << mic::ColumnPrinter::commafy(sigsDone) << '\n';
  extra << mic::ColumnPrinter::oneDecimal(perSig)
    << " spairs per signature\n";

  //Reducer::Stats reducerStats = reducer->sigStats();

  /*const unsigned long long reductions = reducerStats.reductions;
  const size_t koszulElim = stats_koszulEliminated;
  name << "Koszul sp eliminated:\n";
  value << mic::ColumnPrinter::commafy(koszulElim) << '\n';
  extra << mic::ColumnPrinter::percentInteger(koszulElim, sigsDone - reductions)
    << " of late eliminations\n";*/

  /*const size_t primeElim = stats_relativelyPrimeEliminated;
  name << "Rel.prime sp eliminated:\n";
  value << mic::ColumnPrinter::commafy(primeElim) << '\n';
  extra << mic::ColumnPrinter::percentInteger(primeElim, sigsDone - reductions)
    << " of late eliminations\n";*/

  /*name << "Signature reductions:\n";
  value << mic::ColumnPrinter::commafy(reductions) << '\n';
  extra << mic::ColumnPrinter::percentInteger(reductions, sigsDone)
    << " of S-pairs are reduced\n";*/

  /*const unsigned long long singularReductions =
    reducerStats.singularReductions;
  name << "Singular reductions:\n";
  value << mic::ColumnPrinter::commafy(singularReductions) << '\n';
  extra << mic::ColumnPrinter::percentInteger(singularReductions, reductions)
    << " of reductions\n";*/

  /*const unsigned long long zeroReductions = reducerStats.zeroReductions;
  name << "Reductions to zero:\n";
  value << mic::ColumnPrinter::commafy(zeroReductions) << '\n';
  extra << mic::ColumnPrinter::percentInteger(zeroReductions, reductions)
    << " of reductions\n";*/

  /*const unsigned long long newReductions =
    reductions - singularReductions - zeroReductions;
  name << "Reductions to new ele:\n";
  value << mic::ColumnPrinter::commafy(newReductions) << '\n';
  extra << mic::ColumnPrinter::percentInteger(newReductions, reductions)
    << " of reductions\n";*/

  /*const unsigned long long redSteps = reducerStats.steps;
  const double stepsRatio =
    static_cast<double>(redSteps) / (reductions - singularReductions);
  name << "Sig reduction steps:\n";
  value << mic::ColumnPrinter::commafy(redSteps) << '\n';
  extra << mic::ColumnPrinter::oneDecimal(stepsRatio)
    << " steps per non-sing reduction\n";*/

  /*const unsigned long long longestReduction = reducerStats.maxSteps;
  name << "Longest sig reduction:\n";
  value << mic::ColumnPrinter::commafy(longestReduction) << '\n';
  extra << '\n';*/

  /*Reducer::Stats classicRedStats = reducer->classicStats();
  const unsigned long long clReductions = classicRedStats.reductions;
  name << "Classic reductions:\n";
  value << mic::ColumnPrinter::commafy(clReductions) << '\n';
  extra << '\n';*/

  /*const unsigned long long clRedSteps =  classicRedStats.steps;
  const double clStepsRatio = static_cast<double>(clRedSteps) / clReductions;
  name << "Classic reduction steps:\n";
  value << mic::ColumnPrinter::commafy(clRedSteps) << '\n';
  extra << mic::ColumnPrinter::oneDecimal(clStepsRatio)
    << " steps per reduction\n";*/

  /*const unsigned long long clLongestReduction = classicRedStats.maxSteps;
  name << "Longest classic red:\n";
  value << mic::ColumnPrinter::commafy(clLongestReduction) << '\n';
  extra << '\n';*/

  out << "*** Some of the statistics ***\n" << pr << std::flush;
}

void SignatureGB::displayMemoryUse(std::ostream& out) const
{
  // set up printer
  mic::ColumnPrinter pr;
  pr.addColumn();
  pr.addColumn(false);
  pr.addColumn(false);

  std::ostream& name = pr[0];
  std::ostream& value = pr[1];
  std::ostream& extra = pr[2];

  const size_t total = getMemoryUse();
  { // Grobner basis
    const size_t basisMem = GB->getMemoryUse();
    name << "Grobner basis:\n";
    value << mic::ColumnPrinter::bytesInUnit(basisMem) << '\n';
    extra << mic::ColumnPrinter::percentInteger(basisMem, total) << '\n';
  }
  { // Spairs
    const size_t sPairMem = SP->getMemoryUse();
    name << "S-pairs:\n";
    value << mic::ColumnPrinter::bytesInUnit(sPairMem) << '\n';
    extra << mic::ColumnPrinter::percentInteger(sPairMem, total) << '\n';

    const size_t knownSyzygyMem = SP->getKnownSyzygyBitsMemoryUse();
    name << "  Known syzygy bits:\n";
    value << mic::ColumnPrinter::bytesInUnit(knownSyzygyMem) << '\n';
    extra << '\n';
  }
  { // Syzygies
    const size_t syzMem = Hsyz->getMemoryUse();
    name << "Minimal syzygies:\n";
    value << mic::ColumnPrinter::bytesInUnit(syzMem) << '\n';
    extra << mic::ColumnPrinter::percentInteger(syzMem, total) << '\n';
  }
  { // Koszul queue
    const size_t syzQueueMem = mKoszuls.getMemoryUse();
    name << "Koszul queue:\n";
    value << mic::ColumnPrinter::bytesInUnit(syzQueueMem) << '\n';
    extra << mic::ColumnPrinter::percentInteger(syzQueueMem, total) << '\n';
  }
  { // Reducer
    const size_t reducerMem = reducer->getMemoryUse();
    name << "Reducer:\n";
    value << mic::ColumnPrinter::bytesInUnit(reducerMem) << '\n';
    extra << mic::ColumnPrinter::percentInteger(reducerMem, total) << '\n';
  }
  { // Signatures
    const size_t sigMem = R->getMemoryUse();
    name << "Signatures:\n";
    value << mic::ColumnPrinter::bytesInUnit(sigMem) << '\n';
    extra << mic::ColumnPrinter::percentInteger(sigMem, total) << '\n';
  }
  // total
  name << "-------------\n";
  value << '\n';
  extra << '\n';

  name << "Memory used in total:\n";
  value << mic::ColumnPrinter::bytesInUnit(total) << "\n";
  extra << "\n";

  out << "*** Summary of memory use ***\n" << pr << std::flush;
}

MATHICGB_NAMESPACE_END
