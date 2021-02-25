// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "ClassicGBAlg.hpp"

#include "Reducer.hpp"
#include "SPairs.hpp"
#include "PolyBasis.hpp"
#include "Basis.hpp"
#include "LogDomain.hpp"
#include "MathicIO.hpp"
#include <iostream>
#include "mathic/mathic.h"
#include <memory>
#include <vector>

MATHICGB_DEFINE_LOG_DOMAIN(
  SPairDegree,
  "Displays the degree of the S-pairs being considered in "
    "Buchberger's algorithm."
);

MATHICGB_DEFINE_LOG_DOMAIN(
  GBInsert,
  "Outputs polynomials that are inserted into the current "
  "basis during Groebner basis computation using Buchberger's. "
  "algorithm."
);

MATHICGB_NAMESPACE_BEGIN

/// Calculates a classic Grobner basis using Buchberger's algorithm.
class ClassicGBAlg {
public:
  ClassicGBAlg(
    const Basis& basis,
    Reducer& reducer,
    int monoLookupType,
    bool preferSparseReducers,
    size_t queueType
  );

  // Replaces the current basis with a Grobner basis of the same ideal.
  void computeGrobnerBasis();

  // How many S-pairs were not eliminated before reduction of the
  // corresponding S-polynomial.
  unsigned long long sPolyReductionCount() const {return mSPolyReductionCount;}

  // Returns the current basis.
  PolyBasis& basis() {return mBasis;}

  // Shows statistics on what the algorithm has done.
  void printStats(std::ostream& out) const;

  void printMemoryUse(std::ostream& out) const;

  size_t getMemoryUse() const;

  void setBreakAfter(unsigned int elements) {
    mBreakAfter = elements;
  }

  void setPrintInterval(unsigned int reductions) {
    mPrintInterval = reductions;
  }

  /// A value of zero means to let the algorithm decide a reasonable
  /// value based on the other settings.
  void setSPairGroupSize(unsigned int groupSize);

  void setReducerMemoryQuantum(size_t memoryQuantum) {
    mReducer.setMemoryQuantum(memoryQuantum);
  }

  void setUseAutoTopReduction(bool value) {
    mUseAutoTopReduction = value;
  }

  void setUseAutoTailReduction(bool value) {
    mUseAutoTailReduction = value;
  }

  /// callback is called every once in a while and then it has the
  /// option of stopping the computation. callback can be null, in
  /// which case no call is made and the computation continues.
  void setCallback(std::function<bool(void)> callback) {
    mCallback = std::move(callback);
  }

private:
  std::function<bool(void)> mCallback;
  unsigned int mBreakAfter;
  unsigned int mPrintInterval;
  unsigned int mSPairGroupSize;
  bool mUseAutoTopReduction;
  bool mUseAutoTailReduction;

  // Perform a step of the algorithm.
  void step();

  void autoTailReduce();

  void insertReducedPoly(std::unique_ptr<Poly> poly);

  // clears polynomials.
  void insertPolys(std::vector<std::unique_ptr<Poly> >& polynomials);

  const PolyRing& mRing;
  Reducer& mReducer;
  PolyBasis mBasis;
  SPairs mSPairs;
  mic::Timer mTimer;
  unsigned long long mSPolyReductionCount;
};

ClassicGBAlg::ClassicGBAlg(
  const Basis& basis,
  Reducer& reducer,
  int monoLookupType,
  bool preferSparseReducers,
  size_t queueType
):
  mCallback(nullptr),
  mBreakAfter(0),
  mPrintInterval(0),
  mSPairGroupSize(reducer.preferredSetSize()),
  mUseAutoTopReduction(true),
  mUseAutoTailReduction(false),
  mRing(*basis.getPolyRing()),
  mReducer(reducer),
  mBasis(mRing,
    MonoLookup::makeFactory(
      basis.getPolyRing()->monoid(),
      monoLookupType
    )->make(preferSparseReducers, true)
  ),
  mSPairs(mBasis, preferSparseReducers),
  mSPolyReductionCount(0)
{
  // Reduce and insert the generators of the ideal into the starting basis
  size_t const basisSize = basis.size();
  std::vector<std::unique_ptr<Poly> > polys;
  for (size_t gen = 0; gen != basisSize; ++gen)
    polys.push_back(make_unique<Poly>(*basis.getPoly(gen)));
  insertPolys(polys);
}

void ClassicGBAlg::setSPairGroupSize(unsigned int groupSize) {
  if (groupSize == 0)
    groupSize = mReducer.preferredSetSize();
  else
    mSPairGroupSize = groupSize;
}

void ClassicGBAlg::insertPolys(
  std::vector<std::unique_ptr<Poly> >& polynomials
) {
  if (!mUseAutoTopReduction) {
    for (auto it = polynomials.begin(); it != polynomials.end(); ++it) {
      MATHICGB_ASSERT(it->get() != 0);
      if ((*it)->isZero())
        continue;
      if (mBasis.divisor((*it)->leadMono()) != static_cast<size_t>(-1)) {
        *it = mReducer.classicReduce(**it, mBasis);
        if ((*it)->isZero())
          continue;
      }

      mBasis.insert(std::move(*it));
      mSPairs.addPairs(mBasis.size() - 1);
    }
    polynomials.clear();
    return;
  }

  std::vector<size_t> toRetire;
  std::vector<std::unique_ptr<Poly>> toReduce;
  std::vector<std::unique_ptr<Poly>> toInsert;
  std::swap(toInsert, polynomials);

  while (!toInsert.empty()) {
    // todo: sort by lead term to avoid insert followed by immediate
    // removal.

    // insert polynomials from toInsert with minimal lead term and
    // extract those from the basis that become non-minimal.
    for (auto it = toInsert.begin(); it != toInsert.end(); ++it) {
      MATHICGB_ASSERT(it->get() != 0);
      if ((*it)->isZero())
        continue;

      // We check for a divisor from mBasis because a new reducer
      // might have been added since we did the reduction or perhaps a
      // non-reduced polynomial was passed in.
      if (mBasis.divisor((*it)->leadMono()) != static_cast<size_t>(-1))
        toReduce.push_back(std::move(*it));
      else {
        MATHICGB_IF_STREAM_LOG(GBInsert) {
          stream << "Inserting basis element " << mBasis.size() << ": ";
          MathicIO<>().writePoly(**it, true, stream);
          stream << '\n';
        };
        mBasis.insert(std::move(*it));
        MATHICGB_ASSERT(toRetire.empty());
        mSPairs.addPairsAssumeAutoReduce(mBasis.size() - 1, toRetire);
        for (auto r = toRetire.begin(); r != toRetire.end(); ++r)
          toReduce.push_back(mBasis.retire(*r));
        toRetire.clear();
      }
    }
    toInsert.clear();
    MATHICGB_ASSERT(toRetire.empty());

    // reduce everything in toReduce
    if (!toReduce.empty()) {
      mReducer.classicReducePolySet(toReduce, mBasis, toInsert);
      toReduce.clear();
    }
  }

  MATHICGB_ASSERT(toRetire.empty());
  MATHICGB_ASSERT(toInsert.empty());
  MATHICGB_ASSERT(toReduce.empty());
}

void ClassicGBAlg::insertReducedPoly(
  std::unique_ptr<Poly> polyToInsert
) {
  MATHICGB_ASSERT(polyToInsert.get() != 0);
  if (polyToInsert->isZero())
    return;
  MATHICGB_ASSERT(mBasis.divisor(polyToInsert->leadMono()) ==
    static_cast<size_t>(-1));

  if (tracingLevel > 20) {
    std::cerr << "inserting basis element " << mBasis.size() << ": ";
    if (tracingLevel > 100) {
      MathicIO<>().writePoly(*polyToInsert, false, std::cerr);
      std::cerr << std::endl;
    } else {
      mRing.printMonomialFrobbyM2Format
        (std::cerr, Monoid::toOld(polyToInsert->leadMono()));
      if (polyToInsert->termCount() > 1)
        std::cerr << " + [...]";
      std::cerr << std::endl;
    }
  }

  if (!mUseAutoTopReduction) {
    size_t const newGen = mBasis.size();
    mBasis.insert(std::move(polyToInsert));
    mSPairs.addPairs(newGen);
    return;
  }

  std::vector<size_t> toRetireAndReduce;
  std::vector<Poly*> toReduce;

  try {
    do {
      // reduce polynomial and insert into basis
      {
        std::unique_ptr<Poly> reduced;
        if (toReduce.empty()) // if first iteration
          reduced = std::move(polyToInsert);
        else {
          reduced = mReducer.classicReduce(*toReduce.back(), mBasis);
          if (tracingLevel > 20) {
            if (reduced->isZero()) {
              std::cerr << "auto-top-reduce cascade: "
                "basis element reduced to zero."
                << std::endl;
            } else {
              std::cerr << "auto-top-reduce cascade: "
                "inserting reduced poly with lead term "
                << std::endl;
              mRing.printMonomialFrobbyM2Format
              (std::cerr, Monoid::toOld(reduced->leadMono()));
              std::cerr << '\n';
            }
          }
          delete toReduce.back();
          toReduce.pop_back();
        }
        if (reduced->isZero())
          continue;
        reduced->makeMonic(); 
        mBasis.insert(std::move(reduced));
      }

      // form S-pairs and retire basis elements that become top reducible.
      const size_t newGen = mBasis.size() - 1;
      MATHICGB_ASSERT(toRetireAndReduce.empty());
      mSPairs.addPairsAssumeAutoReduce(newGen, toRetireAndReduce);
      for (std::vector<size_t>::const_iterator it = toRetireAndReduce.begin();
        it != toRetireAndReduce.end(); ++it) {
        toReduce.push_back(0); // allocate space in vector before .release()
        toReduce.back() = mBasis.retire(*it).release();
      }
      toRetireAndReduce.clear();
    } while (!toReduce.empty());
  } catch (...) {
    for (std::vector<Poly*>::iterator it = toReduce.begin();
      it != toReduce.end(); ++it)
      delete *it;
    throw;
  }
  MATHICGB_ASSERT(toReduce.empty());
  MATHICGB_ASSERT(toRetireAndReduce.empty());
}

void ClassicGBAlg::computeGrobnerBasis() {
  size_t counter = 0;
  mTimer.reset();

  if (mUseAutoTailReduction)
    autoTailReduce();

  while (!mSPairs.empty()) {
    if (mCallback != nullptr && !mCallback())
      break;

    step();
    if (mBreakAfter != 0 && mBasis.size() > mBreakAfter) {
      std::cerr
        << "Stopping Grobner basis computation due to reaching limit of "
        << mBreakAfter << " basis elements." << std::endl;
      break;
    }
    if (mPrintInterval != 0 && (++counter % mPrintInterval) == 0)
      printStats(std::cerr);
  }
  if (mPrintInterval != 0)
    printStats(std::cerr);
  //mReducer->dump();
  /*
  for (size_t i = 0; i < mBasis.size(); ++i)
    if (!mBasis.retired(i))
      mBasis.replaceSameLeadTerm
        (i, mReducer->classicTailReduce(mBasis.poly(i), mBasis));
        */
}

void ClassicGBAlg::step() {
  MATHICGB_ASSERT(!mSPairs.empty());
  if (tracingLevel > 30)
    std::cerr << "Determining next S-pair" << std::endl;

  MATHICGB_ASSERT(mSPairGroupSize >= 1);
  std::vector<std::pair<size_t, size_t> > spairGroup;
  exponent w = 0;
  for (unsigned int i = 0; i < mSPairGroupSize; ++i) {
    auto p = mSPairs.pop(w);
    if (p.first == static_cast<size_t>(-1)) {
      MATHICGB_ASSERT(p.second == static_cast<size_t>(-1));
      break; // no more S-pairs
    }
    MATHICGB_ASSERT(p.first != static_cast<size_t>(-1));
    MATHICGB_ASSERT(p.second != static_cast<size_t>(-1));
    MATHICGB_ASSERT(!mBasis.retired(p.first));
    MATHICGB_ASSERT(!mBasis.retired(p.second));
    
    spairGroup.push_back(p);
  }
  if (spairGroup.empty())
    return; // no more s-pairs
  std::vector<std::unique_ptr<Poly>> reduced;

  // w is the negative of the degree of the lcm's of the chosen spairs
  MATHICGB_LOG(SPairDegree) <<
    spairGroup.size() << " pairs in degree " << -w << std::endl;

  mReducer.classicReduceSPolySet(spairGroup, mBasis, reduced);

  // sort the elements to get deterministic behavior. The order will change
  // arbitrarily when running multithreaded. Also, if preferring older
  // reducers, it is of benefit to break ties by preferring the sparser
  // reducer. Age does not have ties, since each element has a distinct
  // index, but if they all come from the same matrix then there is
  // really nothing to distinguish them - the relative age is arbitrarily
  // chosen. If we order sparsest-first, we will effectively make the
  // arbitrary choice among reducers from the same matrix in favor of sparser
  // reducers.
  auto order = [&](
    const std::unique_ptr<Poly>& a,
    const std::unique_ptr<Poly>& b
  ) {
    const auto aTermCount = a->termCount();
    const auto bTermCount = b->termCount();
    if (aTermCount < bTermCount)
      return true;
    if (aTermCount > bTermCount)
      return false;
    auto bIt = b->begin();
    const auto aEnd = a->end();
    for (auto aIt = a->begin(); aIt != aEnd; ++aIt, ++bIt) {
      const auto monoCmp =
        mRing.monoid().compare(aIt.mono(), bIt.mono());
      if (monoCmp == LT)
        return true;
      if (monoCmp == GT)
        return false;
      if (aIt.coef() < bIt.coef())
          return true;
      if (aIt.coef() > bIt.coef())
        return false;
    }
    return false;
  };
  std::sort(reduced.begin(), reduced.end(), order);
  
  insertPolys(reduced);
  if (mUseAutoTailReduction)
    autoTailReduce();
}

void ClassicGBAlg::autoTailReduce() {
  MATHICGB_ASSERT(mUseAutoTailReduction);

  for (size_t i = 0; i < mBasis.size(); ++i) {
    if (mBasis.retired(i))
      continue;
    if (mBasis.usedAsReducerCount(i) < 1000)
      continue;
    mBasis.replaceSameLeadTerm
      (i, mReducer.classicTailReduce(mBasis.poly(i), mBasis));
  }
}

size_t ClassicGBAlg::getMemoryUse() const {
  return
    mBasis.getMemoryUse() +
    mRing.getMemoryUse() +
    mReducer.getMemoryUse() +
    mSPairs.getMemoryUse();
}

void ClassicGBAlg::printStats(std::ostream& out) const {
  out << " reduction type:     " << mReducer.description() << '\n';
  out << " divisor tab type:   " << mBasis.monoLookup().getName() << '\n';
  out << " S-pair queue type:  " << mSPairs.name() << '\n';
  out << " total compute time: " << mTimer.getMilliseconds()/1000.0 << " seconds " << '\n';
  out << " S-pair group size:  " << mSPairGroupSize << '\n';

  mic::ColumnPrinter pr;
  pr.addColumn(true, " ");
  pr.addColumn(false, " ");
  pr.addColumn(true, " ");

  std::ostream& name = pr[0];
  std::ostream& value = pr[1];
  std::ostream& extra = pr[2];

  const size_t basisSize = mBasis.size();
  const double mseconds = mTimer.getMilliseconds();
  const size_t pending = mSPairs.pairCount();

  name << "Time spent:\n";
  value << mTimer << '\n';
  extra << mic::ColumnPrinter::oneDecimal(mseconds / basisSize)
        << " ms per basis element\n";

  const double pendingRatio = static_cast<double>(pending) / basisSize;
  name << "Basis elements:\n";
  value << mic::ColumnPrinter::commafy(basisSize) << '\n';
  extra << mic::ColumnPrinter::oneDecimal(pendingRatio)
        << " Sp pend per basis ele\n";

  const size_t basisTermCount = mBasis.monomialCount();
  name << "Terms for basis:\n";
  value << mic::ColumnPrinter::commafy(basisTermCount) << '\n';
  extra << mic::ColumnPrinter::ratioInteger(basisTermCount, basisSize)
        << " terms per basis ele\n";

  const size_t minLeadCount = mBasis.minimalLeadCount();
  name << "Minimum lead terms:\n";
  value << mic::ColumnPrinter::commafy(minLeadCount) << '\n';
  extra << mic::ColumnPrinter::percentInteger(minLeadCount, basisSize)
        << " basis ele have min lead\n";

  const size_t lastMinLead = mBasis.maxIndexMinimalLead() + 1;
  const size_t timeSinceLastMinLead = basisSize - lastMinLead;
  name << "Index of last min lead:\n";
  value << mic::ColumnPrinter::commafy(lastMinLead) << '\n';
  extra << mic::ColumnPrinter::percentInteger(timeSinceLastMinLead, basisSize)
        << " of basis added since then\n";

  const unsigned long long considered =
    mBasis.size() * (mBasis.size() - 1) / 2;
  name << "S-pairs considered:\n";
  value << mic::ColumnPrinter::commafy(considered) << '\n';
  extra << '\n';

  name << "S-pairs pending:\n";
  value << mic::ColumnPrinter::commafy(pending) << '\n';
  extra << mic::ColumnPrinter::percentInteger(pending, considered)
        << " of considered\n";

  unsigned long long const reductions = sPolyReductionCount();
  name << "S-pairs reduced:\n";
  value << mic::ColumnPrinter::commafy(reductions) << '\n';
  extra << '\n';

  //Reducer::Stats reducerStats = mReducer.sigStats();
  SPairs::Stats sPairStats = mSPairs.stats();

  unsigned long long const primeElim = sPairStats.relativelyPrimeHits;
  name << "Rel.prime sp eliminated:\n";
  value << mic::ColumnPrinter::commafy(primeElim) << '\n';
  extra << mic::ColumnPrinter::percentInteger(primeElim, reductions)
        << " of late eliminations\n";

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
  name << "Sig reduction steps:\n";
  value << mic::ColumnPrinter::commafy(redSteps) << '\n';
  extra << mic::ColumnPrinter::ratioInteger
    (redSteps, reductions - singularReductions)
        << " steps per non-sing reduction\n";*/

  /*const unsigned long long longestReduction = reducerStats.maxSteps;
  name << "Longest sig reduction:\n";
  value << mic::ColumnPrinter::commafy(longestReduction) << '\n';
  extra << '\n';*/

  /*Reducer::Stats classicRedStats = mReducer.classicStats();
  const unsigned long long clReductions = classicRedStats.reductions;
  name << "Classic reductions:\n";
  value << mic::ColumnPrinter::commafy(clReductions) << '\n';
  extra << '\n';*/

  /*const unsigned long long clRedSteps =  classicRedStats.steps;
  const double clStepsRatio = static_cast<double>(clRedSteps) / clReductions;
  name << "Classic reduction steps:\n";
  value << mic::ColumnPrinter::commafy(clRedSteps) << '\n';
  extra << mic::ColumnPrinter::ratioInteger(clRedSteps, clReductions)
        << " steps per reduction\n";*/

  /*const unsigned long long clLongestReduction = classicRedStats.maxSteps;
  name << "Longest classic red:\n";
  value << mic::ColumnPrinter::commafy(clLongestReduction) << '\n';
  extra << '\n';*/

  //SPairs::Stats sPairStats = mSPairs.stats();  
  unsigned long long marginal = sPairStats.sPairsConsidered;

  unsigned long long const primeHits = sPairStats.relativelyPrimeHits;
  name << "Buchb relatively prime:\n";
  value << mic::ColumnPrinter::commafy(primeHits) << '\n';
  extra << mic::ColumnPrinter::percentInteger(primeHits, marginal) <<
    " of S-pairs\n";
  marginal -= primeHits;

  unsigned long long const simpleHits = sPairStats.buchbergerLcmSimpleHits;
  name << "Buchb lcm simple hits:\n";
  value << mic::ColumnPrinter::commafy(simpleHits) << '\n';
  extra << mic::ColumnPrinter::percentInteger(simpleHits, marginal) <<
    " of remaining S-pairs\n";
  marginal -= simpleHits;

  unsigned long long const simpleHitsLate = sPairStats.buchbergerLcmSimpleHitsLate;
  name << "Buchb late lcm simple hits:\n";
  value << mic::ColumnPrinter::commafy(simpleHitsLate) << '\n';
  extra << mic::ColumnPrinter::percentInteger(simpleHitsLate, marginal) <<
    " of remaining S-pairs\n";
  marginal -= simpleHitsLate;

  unsigned long long const buchAdvHits =
    sPairStats.buchbergerLcmAdvancedHits;
  name << "Buchb lcm adv hits:\n";
  value << mic::ColumnPrinter::commafy(buchAdvHits) << '\n';
  extra << mic::ColumnPrinter::percentInteger(buchAdvHits, marginal) <<
    " of remaining S-pairs\n";

  const unsigned long long buchCache = sPairStats.buchbergerLcmCacheHits;
  name << "Buchb lcm cache hits:\n";
  value << mic::ColumnPrinter::commafy(buchCache) << '\n';
  extra << mic::ColumnPrinter::percentInteger(buchCache, simpleHits) <<
    " of simple hits\n";

  const unsigned long long buchCacheLate = sPairStats.buchbergerLcmCacheHitsLate;
  name << "Buchb late lcm cache hits:\n";
  value << mic::ColumnPrinter::commafy(buchCacheLate) << '\n';
  extra << mic::ColumnPrinter::percentInteger(buchCacheLate, simpleHits) <<
    " of simple hits\n";

  out << "***** Classic Buchberger algorithm statistics *****\n"
      << pr << std::flush;
}

void ClassicGBAlg::printMemoryUse(std::ostream& out) const
{
  // Set up printer
  mic::ColumnPrinter pr;
  pr.addColumn();
  pr.addColumn(false);
  pr.addColumn(false);

  std::ostream& name = pr[0];
  std::ostream& value = pr[1];
  std::ostream& extra = pr[2];

  const size_t total = getMemoryUse();
  { // Grobner basis
    const size_t basisMem = mBasis.getMemoryUse();
    name << "Grobner basis:\n";
    value << mic::ColumnPrinter::bytesInUnit(basisMem) << '\n';
    extra << mic::ColumnPrinter::percentInteger(basisMem, total) << '\n';
  }
  { // Spairs
    const size_t sPairMem = mSPairs.getMemoryUse();
    name << "S-pairs:\n";
    value << mic::ColumnPrinter::bytesInUnit(sPairMem) << '\n';
    extra << mic::ColumnPrinter::percentInteger(sPairMem, total) << '\n';
  }
  { // Reducer
    const size_t reducerMem = mReducer.getMemoryUse();
    name << "Reducer:\n";
    value << mic::ColumnPrinter::bytesInUnit(reducerMem) << '\n';
    extra << mic::ColumnPrinter::percentInteger(reducerMem, total) << '\n';
  }
  { // Signatures
    const size_t sigMem = mRing.getMemoryUse();
    name << "Monomials:\n";
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

  out << "*** Memory use by component ***\n" << pr << std::flush;
}

Basis computeGBClassicAlg(
  Basis&& inputBasis,
  ClassicGBAlgParams params
) {
  ClassicGBAlg alg(
    inputBasis,
    *params.reducer,
    params.monoLookupType,
    params.preferSparseReducers,
    params.sPairQueueType
  );
  alg.setBreakAfter(params.breakAfter);
  alg.setPrintInterval(params.printInterval);
  alg.setSPairGroupSize(params.sPairGroupSize);
  alg.setReducerMemoryQuantum(params.reducerMemoryQuantum);
  alg.setUseAutoTopReduction(params.useAutoTopReduction);
  alg.setUseAutoTailReduction(params.useAutoTailReduction);
  alg.setCallback(params.callback);

  alg.computeGrobnerBasis();
  return std::move(*alg.basis().toBasisAndRetireAll());
}

Basis computeModuleGBClassicAlg(
  Basis&& inputBasis,
  ClassicGBAlgParams params
) {
  return computeGBClassicAlg(std::move(inputBasis), params);
}

MATHICGB_NAMESPACE_END
