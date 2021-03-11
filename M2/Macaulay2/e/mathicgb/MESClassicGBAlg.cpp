// Parts taken from MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
// Changes Copyright 2021 Michael Stillman.

#include "ClassicGBAlg.hpp"

#include "Reducer.hpp"
#include "SPairs.hpp"
#include "PolyBasis.hpp"
#include "Basis.hpp"
#include "MathicIO.hpp"

#include <iostream>
#include <mathic.h>
#include <memory>
#include <vector>

namespace mgb {
  /// Calculates a classic Grobner basis using Buchberger's algorithm.
  class ClassicGBAlg {
  private:
    std::function<bool(void)> mCallback;
    unsigned int mBreakAfter;
    unsigned int mPrintInterval;
    unsigned int mSPairGroupSize;
    bool mUseAutoTopReduction;
    bool mUseAutoTailReduction;

    const PolyRing& mRing;
    Reducer& mReducer;
    PolyBasis mBasis;
    SPairs mSPairs;
    mic::Timer mTimer;
    unsigned long long mSPolyReductionCount;

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
    uint64 sPolyReductionCount() const {return mSPolyReductionCount;}
    
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
    // Perform a step of the algorithm.
    void step();
    
    void autoTailReduce();
    
    void insertReducedPoly(std::unique_ptr<Poly> poly);
    
    // clears polynomials.
    void insertPolys(std::vector<std::unique_ptr<Poly> >& polynomials);    
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
        assert(it->get() != 0);
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
        assert(it->get() != 0);
        if ((*it)->isZero())
          continue;
        
        // We check for a divisor from mBasis because a new reducer
        // might have been added since we did the reduction or perhaps a
        // non-reduced polynomial was passed in.
        if (mBasis.divisor((*it)->leadMono()) != static_cast<size_t>(-1))
          toReduce.push_back(std::move(*it));
        else {
          mBasis.insert(std::move(*it));
          assert(toRetire.empty());
          mSPairs.addPairsAssumeAutoReduce(mBasis.size() - 1, toRetire);
          for (auto r = toRetire.begin(); r != toRetire.end(); ++r)
            toReduce.push_back(mBasis.retire(*r));
          toRetire.clear();
        }
      }
      toInsert.clear();
      assert(toRetire.empty());
      
      // reduce everything in toReduce
      if (!toReduce.empty()) {
        mReducer.classicReducePolySet(toReduce, mBasis, toInsert);
        toReduce.clear();
      }
    }
    
    assert(toRetire.empty());
    assert(toInsert.empty());
    assert(toReduce.empty());
  }
  
  void ClassicGBAlg::insertReducedPoly(
                                       std::unique_ptr<Poly> polyToInsert
                                       ) {
    assert(polyToInsert.get() != 0);
    if (polyToInsert->isZero())
      return;
    assert(mBasis.divisor(polyToInsert->leadMono()) ==
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
        assert(toRetireAndReduce.empty());
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
    assert(toReduce.empty());
    assert(toRetireAndReduce.empty());
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
    assert(!mSPairs.empty());
    if (tracingLevel > 30)
      std::cerr << "Determining next S-pair" << std::endl;

    assert(mSPairGroupSize >= 1);
    std::vector<std::pair<size_t, size_t> > spairGroup;
    exponent w = 0;
    for (unsigned int i = 0; i < mSPairGroupSize; ++i) {
      auto p = mSPairs.pop(w);
      if (p.first == static_cast<size_t>(-1)) {
        assert(p.second == static_cast<size_t>(-1));
        break; // no more S-pairs
      }
      assert(p.first != static_cast<size_t>(-1));
      assert(p.second != static_cast<size_t>(-1));
      assert(!mBasis.retired(p.first));
      assert(!mBasis.retired(p.second));
    
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
    assert(mUseAutoTailReduction);

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
  }

  void ClassicGBAlg::printMemoryUse(std::ostream& out) const
  {
    // TODO: bring over from mathicgb or rewrite.
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

} // namespace mgb
