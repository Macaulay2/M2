// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "SigSPairQueue.hpp"

#include "SigPolyBasis.hpp"

MATHICGB_NAMESPACE_BEGIN

SigSPairQueue::~SigSPairQueue() {}

namespace {
  class Comparer {
  public:
    typedef PolyRing::Monoid Monoid;

    Comparer(const Monoid& monoid): mMonoid(monoid) {}
    bool operator()(const PreSPair& a, const PreSPair& b) const {
      return mMonoid.lessThan(*a.signature, *b.signature);
    }
  private:
    const Monoid& mMonoid;
  };

  // Iterator that accesses the field i based on a passed-in iterator.
  template<class PairIterator>
  class IndexIterator {
  public:
    
    typedef typename PairIterator::iterator_category iterator_category;
    typedef decltype(reinterpret_cast<typename PairIterator::value_type*>(0)->i) value_type;
    typedef typename PairIterator::difference_type difference_type;
    typedef value_type* pointer;
    typedef value_type& reference;
    
    IndexIterator(PairIterator pairIterator): mIterator(pairIterator) {}
    IndexIterator& operator++() {++mIterator; return *this;}
    const value_type operator*() const {return mIterator->i;}
    difference_type operator-(const IndexIterator<PairIterator>& it) const {
      return mIterator - it.mIterator;
    }
    bool operator==(const IndexIterator<PairIterator>& it) const {
      return mIterator == it.mIterator;
    }
    bool operator!=(const IndexIterator<PairIterator>& it) const {
      return mIterator != it.mIterator;
    }
    
  private:
    PairIterator mIterator;
  };
}


class ConcreteSigSPairQueue : public SigSPairQueue {
public:
  ConcreteSigSPairQueue(SigPolyBasis const& basis):
  mPairQueue(Configuration(basis)) {}

  virtual Mono popSignature(Pairs& pairs) {
    pairs.clear();
    if (mPairQueue.empty())
      return 0;
    auto sig = monoid().alloc();
    monoid().copy(*mPairQueue.topPairData(), *sig);
    do {
      pairs.push_back(mPairQueue.topPair());
      mPairQueue.pop();
    } while (
      !mPairQueue.empty() &&
      monoid().equal(*mPairQueue.topPairData(), *sig)
    );
    return sig;
  }

  virtual void pushPairs(size_t pairWith, IndexSigs& pairs) {
#ifdef MATHICGB_DEBUG
    {
      auto tmp = monoid().alloc();
      for (size_t i = 0; i < pairs.size(); ++i) {
        MATHICGB_ASSERT(pairs[i].i < columnCount());
        mPairQueue.configuration().computePairData
          (columnCount(), pairs[i].i, tmp.ptr());
        MATHICGB_ASSERT(monoid().equal(*tmp, *pairs[i].signature));
      }
    }
#endif

    if (columnCount() >= std::numeric_limits<BigIndex>::max())
      throw std::overflow_error
        ("Too large basis element index in constructing S-pairs.");
    
    // sort and insert new column
    Comparer cmp(ring().monoid());
    std::sort(pairs.begin(), pairs.end(), cmp);
    typedef IndexIterator<std::vector<PreSPair>::const_iterator> Iter;
    mPairQueue.addColumnDescending(Iter(pairs.begin()), Iter(pairs.end()));
    
    // free signatures
    for (auto& spair : pairs)
      monoid().freeRaw(spair.signature);
    pairs.clear();
  }
  
  virtual std::string name() const {return "todo";}

  virtual size_t memoryUse() const {return mPairQueue.getMemoryUse();}

  virtual size_t pairCount() const {return mPairQueue.pairCount();}

  virtual size_t columnCount() const {return mPairQueue.columnCount();}

private:
  ConcreteSigSPairQueue(const ConcreteSigSPairQueue&); // not available
  void operator=(const ConcreteSigSPairQueue&); // not available

  // Configuration of mathic::PairTriangle for use with signature queues.
  class Configuration {
  public:
    Configuration(SigPolyBasis const& basis): mBasis(basis) {}

    typedef MonoPtr PairData;
    void computePairData(size_t col, size_t row, PairData sig) {
      MATHICGB_ASSERT(mBasis.ratioCompare(col, row) != EQ);
      // ensure that ratio(col) > ratio(row)
      if (mBasis.ratioCompare(col, row) == LT)
        std::swap(col, row);
      monoid().colonMultiply(
        mBasis.leadMono(col),
        mBasis.leadMono(row),
        mBasis.signature(col),
        *sig
      );
    }

    typedef bool CompareResult;
    bool compare(int colA, int rowA, PairData a,
                 int colB, int rowB, PairData b) const {
      return mBasis.ring().monoid().lessThan(*b, *a);
    }
    bool cmpLessThan(bool v) const {return v;}

    SigPolyBasis const& basis() const {return mBasis;}
    const PolyRing& ring() const {return basis().ring();}
    const Monoid& monoid() const {return ring().monoid();}

  private:
    SigPolyBasis const& mBasis;
  };

  // the compiler should be able to resolve these accessors into a direct
  // offset as though these were member variables.
  const SigPolyBasis& basis() const {
    return mPairQueue.configuration().basis();
  }

  const PolyRing& ring() const {return basis().ring();}
  const Monoid& monoid() const {return ring().monoid();}

  mathic::PairQueue<Configuration> mPairQueue;
  friend struct
  mathic::PairQueueNamespace::ConstructPairDataFunction<Configuration>;
  friend struct
  mathic::PairQueueNamespace::DestructPairDataFunction<Configuration>;
};

std::unique_ptr<SigSPairQueue> SigSPairQueue::create(
  SigPolyBasis const& basis
) {
  return make_unique<ConcreteSigSPairQueue>(basis);
}

MATHICGB_NAMESPACE_END

namespace mathic {
  namespace PairQueueNamespace {
    template<>
    struct ConstructPairDataFunction
      <mgb::ConcreteSigSPairQueue::Configuration>
    {
      inline static void function(
        void* memory,
        Index col,
        Index row,
        mgb::ConcreteSigSPairQueue::Configuration& conf
      ) {
        MATHICGB_ASSERT(memory != 0);
        MATHICGB_ASSERT(col > row);
        auto pd = new (memory)
          mgb::ConcreteSigSPairQueue::Configuration::PairData
          (conf.basis().ring().allocMonomial());
        conf.computePairData(col, row, *pd);
      }
    };

    template<>
    struct DestructPairDataFunction
      <mgb::ConcreteSigSPairQueue::Configuration>
    {
      inline static void function(
        mgb::ConcreteSigSPairQueue::Configuration::PairData* pd,
        Index col,
        Index row,
        mgb::ConcreteSigSPairQueue::Configuration& conf
      ) {
        MATHICGB_ASSERT(pd != nullptr);
        MATHICGB_ASSERT(col > row);
        conf.monoid().freeRaw(*pd);
      }
    };
  }
}
