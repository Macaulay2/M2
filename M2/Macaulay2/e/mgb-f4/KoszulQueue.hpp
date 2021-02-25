// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_KOSZUL_QUEUE_GUARD
#define MATHICGB_KOSZUL_QUEUE_GUARD

#include "PolyRing.hpp"
#include "NonCopyable.hpp"
#include "mathic/mathic.h"

MATHICGB_NAMESPACE_BEGIN

/// Used to keep track of pending Koszul syzygy signatures in the signature
/// basis algorithm.
class KoszulQueue : public NonCopyable<KoszulQueue> {
public:
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::Mono Mono;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::MonoPool MonoPool;

  KoszulQueue(const Monoid& monoid):
    mPool(monoid),
    mQueue(Configuration(monoid, mPool))
  {}

  KoszulQueue(KoszulQueue&& kq):
    mPool(std::move(kq.mPool)),
    mQueue(std::move(kq.mQueue))
  {}

  ConstMonoRef top() const {
    MATHICGB_ASSERT(!empty());
    return *mQueue.top();
  }

  void pop() {
    MATHICGB_ASSERT(!empty());
    mPool.freeRaw(*mQueue.pop());
  }

  void push(ConstMonoRef sig) {
    auto m = mPool.alloc();
    monoid().copy(sig, m);
    mQueue.push(m.release());
  }
  bool empty() const {return mQueue.empty();}
  size_t size() const {return mQueue.size();}

  size_t getMemoryUse() const {return mQueue.getMemoryUse();}

  const Monoid& monoid() const {return mQueue.getConfiguration().monoid();}

private:
  class Configuration {
  public:
    typedef Monoid::MonoPtr Entry;

    Configuration(const Monoid& monoid, MonoPool& pool):
      mMonoid(monoid), mPool(pool) {}

    typedef Monoid::CompareResult CompareResult;
    CompareResult compare(const Entry& a, const Entry& b) const {
      return monoid().compare(*a, *b);
    }
    bool cmpLessThan(CompareResult r) const {return r == Monoid::GreaterThan;}

    static const bool fastIndex = false;
    static const bool supportDeduplication = true;
    bool cmpEqual(CompareResult r) const {return r == Monoid::EqualTo;}

    Entry deduplicate(Entry a, Entry b) {
      mPool.freeRaw(*b);
      return a;
    }

    const Monoid& monoid() const {return mMonoid;}

  private:
    const Monoid& mMonoid;
    MonoPool& mPool;
  };

  MonoPool mPool;
  mic::Heap<Configuration> mQueue;
};

MATHICGB_NAMESPACE_END
#endif
