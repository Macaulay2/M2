// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_MONOMIAL_MAP_GUARD
#define MATHICGB_MONOMIAL_MAP_GUARD

#include "FixedSizeMonomialMap.h"
#include "mtbb.hpp"
#include "Atomic.hpp"
#include "PolyRing.hpp"
#include "memtailor/memtailor.h"
#include <limits>
#include <vector>
#include <algorithm>

MATHICGB_NAMESPACE_BEGIN

/// A concurrent hash map from monomials to T. This map can resize itself
/// if there are too few buckets compared to entries.
///
/// Queries are supported through a MonomialMap::Reader object. On resize all
/// previous readers are subject to permanent spurious misses -
/// querying clients need to grab a fresh reader to confirm misses. Grabbing
/// a reader incurs synchronization so do not do it for every query.
///
/// External synchronization with writers is required if spurious misses are
/// not acceptable. There are no spurious hits. If misses are very rare then
/// reads can be done with minimal overhead by following this pattern:
///
///  1) grab a reader X
///  2) perform queries on X until done or there is a miss
///  3) replace X with a fresh reader
///  4) go to 2 if the miss is now a hit
///  5) grab a lock shared with all writers
///  6) perform the query again - a miss is now guaranteed to be accurate
///  7) release the lock after the processing of the miss is done and go to 2
///
/// There is no way to avoid locking if spurious misses are not acceptable
/// as otherwise a writer could make an insertion of the requested key at any
/// time while processing the miss - which then makes the miss spurious
/// after-the-fact.
template<class T>
class MonomialMap {
public:
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::Mono Mono;
  typedef Monoid::MonoRef MonoRef;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::MonoPtr MonoPtr;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;

  typedef T mapped_type;
  typedef FixedSizeMonomialMap<mapped_type> FixedSizeMap;
  typedef typename FixedSizeMap::value_type value_type;

  MonomialMap(const PolyRing& ring):
    mMap(new FixedSizeMap(InitialBucketCount, ring)),
    mRing(ring),
    mCapacityUntilGrowth
    (maxEntries(mMap.load(std::memory_order_relaxed)->bucketCount()))
  {
    // We can load mMap as std::memory_order_relaxed because we just stored it
    // and the constructor cannot run concurrently.
  }

  ~MonomialMap() {
    // We can load with std::memory_order_relaxed because the destructor
    // cannot run concurrently.
    delete mMap.load(std::memory_order_relaxed);
  }

  const PolyRing& ring() const {return mRing;}

  /// All queries are performed through a Reader. Readers are subject to
  /// permanent spurious misses on hash map resize. Grab a fresh reader
  /// on misses to confirm them. Making a Reader imposes synchronization
  /// overhead. External locking is required to guarantee that a miss is
  /// genuine since there is no mutual exclusion between queries and
  /// insertions.
  ///
  /// It is intentional that a reader does not have an update() method. The
  /// purpose of this is to make the internal hash table pointer const inside
  /// the class which guarantees to the compiler that it will never change.
  /// This in turn allows the compiler to store the fields of the internal
  /// hash table pointer such as the hash map mask. If there were an update()
  /// method this would not be possible and the compiler might have to load
  /// that mask for every query. I made it this way because I was seeming
  /// a performance regression of several percent which then went away with
  /// this solution. (well actually it's a reference, which has the same
  /// effect).
  class Reader {
  public:
    Reader(const MonomialMap<T>& map):
      mMap(*map.mMap.load(std::memory_order_seq_cst))
    {
      // We grab the hash table pointer with std::memory_order_seq_cst in order
      // to force a CPU cache flush - in this way we are more likely to get an
      // up to date value.
    }

    /// Returns the value that mono maps to or null if no such key has been
    /// inserted. Also returns the internal monomial that matches mono if such
    /// a monomial exists. Misses can be spurious! Read the comments on the parent
    /// class.
    std::pair<const mapped_type*, ConstMonoPtr>
    find(ConstMonoRef mono) const {
      return mMap.find(mono);
    }

    // As find but looks for the product of a and b and also returns the
    // monomal that is the product.
    std::pair<const mapped_type*, ConstMonoPtr> findProduct(
      ConstMonoRef a,
      ConstMonoRef b
    ) const {
      return mMap.findProduct(a, b);
    }

    /// As findProduct() but looks for the two products a1*b and a2*b
    /// simultaneously. The purpose of this is similar to that of unrolling a
    /// loop.
    MATHICGB_INLINE
    std::pair<const mapped_type*, const mapped_type*> findTwoProducts(
      const ConstMonoRef a1,
      const ConstMonoRef a2,
      const ConstMonoRef b
    ) const {
      return mMap.findTwoProducts(a1, a2, b);
    }

    typedef typename FixedSizeMonomialMap<T>::const_iterator const_iterator;

    /// The range [begin(), end()) contains all entries in the hash table.
    /// Insertions invalidate all iterators. Beware that insertions can
    /// happen concurrently.
    const_iterator begin() const {return mMap.begin();}
    const_iterator end() const {return mMap.end();}

  private:
    const FixedSizeMonomialMap<T>& mMap;
  };

  /// Removes all entries from the hash table. This requires mutual exclusion
  /// from and synchronization with all readers and writers.
  void clearNonConcurrent() {
    // We can load with std::memory_order_relaxed because this method
    // requires external synchronization.
    mMap.load(std::memory_order_relaxed)->clearNonConcurrent();
  }

  /// Makes value.first map to value.second unless value.first is already
  /// present in the map - in that case nothing is done. If p is the returned
  /// pair then *p.first.first is the value that value.first maps to after the insert
  /// and p.second is true if an insertion was performed. *p.first.first will not
  /// equal value.second if an insertion was not performed - unless the
  /// inserted value equals the already present value. p.first.second is an
  /// internal monomial that equals value.first.
  std::pair<std::pair<const mapped_type*, ConstMonoPtr>, bool>
  insert(const value_type& value) {
    const mgb::mtbb::mutex::scoped_lock lockGuard(mInsertionMutex);

    // We can load mMap as std::memory_order_relaxed because we have already
    // synchronized with all other mutators by locking mInsertionMutex;
    auto map = mMap.load(std::memory_order_relaxed);

    // this is a loop since it is possible to set the growth factor and
    // the initial size so low that several rounds are required. This should
    // only happen when debugging as otherwise such low parameters are
    // not a good idea.
    while (mCapacityUntilGrowth == 0) {
      // Resize the table by making a bigger one and using that instead.
      if (map->bucketCount() > // check overflow
        std::numeric_limits<size_t>::max() / GrowthFactor)
      {
        throw std::bad_alloc();
      }
      const size_t newBucketCount = map->bucketCount() * GrowthFactor;
      auto nextMap =
        make_unique<FixedSizeMap>(newBucketCount, std::move(*map));
      mOldMaps.emplace_back(map);
      mCapacityUntilGrowth =
        maxEntries(nextMap->bucketCount()) - maxEntries(map->bucketCount());

      // Store with std::memory_order_seq_cst to force a memory flush so that
      // readers see the new table as soon as possible.
      map = nextMap.release();
      mMap.store(map, std::memory_order_seq_cst);
    }
    MATHICGB_ASSERT(mCapacityUntilGrowth > 0);

    auto p = map->insert(value);
    if (p.second)
      --mCapacityUntilGrowth;
    return p;
  }

  /// Return the number of entries. This method uses internal synchronization
  /// so do not call too much or you'll get degraded performance.
  size_t entryCount() const {
    const mgb::mtbb::mutex::scoped_lock lockGuard(mInsertionMutex);
    // We can load with std::memory_order_relaxed because we are holding the
    // lock.
    auto& map = *mMap.load(std::memory_order_relaxed);
    return maxEntries(map.bucketCount()) - mCapacityUntilGrowth;
  }

private:
  static const size_t MinBucketsPerEntry = 3; // inverse of max load factor
  static const size_t GrowthFactor = 2;
  static const size_t InitialBucketCount = 1 << 1;

  static size_t maxEntries(const size_t bucketCount) {
    return (bucketCount + (MinBucketsPerEntry - 1)) / MinBucketsPerEntry;
  }

  Atomic<FixedSizeMap*> mMap;
  const PolyRing& mRing;
  mutable mgb::mtbb::mutex mInsertionMutex;

  /// Only access this field while holding the mInsertionMutex lock.
  size_t mCapacityUntilGrowth;

  /// Only access this field while holding the mInsertionMutex lock.
  /// Contains the old hash tables that we discarded on resize. We have to
  /// keep these around as we have no way to determine if there are still
  /// readers looking at them. This could be changed at the cost of
  /// more overhead in the Reader constructor and destructor.
  std::vector<std::unique_ptr<FixedSizeMap>> mOldMaps;
};

MATHICGB_NAMESPACE_END
#endif
